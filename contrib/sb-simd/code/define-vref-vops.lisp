(in-package #:sb-vm)

;;; Both load- and store VOPs are augmented with an auxiliary last argument
;;; that is a constant addend for the address calculation.  This addend is
;;; zero by default, but we can sometimes transform the code for the index
;;; calculation such that we have a nonzero addend.  We also generate two
;;; variants of the VOP - one for the general case, and one for the case
;;; where the index is a compile-time constant.

#+(or x86 x86-64)
(macrolet
    ((define-vref-vop (vref-record-name)
       (with-accessors ((name sb-simd-internals:vref-record-name)
                        (vop sb-simd-internals:vref-record-vop)
                        (vop-c sb-simd-internals:vref-record-vop-c)
                        (mnemonic sb-simd-internals:vref-record-mnemonic)
                        (value-record sb-simd-internals:vref-record-value-record)
                        (vector-record sb-simd-internals:vref-record-vector-record)
                        (store sb-simd-internals:store-record-p))
           (sb-simd-internals:find-function-record vref-record-name)
         (assert (= (length value-record) 1))
         (let* ((vector-type (sb-simd-internals:value-record-type vector-record))
                (vector-primitive-type (sb-simd-internals:value-record-primitive-type vector-record))
                (value-scs (sb-simd-internals:value-record-scs (first value-record)))
                (value-type (sb-simd-internals:value-record-type (first value-record)))
                (value-primitive-type (sb-simd-internals:value-record-primitive-type (first value-record)))
                (scalar-record
                  (etypecase (first value-record)
                    (sb-simd-internals:simd-record (sb-simd-internals:simd-record-scalar-record (first value-record)))
                    (sb-simd-internals:value-record (first value-record))))
                (bits-per-element (sb-simd-internals:value-record-bits scalar-record))
                (bytes-per-element (ceiling bits-per-element 8))
                (displacement
                  (multiple-value-bind (lo hi)
                      (displacement-bounds other-pointer-lowtag bits-per-element vector-data-offset)
                    `(integer ,lo ,hi))))
           (multiple-value-bind (index-scs scale)
               (if (>= bytes-per-element (ash 1 n-fixnum-tag-bits))
                   (values '(any-reg signed-reg unsigned-reg) `(index-scale ,bytes-per-element index))
                   (values '(signed-reg unsigned-reg) bytes-per-element))
             `(progn
                (defknown ,vop (,@(when store `(,value-type)) ,vector-type index ,displacement)
                    (values ,value-type &optional)
                    (always-translatable)
                  :overwrite-fndb-silently t)
                (define-vop (,vop)
                  (:translate ,vop)
                  (:policy :fast-safe)
                  (:args
                   ,@(when store `((value :scs ,value-scs :target result)))
                   (vector :scs (descriptor-reg))
                   (index :scs ,index-scs))
                  (:info addend)
                  (:arg-types
                   ,@(when store `(,value-primitive-type))
                   ,vector-primitive-type
                   positive-fixnum
                   (:constant ,displacement))
                  (:results (result :scs ,value-scs))
                  (:result-types ,value-primitive-type)
                  (:generator
                   2
                   ,@(let ((ea `(ea (+ (* vector-data-offset n-word-bytes)
                                       (* addend ,bytes-per-element)
                                       (- other-pointer-lowtag))
                                    vector index ,scale)))
                       (if store
                           `((inst ,mnemonic ,ea value)
                             (move result value))
                           `((inst ,mnemonic result ,ea))))))
                (define-vop (,vop-c)
                  (:translate ,vop)
                  (:policy :fast-safe)
                  (:args ,@(when store `((value :scs ,value-scs :target result)))
                         (vector :scs (descriptor-reg)))
                  (:info index addend)
                  (:arg-types ,@(when store `(,value-primitive-type))
                              ,vector-primitive-type
                              (:constant low-index)
                              (:constant ,displacement))
                  (:results (result :scs ,value-scs))
                  (:result-types ,value-primitive-type)
                  (:generator
                   1
                   ,@(let ((ea `(ea (+ (* vector-data-offset n-word-bytes)
                                       (* ,bytes-per-element (+ index addend))
                                       (- other-pointer-lowtag))
                                    vector)))
                       (if store
                           `((inst ,mnemonic ,ea value)
                             (move result value))
                           `((inst ,mnemonic result ,ea)))))))))))
     (define-vref-vops ()
       `(progn
          ,@(loop for vref-record
                    in (sb-simd-internals:filter-available-function-records
                        #'sb-simd-internals:vref-record-p)
                  collect `(define-vref-vop ,(sb-simd-internals:vref-record-name vref-record))))))
  (define-vref-vops))

#+arm64
(macrolet
    ((define-vref-vop (vref-record-name)
       (with-accessors ((name sb-simd-internals:vref-record-name)
                        (vop sb-simd-internals:vref-record-vop)
                        (vop-c sb-simd-internals:vref-record-vop-c)
                        (mnemonic sb-simd-internals:vref-record-mnemonic)
                        (value-record sb-simd-internals:vref-record-value-record)
                        (vector-record sb-simd-internals:vref-record-vector-record)
                        (store sb-simd-internals:store-record-p))
           (sb-simd-internals:find-function-record vref-record-name)
         (let* ((vector-type (sb-simd-internals:value-record-type vector-record))
                (vector-primitive-type (sb-simd-internals:value-record-primitive-type vector-record))
                (value-scs (mapcar #'sb-simd-internals:value-record-scs value-record))
                (value-type (mapcar #'sb-simd-internals:value-record-type value-record))
                (value-primitive-type (mapcar #'sb-simd-internals:value-record-primitive-type value-record))
                (scalar-record
                  (etypecase (first value-record)
                    (sb-simd-internals:simd-record (sb-simd-internals:simd-record-scalar-record (first value-record)))
                    (sb-simd-internals:value-record (first value-record))))
                (bits-per-element (sb-simd-internals:value-record-bits scalar-record))
                (bytes-per-element (ceiling bits-per-element 8))
                (shift (1- (integer-length bytes-per-element)))
                (value-names (loop for x in value-record collect (gensym "VALUE")))
                (temporary-names (loop for x in value-record collect (gensym "TEMP"))))
           `(progn
              (defknown ,vop (,@(when store value-type) ,vector-type index (integer 0 0))
                  (values ,@value-type &optional)
                  (always-translatable)
                :overwrite-fndb-silently t)
              (define-vop (,vop)
                (:translate ,vop)
                (:policy :fast-safe)
                (:args ,@(when store (loop for scs in value-scs
                                           for name in value-names
                                           for temp in temporary-names
                                           collect `(,name :scs (,@scs zero)
                                                           ,@(when (rest value-record)
                                                               `(:target ,temp
                                                                 :to :save)))))
                       (object :scs (descriptor-reg))
                       (index :scs (any-reg unsigned-reg signed-reg immediate)))
                (:arg-types ,@(when store value-primitive-type)
                            ,vector-primitive-type
                            tagged-num
                            (:constant (integer 0 0)))
                (:info addend) ; always zero
                ,@(unless store
                    `((:results ,@(loop for scs in value-scs
                                        for name in value-names
                                        collect `(,name :scs ,scs)))
                      (:result-types ,@value-primitive-type)))
                ;; For instructions like ld1/st1/etc, we need to allocate sequential
                ;; registers, this isn't supported by Python, so we fix which registers
                ;; get used and just deal with things getting moved about.
                ,@(when (rest value-record)
                    (loop for offset from 0
                          for temp in temporary-names
                          for scs in value-scs
                          for value in value-names
                          collect `(:temporary
                                    (:sc ,(first scs)
                                     :offset ,offset
                                     ,@(unless store
                                         `(:target ,value)))
                                    ,temp)))
                (:ignore addend)
                (:generator 2
                  ,@(when (and store (rest value-record))
                      (loop for temp in temporary-names
                            for value in value-names
                            collect `(unless (location= ,temp ,value)
                                       (inst mov ,temp ,value :16b))))
                  ;; Unfortunately ld1/etc don't support an immediate offset, so
                  ;; we need to do the full address calculation.
                  ,(if (rest value-record)
                       `(progn
                           (let ((shift ,shift))
                             (when (sc-is index any-reg)
                               (decf shift n-fixnum-tag-bits))
                             (inst add tmp-tn object (if (minusp shift)
                                                         (asr index (- shift))
                                                         (lsl index shift))))
                           (inst add tmp-tn tmp-tn (- (ash vector-data-offset word-shift)
                                                      other-pointer-lowtag))
                           (inst ,mnemonic
                                 (list ,@temporary-names)
                                 (@ tmp-tn)
                                 ,(ecase (second (first value-type))
                                    ((sb-simd:u8 sb-simd:s8) :16b)
                                    ((sb-simd:u16 sb-simd:s16) :8h)
                                    ((sb-simd:u32 sb-simd:s32 sb-simd:f32) :4s)
                                    ((sb-simd:u64 sb-simd:s64 sb-simd:f64) :2d))))
                       `(sc-case index
                          (immediate
                           (inst ,mnemonic
                                 ,@value-names
                                 (@ object (load-store-offset
                                            (+ (ash (tn-value index) ,shift)
                                               (- (ash vector-data-offset word-shift)
                                                  other-pointer-lowtag))))))
                          (t
                           (let ((shift ,shift))
                             (when (sc-is index any-reg)
                               (decf shift n-fixnum-tag-bits))
                             (inst add tmp-tn object (if (minusp shift)
                                                         (asr index (- shift))
                                                         (lsl index shift))))
                           (inst ,mnemonic
                                 ,@value-names
                                 (@ tmp-tn (load-store-offset (- (ash vector-data-offset word-shift)
                                                                 other-pointer-lowtag)))))))
                  ,@(when (and (not store) (rest value-record))
                      (loop for temp in temporary-names
                            for value in value-names
                            collect `(unless (location= ,temp ,value)
                                       (inst mov ,value ,temp :16b))))))))))
     (define-vref-vops ()
       `(progn
          ,@(loop for vref-record
                    in (sb-simd-internals:filter-available-function-records
                        #'sb-simd-internals:vref-record-p)
                  collect `(define-vref-vop ,(sb-simd-internals:vref-record-name vref-record))))))
  (define-vref-vops))
