(in-package #:sb-simd-internals)

(macrolet
    ((define-vref (name kind)
       (with-accessors ((name vref-record-name)
                        (instruction-set vref-record-instruction-set)
                        (value-record vref-record-value-record)
                        (vector-record vref-record-vector-record)
                        (vop vref-record-vop))
           (find-function-record name)
         (let* ((simd-width (* (value-record-simd-width (first value-record))
                               (length value-record)))
                (element-type
                  (second
                   (value-record-type vector-record))))
           (ecase kind
             (:load
              (if (not (instruction-set-available-p instruction-set))
                  `(define-missing-instruction ,name
                     :required-arguments (array index))
                  `(define-inline ,name (array index)
                     (declare (type (array ,element-type) array)
                              (index index))
                     (sb-kernel:check-bound array (array-total-size array) (+ index ,(1- simd-width)))
                     (multiple-value-bind (vector index)
                         (sb-kernel:%data-vector-and-index array index)
                       (declare (type (simple-array ,element-type (*)) vector))
                       (,vop vector index 0)))))
             (:store
              (let ((value-names (if (rest value-record)
                                     (loop for rec in value-record
                                           collect (gensym "VALUE"))
                                     '(value))))
                (if (not (instruction-set-available-p instruction-set))
                    `(define-missing-instruction ,name
                       :required-arguments (,@value-names array index))
                    `(define-inline ,name (,@value-names array index)
                       (declare (type (array ,element-type) array)
                                (index index))
                       (sb-kernel:check-bound array (array-total-size array) (+ index ,(1- simd-width)))
                       (multiple-value-bind (vector index)
                           (sb-kernel:%data-vector-and-index array index)
                         (declare (type (simple-array ,element-type (*)) vector))
                         (,vop ,@(loop for value in value-record
                                       for name in value-names
                                       collect `(,(value-record-name value) ,name))
                               vector index 0)
                         (values ,@value-names))))))))))
     (define-vrefs ()
       `(progn
          ,@(loop for load-record in (filter-function-records #'load-record-p)
                  for name = (load-record-name load-record)
                  collect `(define-vref ,name :load))
          ,@(loop for store-record in (filter-function-records #'store-record-p)
                  for name = (store-record-name store-record)
                  collect `(define-vref ,name :store)))))
  (define-vrefs))
