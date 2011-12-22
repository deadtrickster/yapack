(in-package #:pack-test)

(defun tag-p (arg)
  (or (keywordp arg)
      (and (listp arg)
           (eq (first arg) 'quote))))

(defmacro bounce-test (&rest args)
  (let ((tags (remove-if-not #'tag-p args))
        (vals (remove-if     #'tag-p args)))
    (if (null (rest vals))
        `(assert-equalp ',@vals (unpack-bytes (pack-bytes ,@args) ,@tags))
        `(assert-equalp ',vals  (unpack-bytes (pack-bytes ,@args) ,@tags)))))

(define-test bounce
  (bounce-test :uchar 1)
  (bounce-test :ushort 256)
  (bounce-test :float 1.0)
  (bounce-test '(:utf-8 :ushort) "string"))

(define-test readme
  (assert-equalp #(0 1 2 3) (pack-bytes :ushort 1 :uchar 2 :uchar 3))
  (assert-equalp #(1 0 0 2) (pack-bytes :uchar 1 :pad :ushort 2))
  (assert-equalp #(0 13 104 101 108 108 111 44 32 119 111 114 108 100 33)
                 (pack-bytes '(:utf-8 :ushort) "hello, world!")))
