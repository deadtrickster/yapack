(in-package #:yapack)

(deftype octet ()
  '(unsigned-byte 8))

(deftype octets ()
  '(vector octet))

(defparameter *endianness* :big-endian)

(defparameter *base-types*
  '(((unsigned-byte  8) :byte      :uchar  :uint8  :unsigned-char :octet)
    ((signed-byte    8)            :char   :int8)
    ((unsigned-byte 16) :uhalfword :ushort :uint16 :unsigned-short)
    ((signed-byte   16) :halfword  :short  :int16)
    ((unsigned-byte 32) :uword     :uint   :uint32 :unsigned-int)
    ((signed-byte   32) :word      :int    :int32)
    ((unsigned-byte 64) :udword    :ulong  :uint64 :unsigned-long)
    ((signed-byte   64) :dword     :long   :int64)))

(defvar *writers* nil)
(defvar *readers* nil)

(defun size-of (tag)
  "Size in bytes."
  (let ((type (rassoc tag *base-types* :test #'member)))
    (floor (cadar type) 8)))

(defun write* (stream tag value &rest args)
  (if-let ((writer (assoc tag *writers* :test #'eq)))
    (apply (cdr writer) stream value args)
    (apply 'write-sequence* stream tag value args)))

(defun read* (stream tag &rest args)
  (if-let ((reader (assoc tag *readers* :test #'eq)))
    (apply (cdr reader) stream args)
    (apply 'read-sequence* stream tag args)))

(defun define-writer (tag fn)
  (setf *writers*
        (cons (cons tag fn)
              (delete tag *writers* :key #'car :test #'eq))))

(defun define-reader (tag fn)
  (setf *readers*
        (cons (cons tag fn)
              (delete tag *readers* :key #'car :test #'eq))))

(defmacro defwriter (name tag (&rest args) &body body)
  (if name
      `(progn
         (defun ,name (,@args)
           ,@body)
         (define-writer ,tag #',name))
      `(define-writer ,tag (lambda (,@args) ,@body))))

(defmacro defreader (name tag (&rest args) &body body)
  (if name
      `(progn
         (defun ,name (,@args)
           ,@body)
         (define-reader ,tag #',name))
      `(define-reader ,tag (lambda (,@args) ,@body))))

(defwriter write-pad :pad (stream value)
  (declare (ignore value))
  (write-byte 0 stream))

(defreader read-pad :pad (stream)
  (and (read-byte stream nil) 0))

(defun write-sequence* (stream tag value)
  (destructuring-bind (encoding length-tag)
      tag
    (if (assoc encoding *writers* :test #'eq)
        (progn
          (when (assoc length-tag *writers* :test #'eq)
            (write* stream length-tag (length value)))
          (if (member encoding '(:byte :octet :uchar :uint8 :unsigned-char))
              (when (plusp (length value))
                (write-sequence value stream))
              (loop for obj across value do (write* stream encoding obj))))
        (let ((octets
               (if (eq encoding :string)
                   (babel:string-to-octets value)
                   (babel:string-to-octets value :encoding encoding))))
          (when (assoc length-tag *writers* :test #'eq)
            (write* stream length-tag (length octets)))
          (when (plusp (length octets))
            (write-sequence octets stream))))))

(defun read-sequence* (stream tag)
  (destructuring-bind (encoding length-tag)
      tag
    (let ((length
           (cond
             ((numberp length-tag)
              length-tag)
             ((assoc length-tag *readers* :test #'eq)
              (read* stream length-tag))
             (t
              nil))))
      (flet ((slurp ()
               (if length
                   (let ((octets (make-array length :element-type 'octet)))
                     (when (plusp length)
                       (read-sequence octets stream))
                     octets)
                   (coerce (loop for byte = (read-byte stream nil) while byte collect byte)
                           'octets))))
        (if (assoc encoding *readers* :test #'eq)
            (if (member encoding '(:byte :octet :uchar :uint8 :unsigned-char))
                (slurp)
                (coerce (if length
                            (loop repeat length collect (read* stream encoding))
                            (loop for obj = (read* stream encoding) while obj collect obj))
                        'vector))
            (let ((octets (slurp)))
              (if (eq encoding :string)
                  (babel:octets-to-string octets)
                  (babel:octets-to-string octets :encoding encoding))))))))

(defun write-big-endian (stream value bits)
  (loop
     for pos from (- bits 8) downto 0 by 8
     do (write-byte (ldb (byte 8 pos) value) stream)))

(defun write-little-endian (stream value bits)
  (loop
     for pos from 0 below bits by 8
     do (write-byte (ldb (byte 8 pos) value) stream)))

(defun write-word (stream value bits)
  (funcall (if (eq *endianness* :big-endian)
               #'write-big-endian
               #'write-little-endian)
           stream
           (if (characterp value) (char-code value) value)
           bits))

(defmacro read-word (stream (&whole type signed bits))
  (declare (ignore signed))
  (once-only (stream)
    (with-unique-names (value pos)
      `(let ((,value 0))
         (declare (,type ,value))
         (if (eq *endianness* :big-endian)
             (loop
                for ,pos from (- ,bits 8) downto 0 by 8
                do (setf (ldb (byte 8 ,pos) ,value) (read-byte ,stream)))
             (loop
                for ,pos from 0 below ,bits by 8
                do (setf (ldb (byte 8 ,pos) ,value) (read-byte ,stream))))
         ,value))))

(defwriter write-float :float (stream value)
  (let ((bits (ieee-floats:encode-float32 value)))
    (write-word stream bits 32)))

(defreader read-float :float (stream)
  (let ((bits (read-word stream (unsigned-byte 32))))
    (ieee-floats:decode-float32 bits)))

(defwriter write-double :double (stream value)
  (let ((bits (ieee-floats:encode-float64 value)))
    (write-word stream bits 64)))

(defreader read-double :double (stream)
  (let ((bits (read-word stream (unsigned-byte 64))))
    (ieee-floats:decode-float64 bits)))

;; Intel 8087
(ieee-floats:make-float-converters encode-float80 decode-float80 15 64 nil)

(defwriter write-long-double :long-double (stream value)
  (let ((bits (encode-float80 value)))
    (write-word stream bits 80)))

(defreader read-long-double :long-double (stream)
  (let ((bits (read-word stream (unsigned-byte 80))))
    (decode-float80 bits)))

(defmacro word (tag (&whole type signed bits))
  (declare (ignore signed))
  (with-unique-names (value stream)
    `(progn
       (defwriter nil ,tag (,stream ,value) (write-word ,stream ,value ,bits))
       (defreader nil ,tag (,stream) (read-word ,stream ,type)))))

(defmacro words ()
  `(progn
     ,@(mapcan (lambda (spec)
                 (destructuring-bind (type &rest tags)
                     spec
                   (mapcar (lambda (tag)
                             `(word ,tag ,type))
                           tags)))
               *base-types*)))

(defun pack (stream &rest args)
  (do ()
      ((null args))
    (let ((tag (pop args)))
      (if (eq tag :pad)
          (write* stream :pad nil)
          (let ((value (pop args)))
            (write* stream tag value))))))

(defmacro packing ((stream) &body body)
  `(babel-streams:with-output-to-sequence (,stream)
     ,@body))

(defun pack-bytes (&rest args)
  (packing (stream)
    (apply #'pack stream args)))

(defun unpack (stream &rest tags)
  (loop
     for tag in tags
     for value = (read* stream tag)
     while value
     unless (or (eq tag :pad)
                (and (listp tag)
                     (eq (first tag) :pad)))
     collect value into values
     finally (return
               (if (null (rest values))
                   (first values)
                   values))))

(defmacro unpacking ((stream bytes) &body body)
  `(babel-streams:with-input-from-sequence (,stream ,bytes)
     ,@body))

(defun unpack-bytes (bytes &rest args)
  (unpacking (stream bytes)
    (apply #'unpack stream args)))

;; TODO: Should be able to automagically add type declarations.

;; (with-unpack (s)
;;     ((this :byte)
;;      (that :byte)
;;      (the-other :byte)
;;      :pad
;;      (a-string '(:short :utf-8)))
;;   (do-stuff))
(defmacro with-unpack ((stream) (&rest vars) &body body)
  (once-only (stream)
    `(let ,(mapcar #'first (remove :pad vars :test #'eq))
       ,@(mapcar (lambda (var)
                   (if (eq var :pad)
                       `(unpack ,stream :pad)
                       (destructuring-bind (name tag)
                           var
                         `(setf ,name (unpack ,stream ,tag)))))
                 vars)
       (locally
           ,@body))))

(defmacro with-unpack-bytes ((bytes) (&rest vars) &body body)
  (with-unique-names (stream)
    `(unpacking (,stream ,bytes)
       (with-unpack (,stream)
           (,@vars)
         ,@body))))
