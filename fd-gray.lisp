;;
;;  fd-gray  -  Unix file descriptor gray streams for Common Lisp
;;
;;  Copyright 2017 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :fd-gray)

(deftype octet ()
  `(unsigned-byte 8))

(deftype fixnum+ (&optional (start 0))
  `(integer ,start ,most-positive-fixnum))

(defclass stream (trivial-gray-stream-mixin)
  ((fd :initarg :fd
       :reader stream-fd
       :type file-descriptor)
   (blocking :initarg :blocking
             :accessor stream-blocking%
             :type boolean)))

(defgeneric stream-blocking (stream))

(defmethod stream-blocking ((stream stream))
  (or (and (slot-boundp stream 'blocking)
           (slot-value stream 'blocking))
      (setf (slot-value stream 'blocking)
            (let ((flags (fcntl:getfl (stream-fd stream))))
              (not (= 0 (logand fcntl:+o-nonblock+ flags)))))))

(defgeneric (setf stream-blocking) (value stream))

(defmethod (setf stream-blocking) (value (stream stream))
  (let* ((fd (stream-fd stream))
         (flags (fcntl:getfl fd)))
    (cond
      ((and value (not (= 0 (logand fcntl:+o-nonblock+ flags))))
       t)
      ((and (not value) (= 0 (logand fcntl:+o-nonblock+ flags)))
       nil)
      (t
       (fcntl:setfl fd (if value
                           (logand (lognot fcntl:+o-nonblock+) flags)
                           (logior fcntl:+o-nonblock+ flags)))
       (setf (slot-value stream 'blocking) value)))))

(define-condition stream-error (cl:stream-error)
  ()
  (:documentation "Superclass for all errors related to
fd gray streams."))

(define-condition stream-closed-error (stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~S is closed."
                     (stream-error-stream condition))))
  (:documentation "An error that is signalled when someone is trying
to read from or write to a closed fd gray stream."))

(defgeneric check-if-open (stream))

(defmethod check-if-open ((stream stream))
  "Checks if STREAM is open and signals an error otherwise."
  (unless (open-stream-p stream)
    (error 'stream-closed-error
           :stream stream)))

(defmethod stream-element-type ((stream stream))
  'octet)

(defvar *buffer-size* 65536)

(defclass input-stream (stream fundamental-binary-input-stream)
  ((input-buffer :initform (cffi:foreign-alloc :unsigned-char
                                               :count *buffer-size*)
                 :accessor input-buffer)
   (input-index :initform 0
                :accessor input-index
                :type fixnum+)
   (input-length :initform 0
                 :accessor input-length
                 :type fixnum+)
   (input-max :initform *buffer-size*
              :reader input-max
              :type fixnum+)))

(define-condition stream-input-error (stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "input error on ~S"
                     (stream-error-stream condition))))
  (:documentation "An error that is signalled when an input error happens
on a fd gray stream."))

(defgeneric stream-input (stream))

(defmethod stream-input ((stream input-stream))
  "Fill buffer with file data.
Tries to read once from input-length to input-max (end of buffer)."
  (let* ((buffer (input-buffer stream))
         (length (input-length stream))
         (fd (stream-fd stream))
         (buf (cffi:mem-aptr buffer :unsigned-char length))
         (buflen (- (input-max stream) length))
         (r (if (stream-blocking stream)
                (unistd:read fd buf buflen)
                (unistd:read-non-blocking fd buf buflen))))
    (cond ((null r)
           nil)
          ((= r 0)
           :eof)
          ((< r 0)
           (error 'stream-input-error :stream stream))
          (t
           (incf (input-length stream) r)
           r))))

(defmethod stream-read-byte ((stream input-stream))
  (check-if-open stream)
  (let ((buffer (input-buffer stream)))
    (with-accessors ((input-index input-index)) stream
      (when (= input-index (input-length stream))
        (setf input-index 0
              (input-length stream) 0)
        (case (stream-input stream)
          ((:eof) (return-from stream-read-byte :eof))
          ((nil) (return-from stream-read-byte nil))))
      (let ((b (cffi:mem-aref buffer :unsigned-char input-index)))
        (incf input-index)
        b))))

(defmethod stream-read-sequence ((stream input-stream)
                                 seq start end &key &allow-other-keys)
  (check-if-open stream)
  (let ((buffer (input-buffer stream)))
    (with-accessors ((input-index input-index)) stream
      (loop
         (unless (< start end)
           (return))
         (let ((e (min (- end start)
                       (- (input-length stream) input-index))))
           (dotimes (i e)
             (setf (aref seq start)
                   (cffi:mem-aref buffer :unsigned-char input-index))
             (incf start)
             (incf input-index))
           (when (= input-index (input-length stream))
             (setf input-index 0
                   (input-length buffer) 0)
             (case (stream-input stream)
               ((:eof) (return-from stream-read-sequence :eof))
               ((nil) (return-from stream-read-sequence nil)))))))))

(defmethod close ((stream input-stream) &key abort)
  (declare (ignore abort))
  (cffi:foreign-free (input-buffer stream))
  (setf (input-buffer stream) nil)
  (unistd:close (stream-fd stream)))

(defun input-stream (fd)
  (make-instance 'input-stream :fd fd))

(defmacro with-input-stream ((var fd) &body body)
  (let ((stream (gensym "STREAM-")))
    `(let ((,stream (input-stream ,fd)))
       (unwind-protect (let ((,var ,stream))
                         ,@body)
         (close ,stream)))))

(define-condition stream-output-error (stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "output error on ~S"
                     (stream-error-stream condition))))
  (:documentation "An error that is signalled when an output error happens
on a fd gray stream."))

(defclass output-stream (stream fundamental-binary-output-stream)
  ((output-buffer :initform (cffi:foreign-alloc :unsigned-char
                                                :count *buffer-size*)
                  :accessor output-buffer)
   (output-index :initform 0
                 :accessor output-index
                 :type fixnum+)
   (output-length :initform 0
                  :accessor output-length
                  :type fixnum+)
   (output-max :initform *buffer-size*
               :reader output-max
               :type fixnum+)))

(defgeneric stream-output (stream))

(defmethod stream-output ((stream output-stream))
  "Send buffer data to the file.
Calls write with data ranging from output-index to output-length."
  (let ((buffer (output-buffer stream)))
    (with-accessors ((output-index output-index)) stream
      (let* ((fd (stream-fd stream))
             (buf (cffi:mem-aptr buffer :unsigned-char output-index))
             (buflen (- (output-length stream) output-index))
             (r (if (stream-blocking stream)
                    (unistd:write fd buf buflen)
                    (unistd:write-non-blocking fd buf buflen))))
        (cond ((null r)
               (return-from stream-output nil))
              ((= 0 r)
               (return-from stream-output :eof))
              ((< r 0)
               (error 'stream-output-error :stream stream))
              (t
               (incf output-index r)
               (when (= output-index (output-length stream))
                 (setf output-index 0
                       (output-length stream) 0))))))))

(defmethod stream-force-output ((stream output-stream))
  (stream-output stream))

(defmethod stream-finish-output ((stream output-stream))
  (loop
     (when (= 0 (output-length stream))
       (return))
     (stream-output stream)))

(defmethod stream-write-byte ((stream output-stream) byte)
  (check-if-open stream)
  (let ((buffer (output-buffer stream)))
    (when (= (output-length stream) (output-max stream))
      (let ((r (stream-output stream)))
        (cond ((null r)
               ))))
    (setf (cffi:mem-aref buffer :unsigned-char (input-length stream)) byte)
    (incf (input-length stream)))
  byte)

(defmethod stream-write-sequence ((stream output-stream)
                                  seq start end &key &allow-other-keys)
  (check-if-open stream)
  (let ((buffer (output-buffer stream)))
    (loop
       (unless (< start end)
         (return))
       (let ((e (min (- end start)
                     (- (output-max stream) (output-length stream)))))
         (dotimes (i e)
           (setf (cffi:mem-aref buffer :unsigned-char (output-length stream))
                 (aref seq start))
           (incf (output-length stream))
           (incf start))
         (when (= (output-length stream) (output-max stream))
           (stream-finish-output stream))))))

(defmethod close ((stream output-stream) &key abort)
  (declare (ignore abort))
  (stream-finish-output stream)
  (cffi:foreign-free (output-buffer stream))
  (setf (output-buffer stream) nil)
  (unistd:close (stream-fd stream)))

(defun output-stream (fd)
  (make-instance 'output-stream :fd fd))

(defmacro with-output-stream ((var fd) &body body)
  (let ((stream (gensym "STREAM-")))
    `(let ((,stream (output-stream ,fd)))
       (unwind-protect (let ((,var ,stream))
                         ,@body)
         (close ,stream)))))

(defclass io-stream (input-stream output-stream)
  ())

(defmethod close ((stream io-stream) &key abort)
  (declare (ignore abort))
  (stream-finish-output stream)
  (cffi:foreign-free (input-buffer stream))
  (setf (input-buffer stream) nil)
  (cffi:foreign-free (output-buffer stream))
  (setf (output-buffer stream) nil)
  (unistd:close (stream-fd stream)))

(defun io-stream (fd)
  (make-instance 'io-stream :fd fd))

(defmacro with-io-stream ((var fd) &body body)
  (let ((stream (gensym "STREAM-")))
    `(let ((,stream (io-stream ,fd)))
       (unwind-protect (let ((,var ,stream))
                         ,@body)
         (close ,stream)))))
