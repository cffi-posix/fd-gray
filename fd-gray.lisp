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
       :type file-descriptor)))

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
		 :reader input-buffer)
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

(defmethod stream-input ((stream input-stream))
  "Fill buffer with file data.
Tries to read once from input-length to input-max (end of buffer)."
  (let* ((buffer (input-buffer stream))
	 (length (input-length stream))
	 (r (unistd:read (stream-fd stream)
			 (cffi:mem-aptr buffer :unsigned-char length)
			 (- (input-max stream) length))))
    (cond ((= r 0)
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
	(when (eq :eof (stream-input stream))
	  (return-from stream-read-byte :eof)))
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
	     (stream-input stream)))))))

(defmethod close ((stream input-stream) &key abort)
  (declare (ignore abort))
  (cffi:foreign-free (input-buffer stream))
  (setf (input-buffer stream) nil)
  (unistd:close (stream-fd stream)))

(defclass output-stream (stream fundamental-binary-output-stream)
  ((output-buffer :initform (cffi:foreign-alloc :unsigned-char
						:count *buffer-size*)
		  :reader output-buffer)
   (output-index :initform 0
		 :accessor output-index
		 :type fixnum+)
   (output-length :initform 0
		  :accessor output-length
		  :type fixnum+)
   (output-max :initform *buffer-size*
	       :reader output-max
	       :type fixnum+)))

(defmethod stream-output ((stream output-stream))
  "Send buffer data to the file.
Calls write with data ranging from output-index to output-length."
  (let ((buffer (output-buffer stream)))
    (with-accessors ((output-index output-index)) stream
      (loop
	 (when (= output-index (output-length buffer))
	   (setf output-index 0
		 (output-length buffer) 0)
	   (return))
	 (let ((r (unistd:write (stream-fd stream)
				(cffi:mem-aptr buffer :unsigned-char output-index)
				(- (output-length stream) output-index))))
	   (incf output-index r))))))

(defmethod stream-force-output ((stream output-stream))
  (stream-output stream))

(defmethod stream-finish-output ((stream output-stream))
  (stream-output stream))

(defmethod stream-write-byte ((stream output-stream) byte)
  (check-if-open stream)
  (let ((buffer (output-buffer stream)))
    (when (= (output-length stream) (output-max stream))
      (stream-finish-output stream))
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

(defun make-stream (fd)
  (make-instance 'io-stream :fd fd))

(defmacro with-stream ((var fd) &body body)
  `(let ((,var (make-stream ,fd)))
     (unwind-protect (progn ,@body)
       (close ,var))))
