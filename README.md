# fd-gray

### fd-gray:input-stream *fd* --> *stream*

Creates an input stream for file descriptor *fd*.

```Lisp
(let ((seq (make-array 128 '(unsigned-byte 8))))
  (read-sequence seq (fd-gray:input-stream 0)))
```

### fd-gray:output-stream *fd* --> *stream*

Creates an output stream for file descriptor *fd*.

```Lisp
(let ((seq (make-array 128 '(unsigned-byte 8))))
  (write-sequence seq (fd-gray:output-stream 1)))
```

###  fd-gray:io-stream *fd* --> *stream*

Creates an input/output stream for file descriptor *fd*.

```Lisp
(let ((seq (make-array 128 '(unsigned-byte 8))
      (stream (fd-gray:io-stream (fcntl:open "file.bin" fcntl:+o-rdwr+))))
  (read-sequence seq stream)
  (write-sequence seq stream))
```

### fd-gray:with-input-stream (*var* *fd*) &body *body* --> *result*

Creates an input stream for file descriptor *fd* that will be closed
returning from *body*.

```Lisp
(fd-gray:with-input-stream (in (fcntl:open "file.bin" fcntl:+o-rdonly+))
  (let ((seq (make-array 128 '(unsigned-byte 8))))
    (read-sequence seq in)
    seq))
```

### fd-gray:with-output-stream (*var* *fd*) &body *body* --> *result*

Creates an output stream for file descriptor *fd* that will be closed
returning from *body*.

```Lisp
(let ((seq (make-array 128 '(unsigned-byte 8))))
  (fd-gray:with-output-stream (out (fcntl:open "file.bin" fcntl:+o-wronly+))
    (write-sequence seq out)))
```

### fd-gray:with-io-stream (*var* *fd*) &body *body* --> *result*

Creates an input/output stream for file descriptor *fd* that will be
closed returning from *body*.

```Lisp
(fd-gray:with-io-stream (out (fcntl:open "file.bin" fcntl:+o-rdwr+))
  (let ((seq (make-array 128 '(unsigned-byte 8))))
    (read-sequence seq out)
    (write-sequence seq out)))
```
