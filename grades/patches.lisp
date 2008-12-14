;;; these patches allow the database to use latin1 irrespective of default
;;; character encoding

(in-package #:postgresql-socket)

(defun read-socket-sequence (stream length &optional (allow-wide t))
  (declare (stream stream)
	   (optimize (speed 3) (safety 0)))
  #-sb-unicode
  (let ((result (make-string length)))
    (dotimes (i length result)
      (declare (fixnum i))
      (setf (char result i) (code-char (read-byte stream)))))
  #+sb-unicode
  (let ((bytes (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
    (read-sequence bytes stream)
    (if allow-wide
        (sb-ext:octets-to-string bytes :external-format :utf8)
        (map 'string #'code-char bytes))))

(defun postgresql-socket::read-socket-value-string (socket)
  (declare (type stream socket))
  #-sb-unicode
  (with-output-to-string (out)
    (loop for code = (read-byte socket)
          until (zerop code)
          do (write-char (code-char code) out)))
  #+sb-unicode
  (let ((bytes (make-array 64
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0)))
    (loop for code = (read-byte socket)
          until (zerop code)
          do (vector-push-extend code bytes))
    (sb-ext:octets-to-string bytes :external-format :utf8)))

(defun send-socket-value-string (socket value)
  (declare (type stream socket)
	   (type string value))
  #-sb-unicode
  (loop for char across value
        for code = (char-code char)
        do (write-byte code socket)
        finally (write-byte 0 socket))
  #+sb-unicode
  (write-sequence (sb-ext:string-to-octets value  :external-format :utf8
                                           :null-terminate t) socket)
  nil)
