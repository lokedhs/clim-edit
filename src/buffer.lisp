(in-package :clim-edit)

(defclass buffer (cluffer-standard-buffer:buffer)
  ()
  (:documentation "A buffer contains all the editable content.
Note that the cursor is not part of the buffer, as many
frames can display the same buffer."))

(defclass line (cluffer-simple-line:line)
  ())

(defun make-buffer ()
  (let* ((line (make-instance 'line))
         (buf (make-instance 'buffer :initial-line line)))
    buf))

(defun make-cursor (buf)
  (let ((cursor (make-instance 'cluffer-simple-line:right-sticky-cursor)))
    (cluffer:attach-cursor cursor (cluffer:find-line buf 0))
    cursor))

(defun print-buf (buf)
  (loop
    for row from 0 below (cluffer:line-count buf)
    for line = (cluffer:find-line buf row)
    do (format t "[~d]: ~s~%" row (reduce (lambda (a b) (concatenate 'string a b)) (cluffer:items line)))))

(defun foo ()
  (let* ((buf (make-buffer))
         (cursor (make-instance 'cluffer-simple-line:right-sticky-cursor)))
    (cluffer:attach-cursor cursor (cluffer:find-line buf 0))
    (labels ((insert (s)
               (loop for ch across s do (cluffer:insert-item cursor ch))))
      (insert "This is some text")
      (cluffer:split-line cursor)
      (insert "Some more text here")
      (cluffer:split-line cursor)
      (insert "More text abc foo"))
    (cluffer:detach-cursor cursor)
    buf))
