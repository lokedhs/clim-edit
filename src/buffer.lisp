(in-package :clim-edit)

(defclass buffer (cluffer-standard-buffer:buffer)
  ((frames :initform nil
           :accessor buffer/frames))
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

(defun buffer-move-right (cursor)
  (let ((buffer (cluffer:buffer cursor)))
    (if (= (cluffer:cursor-position cursor) (cluffer:item-count (cluffer:line cursor)))
        ;; At end of line, wrap to next line
        (let ((row-number (cluffer:line-number (cluffer:line cursor))))
          (when (< row-number (1- (cluffer:line-count buffer)))
            (cluffer:detach-cursor cursor)
            (cluffer:attach-cursor cursor (cluffer:find-line buffer (1+ row-number)))))
        ;; ELSE: No need to wrap, just move the cursor one character to the right
        (cluffer:forward-item cursor))))

(defun buffer-move-left (cursor)
  (let ((buffer (cluffer:buffer cursor)))
    (if (zerop (cluffer:cursor-position cursor))
        ;; At beginning of line, wrap to previous line
        (let ((row-number (cluffer:line-number (cluffer:line cursor))))
          (when (plusp row-number)
            (cluffer:detach-cursor cursor)
            (let ((line (cluffer:find-line buffer (1- row-number))))
              (cluffer:attach-cursor cursor line)
              (setf (cluffer:cursor-position cursor) (cluffer:item-count line)))))
        ;; ELSE: Just move the cursor
        (cluffer:backward-item cursor))))

(defun buffer-move-up (cursor)
  (let* ((line (cluffer:line cursor))
         (buffer (cluffer:buffer cursor))
         (row (cluffer:line-number line)))
    (when (plusp row)
      (let ((col (cluffer:cursor-position cursor))
            (new-line (cluffer:find-line buffer (1- row))))
        (cluffer:detach-cursor cursor)
        (cluffer:attach-cursor cursor new-line)
        (setf (cluffer:cursor-position cursor) (min col (cluffer:item-count new-line)))))))

(defun buffer-move-down (cursor)
  (let* ((line (cluffer:line cursor))
         (buffer (cluffer:buffer cursor))
         (row (cluffer:line-number line)))
    (when (< row (1- (cluffer:line-count buffer)))
      (let ((col (cluffer:cursor-position cursor))
            (new-line (cluffer:find-line buffer (1+ row))))
        (cluffer:detach-cursor cursor)
        (cluffer:attach-cursor cursor new-line)
        (setf (cluffer:cursor-position cursor) (min col (cluffer:item-count new-line)))))))

(defun buffer-delete-left (cursor)
  (let* ((buffer (cluffer:buffer cursor))
         (line (cluffer:line cursor))
         (pos (cluffer:cursor-position cursor)))
    (cond ((plusp pos)
           (cluffer:delete-item-at-position line (1- pos)))
          ((and (zerop pos)
                (plusp (cluffer:line-number line)))
           (cluffer:join-line (cluffer:find-line buffer (1- (cluffer:line-number line))))))))

(defun buffer-delete-right (cursor)
  (let* ((buffer (cluffer:buffer cursor))
         (line (cluffer:line cursor))
         (pos (cluffer:cursor-position cursor)))
    (cond ((< pos (cluffer:item-count line))
           (cluffer:delete-item cursor))
          ((< (cluffer:line-number line) (1- (cluffer:line-count buffer)))
           (cluffer:join-line line)))))

(defun buffer-insert-string (cursor string)
  (loop
    for ch across string
    if (eql ch #\Newline)
      do (cluffer:split-line cursor)
    else
      do (cluffer:insert-item cursor ch)))
