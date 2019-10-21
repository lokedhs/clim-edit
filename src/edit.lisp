(in-package :clim-edit)

(defclass edit-pointer-documentation-view (clim:pointer-documentation-view)
  ())

(defvar +edit-pointer-documentation-view+ (make-instance 'edit-pointer-documentation-view))

(defclass editor-pane (clim:basic-gadget)
  ((buffer :initform (make-buffer)
           :initarg :buffer
           :reader editor-pane/buffer)
   (cursor :reader editor-pane/cursor)
   (scroll-pos :initform 0
               :accessor editor-pane/scroll-pos)
   (default-text-style :initform (clim:make-text-style "Source Code Pro Medium" "Medium" 20)
                       :initarg :default-text-style
                       :reader editor-pane/text-style)))

(defmethod initialize-instance :after ((obj editor-pane) &key)
  (setf (slot-value obj 'cursor) (make-cursor (editor-pane/buffer obj))))

(defmacro with-current-frame (pane &body body)
  (alexandria:once-only (pane)
    `(let ((*frame* ,pane))
       ,@body)))

(defun line->string (line)
  (coerce (cluffer:items line) 'string))

(defun line->graphemes (line)
  (sb-unicode:graphemes (line->string line)))

(defun line-height (pane line)
  (let ((max-ascent nil)
        (max-descent nil))
    (loop
      for v in (line->graphemes line)
      do (multiple-value-bind (width height cursor-dx cursor-dy ascent)
             (clim:text-size pane v :text-style (editor-pane/text-style pane))
           (declare (ignore width cursor-dx cursor-dy))
           (when (or (null max-ascent) (> ascent max-ascent))
             (setq max-ascent ascent))
           (let ((descent (- height ascent)))
             (when (or (null max-descent) (> descent max-descent))
               (setq max-descent descent)))))
    (values max-ascent max-descent)))

(defmethod clim:handle-repaint ((pane editor-pane) region)
  (multiple-value-bind (x1 y1 x2 y2)
      (clim:bounding-rectangle* region)
    (clim:draw-rectangle* pane x1 y1 x2 y2 :ink clim:+white+ :filled t)
    (let* ((text-style (editor-pane/text-style pane))
           (char-width (clim:text-style-width text-style pane))
           (cursor (editor-pane/cursor pane)))
      (loop
        with buf = (editor-pane/buffer pane)
        with y = 0
        for row from (editor-pane/scroll-pos pane) below (cluffer:line-count buf)
        for line = (cluffer:find-line buf row)
        do (multiple-value-bind (ascent descent)
               (line-height pane line)
             (let ((pos 0)
                   (x 0))
               (flet ((at-cursor-p (p)
                        (and (eq line (cluffer:line cursor))
                             (eql pos p))))
                 (loop
                   for v in (line->graphemes line)
                   do (let ((draw-cursor (at-cursor-p (cluffer:cursor-position cursor))))
                        (when draw-cursor
                          (clim:draw-rectangle* pane x y (+ x char-width) (+ y ascent descent)))
                        (clim:draw-text* pane v x (+ y ascent)
                                         :text-style text-style
                                         :ink (if draw-cursor clim:+white+ clim:+black+))
                        (incf pos (length v))
                        (incf x char-width))
                   finally (when (at-cursor-p (cluffer:cursor-position cursor))
                             (clim:draw-rectangle* pane x y (+ x char-width) (+ y ascent descent))))))
             (incf y (+ ascent descent)))))))

(defmethod clim:handle-event ((pane editor-pane) (event clim:key-release-event))
  (with-current-frame pane
    (process-key pane *global-keymap* event)))

(defun make-editor-pane ()
  (clim:make-pane 'editor-pane :buffer (foo)))

(clim:define-application-frame clim-edit-frame (clim:standard-application-frame)
  ()
  (:panes (editor-pane (make-editor-pane))
          (doc :pointer-documentation :default-view +edit-pointer-documentation-view+))
  (:layouts (default (clim:vertically ()
                       editor-pane
                       doc))))

(defun clim-edit ()
  (let ((frame (clim:make-application-frame 'clim-edit-frame
                                            :width 900
                                            :height 600)))
    (clim:run-frame-top-level frame)))

(define-edit-function move-right (() (:right))
  (log:info "move right")
  (let ((cursor (editor-pane/cursor *frame*))
        (buffer (editor-pane/buffer *frame*)))
    (if (= (cluffer:cursor-position cursor) (cluffer:item-count (cluffer:line cursor)))
        ;; At end of line, wrap to next line
        (let ((row-number (cluffer:line-number (cluffer:line cursor))))
          (when (< row-number (1- (cluffer:line-count buffer)))
            (cluffer:detach-cursor cursor)
            (cluffer:attach-cursor cursor (cluffer:find-line buffer (1+ row-number)))))
        ;; ELSE: No need to wrap, just move the cursor one character to the right
        (cluffer:forward-item cursor))
    (clim:repaint-sheet *frame* clim:+everywhere+)))

(define-edit-function move-left (() (:left))
  (log:info "move right")
  (let ((cursor (editor-pane/cursor *frame*)))
    (if (zerop (cluffer:cursor-position cursor))
        ;; At beginning of line, wrap to previous line
        (let ((row-number (cluffer:line-number (cluffer:line cursor))))
          (when (plusp row-number)
            (cluffer:detach-cursor cursor)
            (let ((line (cluffer:find-line (editor-pane/buffer *frame*) (1- row-number))))
              (cluffer:attach-cursor cursor line)
              (setf (cluffer:cursor-position cursor) (cluffer:item-count line)))))
        ;; ELSE: Just move the cursor
        (cluffer:backward-item cursor))
    (clim:repaint-sheet *frame* clim:+everywhere+)))

(defun insert-string (string)
  (let ((cursor (editor-pane/cursor *frame*)))
    (loop
      for ch across string
      if (or (eql ch #\Newline) (eql ch #\Return))
        do (cluffer:split-line cursor)
      else
        do (cluffer:insert-item cursor ch))
    (clim:repaint-sheet *frame* clim:+everywhere+)))
