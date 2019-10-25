(in-package :clim-edit)

(defclass edit-pointer-documentation-view (clim:pointer-documentation-view)
  ())

(defvar +edit-pointer-documentation-view+ (make-instance 'edit-pointer-documentation-view))

(defclass cached-glyph ()
  ((ascent  :initarg :ascent
            :accessor content-cache-row/ascent)
   (descent :initarg :descent
            :accessor content-cache-row/descent)
   (width   :initarg :width
            :reader cached-glyph/width)
   (key     :initarg :key
            :reader cached-glyph/key)))

(defclass content-cache-row ()
  ((y        :initarg y
             :reader content-cache-row/y)
   (ascent   :initarg :ascent
             :reader content-cache-row/ascent)
   (descent  :initarg :descent
             :reader content-cache-row/descent)
   (elements :initarg :elements
             :reader content-cache-row/elements)))

(defclass content-cache ()
  ((rows :initform (make-array 100 :adjustable t :fill-pointer 0)
          :reader content-cache/rows)))

(defclass editor-pane (clim:basic-gadget)
  ((buffer :initform (make-buffer)
           :initarg :buffer
           :reader editor-pane/buffer)
   (cursor :reader editor-pane/cursor)
   (scroll-pos :initform 0
               :accessor editor-pane/scroll-pos)
   (default-text-style :initform (clim:make-text-style "Source Code Pro Medium" "Medium" 20)
                       :initarg :default-text-style
                       :reader editor-pane/text-style)
   (content-cache :initform (make-instance 'content-cache)
                  :reader editor-pane/content-cache)))

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

(defun line-height (pane line text-style)
  (let ((char-width (clim:text-style-width text-style pane))
        (max-ascent nil)
        (max-descent nil))
    (loop
      for v in (line->graphemes line)
      collect (multiple-value-bind (width height cursor-dx cursor-dy ascent)
                  (clim:text-size pane v :text-style text-style)
                (declare (ignore width cursor-dx cursor-dy))
                (when (or (null max-ascent) (> ascent max-ascent))
                  (setq max-ascent ascent))
                (let ((descent (- height ascent)))
                  (when (or (null max-descent) (> descent max-descent))
                    (setq max-descent descent))
                  (make-instance 'cached-glyph :ascent ascent
                                               :descent descent
                                               :width char-width
                                               :text-style text-style
                                               :content v)))
        into glyph-line
      finally (return (make-instance 'content-cache-row
                                     :ascent max-ascent
                                     :descent max-descent
                                     :elements glyph-line)))))

;;; Format of glyph key: ("character" cursor-p). The final column contains NIL as character

(defun compute-line-keys (pane line)
  (collectors:with-collector (result)
    (loop
      with cursor = (editor-pane/cursor pane)
      with pos = 0
      for v in (line->graphemes line)
      do (progn
           (result (list v
                         (and (eq line (cluffer:line cursor)
                                  (eql (cluffer:cursor-position cursor) pos)))))
           (incf pos))
      finally (list nil (and (eq line (cluffer:line cursor)
                                  (eql (cluffer:cursor-position cursor) pos)))))))

(defun recompute-and-repaint-content (pane)
  (let* ((cache (editor-pane/content-cache pane))
         (cache-rows (content-cache/rows cache))
         (buf (editor-pane/buffer pane)))
    (loop
      with y = 0
      for row-index from (editor-pane/scroll-pos pane) below (cluffer:line-count buf)
      for cache-row-index from 0
      for line = (cluffer:find-line buf row-index)
      for line-keys = (compute-line-keys pane line)
      for cache-row = (aref cache-rows cache-row-index)
      do (progn
           (if (eql (content-cache-row/y cache-row) y)
               ;; Cache row is on the same vertical position, optimised redraw is possible
               x
               ;; ELSE: Cache row has moved, we need to do a full redraw
               y)
           (incf y (+ (content-cache-row/ascent cache-row)
                      (content-cache-row/descent cache-row)))))))

(defun recompute-content-cache (pane)
  (let* ((text-style (editor-pane/text-style pane))
         (cache (editor-pane/content-cache pane))
         (pane-height (nth-value 1 (clim:bounding-rectangle-size pane))))
    (setf (fill-pointer (content-cache/rows cache)) 0)
    (loop
      with buf = (editor-pane/buffer pane)
      with y = 0
      for row from (editor-pane/scroll-pos pane) below (cluffer:line-count buf)
      for line = (cluffer:find-line buf row)
      for cache-row = (line-height pane line text-style)
      for ascent = (content-cache-row/ascent cache-row)
      for descent = (content-cache-row/descent cache-row)
      until (> y pane-height)
      do (progn
           (incf y (+ ascent descent))
           (vector-push-extend cache-row (content-cache/rows cache))))))

(defmethod clim:handle-repaint ((pane editor-pane) region)
  (recompute-content-cache pane)
  (multiple-value-bind (x1 y1 x2 y2)
      (clim:bounding-rectangle* region)
    (clim:draw-rectangle* pane x1 y1 x2 y2 :ink clim:+white+ :filled t)
    (let* ((text-style (editor-pane/text-style pane))
           (cursor (editor-pane/cursor pane))
           (cache (editor-pane/content-cache pane)))
      (loop
        with buf = (editor-pane/buffer pane)
        with y = 0
        for cache-row across (content-cache/rows cache)
        for row from (editor-pane/scroll-pos pane)
        for line = (cluffer:find-line buf row)
        for ascent = (content-cache-row/ascent cache-row)
        for descent = (content-cache-row/descent cache-row)
        do (let ((pos 0)
                 (x 0)
                 (cursor-pos (if (eq line (cluffer:line cursor))
                                 (cluffer:cursor-position cursor)
                                 nil)))
             (loop
               for v in (content-cache-row/elements cache-row)
               for char-width = (cached-glyph/width v)
               do (let ((draw-cursor (and cursor-pos (eql cursor-pos pos))))
                    (when draw-cursor
                      (clim:draw-rectangle* pane x y (+ x char-width) (+ y ascent descent)))
                    (clim:draw-text* pane (cached-glyph/content v) x (+ y ascent)
                                     :text-style text-style
                                     :ink (if draw-cursor clim:+white+ clim:+black+))
                    (incf pos (length (cached-glyph/content v)))
                    (incf x (cached-glyph/width v)))
               finally (when (and cursor-pos (eql cursor-pos pos))
                         (clim:draw-rectangle* pane x y (+ x char-width) (+ y ascent descent)))))
           (incf y (+ ascent descent))))))

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
