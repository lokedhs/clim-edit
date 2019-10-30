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
   (x       :initarg :x
            :reader cached-glyph/x)
   (key     :initarg :key
            :reader cached-glyph/key)))

(defclass content-cache-row ()
  ((y        :initarg :y
             :reader content-cache-row/y)
   (width    :initarg :width
             :reader content-cache-row/width)
   (ascent   :initarg :ascent
             :reader content-cache-row/ascent)
   (descent  :initarg :descent
             :reader content-cache-row/descent)
   (elements :initarg :elements
             :reader content-cache-row/elements)))

(defclass content-cache ()
  ((rows :initform (make-array 100 :initial-element nil :adjustable t)
          :reader content-cache/rows)))

(defclass editor-pane (clim:basic-gadget)
  ((buffer :initform (make-buffer)
           :initarg :buffer
           :reader editor-pane/buffer)
   (editor-pane-cursor :reader editor-pane/cursor)
   (scroll-pos :initform 0
               :accessor editor-pane/scroll-pos)
   (default-text-style :initform (clim:make-text-style "Source Code Pro Medium" "Medium" 20)
                       :initarg :default-text-style
                       :reader editor-pane/text-style)
   (content-cache :initform (make-instance 'content-cache)
                  :reader editor-pane/content-cache)
   (pixmap :initform nil
           :accessor editor-pane/pixmap)))

(defmethod initialize-instance :after ((obj editor-pane) &key)
  (setf (slot-value obj 'editor-pane-cursor) (make-cursor (editor-pane/buffer obj))))

(defmacro with-current-frame (pane &body body)
  (alexandria:once-only (pane)
    `(let ((*frame* ,pane))
       ,@body)))

(defun line->string (line)
  (coerce (cluffer:items line) 'string))

(defun line->graphemes (line)
  #+sbcl
  (sb-unicode:graphemes (line->string line))
  #-sbcl
  (mapcar #'string (coerce (line->string line) 'list)))

;;; Format of glyph key: ("character" cursor-p). The final column contains NIL as character

(defun p (m v)
  (log:info "~a: ~s" m v)
  v)

(defun compute-line-keys (pane line)
  (collectors:with-collector (result)
    (loop
      with pos = 0
      with cursor = (editor-pane/cursor pane)
      for v in (line->graphemes line)
      do (progn
           (result (list v
                         (and (eq line (cluffer:line cursor))
                              (eql (cluffer:cursor-position cursor) pos))))
           (incf pos (length v)))
      finally (return (result (list nil (and (eq line (cluffer:line cursor))
                                             (eql (cluffer:cursor-position cursor) pos))))))))

(defun recompute-height (pane line-keys y)
  (loop
    with text-style = (editor-pane/text-style pane)
    with char-width = (clim:text-style-width text-style pane)
    with max-ascent = nil
    with max-descent = nil
    with x = 0
    for key in line-keys
    collect (multiple-value-bind (width height cursor-dx cursor-dy ascent)
                (clim:text-size pane (or (first key) " ") :text-style text-style)
              (declare (ignore width cursor-dx cursor-dy))
              (when (or (null max-ascent) (> ascent max-ascent))
                (setq max-ascent ascent))
              (let ((descent (- height ascent)))
                (when (or (null max-descent) (> descent max-descent))
                  (setq max-descent descent))
                (prog1
                    (make-instance 'cached-glyph :ascent ascent
                                                 :descent descent
                                                 :width char-width
                                                 :x x
                                                 :key key)
                  (incf x char-width))))
      into glyph-line
    finally (return (make-instance 'content-cache-row
                                   :ascent (or max-ascent 10)
                                   :descent (or max-descent 5)
                                   :elements glyph-line
                                   :y y
                                   :width x))))

(defun flush-content-cache (cache)
  (loop
    with rows = (content-cache/rows cache)
    for i from 0 below (length rows)
    do (setf (aref rows i) nil)))

(defun reallocate-pixmap (pane)
  (multiple-value-bind (pane-x1 pane-y1 pane-x2 pane-y2)
      (clim:bounding-rectangle* (clim:sheet-region pane))
    (let ((width (- pane-x2 pane-x1))
          (height (- pane-y2 pane-y1))
          (old-pixmap (editor-pane/pixmap pane)))
      (when (or (null old-pixmap)
                (/= (clim:pixmap-width old-pixmap) width)
                (/= (clim:pixmap-height old-pixmap) height))
        ;; Either we didn' thave a pixmap allocated, or it's of the
        ;; wrong size. Allocate a new one and flush the content cache.
        (let ((pixmap (clim:allocate-pixmap pane width height)))
          (clim:draw-rectangle* pixmap 0 0 (1- width) (1- height) :filled t :ink clim:+white+)
          (setf (editor-pane/pixmap pane) pixmap)
          (flush-content-cache (editor-pane/content-cache pane)))))))

(defun recompute-and-repaint-content (pane &key force-repaint region)
  (reallocate-pixmap pane)
  ;;
  (multiple-value-bind (region-x1 region-y1 region-x2 region-y2)
      (clim:bounding-rectangle* (clim:region-intersection (clim:sheet-region pane) (or region clim:+everywhere+)))
    (let* ((cache (editor-pane/content-cache pane))
           (cache-rows (content-cache/rows cache))
           (buf (editor-pane/buffer pane))
           (text-style (editor-pane/text-style pane))
           (pixmap (editor-pane/pixmap pane)))
      (loop
        with y = 0
        for row-index from (editor-pane/scroll-pos pane) below (cluffer:line-count buf)
        for cache-row-index from 0
        for line = (cluffer:find-line buf row-index)
        for line-keys = (compute-line-keys pane line)
        until (> y region-y2)
        do (let* ((new-cache-row (recompute-height pane line-keys y))
                  (old-cache-row (aref cache-rows cache-row-index))
                  (full-repaint (or force-repaint
                                    (null old-cache-row)
                                    (not (eql (content-cache-row/y old-cache-row) y))))
                  (old-glyph-list (if full-repaint nil (content-cache-row/elements old-cache-row)))
                  (ascent (content-cache-row/ascent new-cache-row))
                  (descent (content-cache-row/descent new-cache-row)))
             (when (<= region-y1 (+ y ascent descent))
               (loop
                 with x = 0
                 for cached-glyph in (content-cache-row/elements new-cache-row)
                 for key = (cached-glyph/key cached-glyph)
                 for width = (cached-glyph/width cached-glyph)
                 when (and (<= region-x1 (+ x width))
                           (>= region-x2 x)
                           (or full-repaint
                               (null old-glyph-list)
                               (not (equal key (cached-glyph/key (car old-glyph-list))))
                               (not (eql (cached-glyph/width cached-glyph)
                                         (cached-glyph/width (car old-glyph-list))))
                               (not (eql (cached-glyph/x cached-glyph)
                                         (cached-glyph/x (car old-glyph-list))))))
                   do (let ((clip-region (clim:make-rectangle* x y (+ x width) (+ y ascent descent)))
                            (draw-cursor (second key)))
                        (clim:draw-rectangle* pixmap x y (+ x width) (+ y ascent descent)
                                              :filled t
                                              :ink (if draw-cursor clim:+black+ clim:+white+)
                                              :clipping-region clip-region)
                        (clim:draw-text* pixmap (or (first key) " ")
                                         x (+ y ascent)
                                         :text-style text-style
                                         :ink (if draw-cursor clim:+white+ clim:+black+)
                                         :clipping-region clip-region))
                 do (progn
                      (incf x width)
                      (setf old-glyph-list (cdr old-glyph-list)))
                 finally (when (and (or (null old-cache-row)
                                        (< x (content-cache-row/width old-cache-row)))
                                    (< x region-x2))
                           (clim:draw-rectangle* pixmap x y region-x2 (+ y ascent descent)
                                                 :filled t
                                                 :ink clim:+white+))))
             (incf y (+ ascent descent))
             (setf (aref cache-rows cache-row-index) new-cache-row))))))

(defmethod clim:handle-repaint ((pane editor-pane) region)
  (multiple-value-bind (x1 y1 x2 y2)
      (clim:bounding-rectangle* region)
    (log:info "repaint: ~s" region)
    (clim:draw-rectangle* pane x1 y1 x2 y2 :ink clim:+white+ :filled t)
    (recompute-and-repaint-content pane :force-repaint t :region region)
    (clim:copy-from-pixmap (editor-pane/pixmap pane) x1 y1 (- x2 x1) (- y2 y1) pane x1 y1)
    (clim:draw-line* pane 10 10 100 50 :ink clim:+red+)))

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
    (recompute-and-repaint-content *frame*)))

(define-edit-function move-left (() (:left))
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
    (recompute-and-repaint-content *frame*)))

(defun insert-string (string)
  (let ((cursor (editor-pane/cursor *frame*)))
    (loop
      for ch across string
      if (or (eql ch #\Newline) (eql ch #\Return))
        do (cluffer:split-line cursor)
      else
        do (cluffer:insert-item cursor ch))
    (recompute-and-repaint-content *frame*)))
