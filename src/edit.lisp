(in-package :clim-edit)

(defclass edit-pointer-documentation-view (clim:pointer-documentation-view)
  ())

(defvar +edit-pointer-documentation-view+ (make-instance 'edit-pointer-documentation-view))

;;; Workaround for bug in McCLIM
(defmethod clim-internals::%invalidate-cached-device-regions ((sheet climi::mirrored-pixmap))
  nil)

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
  ((rows     :initform (make-array 100 :initial-element nil :adjustable t)
             :reader content-cache/rows)
   (num-rows :initform 0
             :accessor content-cache/num-rows)))

(defclass editor-pane (clim:basic-gadget)
  ((buffer :initform nil
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

(defmethod initialize-instance :after ((obj editor-pane) &key buffer)
  (unless buffer
    (error "editor-pane requires a buffer"))
  (assign-buffer obj buffer))

(defun assign-buffer (pane buffer)
  (check-type pane editor-pane)
  (check-type buffer buffer)
  (alexandria:when-let ((old-buffer (editor-pane/buffer pane)))
    (assert (member pane (buffer/frames old-buffer)))
    (setf (buffer/frames old-buffer) (remove pane (buffer/frames old-buffer))))
  (assert (not (member pane (buffer/frames buffer))))
  (setf (slot-value pane 'buffer) buffer)
  (push pane (buffer/frames buffer))
  (setf (slot-value pane 'editor-pane-cursor) (make-cursor buffer)))

(defmacro with-current-frame (pane &body body)
  (alexandria:once-only (pane)
    `(let ((*frame* ,pane))
       ,@body)))

;;; Format of glyph key: ("character" cursor-p). The final column contains NIL as character

(defun p (m v)
  (log:info "~a: ~s" m v)
  v)

(defun compute-line-keys (pane line)
  (collectors:with-collector (result)
    (loop
      with pos = 0
      with cursor = (editor-pane/cursor pane)
      for item across (cluffer:items line)
      for v = (string item)
      do (progn
           (result (list v
                         (and (eq line (cluffer:line cursor))
                              (eql (cluffer:cursor-position cursor) pos))))
           (incf pos))
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
              (declare (ignore width cursor-dy))
              (when (or (null max-ascent) (> ascent max-ascent))
                (setq max-ascent ascent))
              (let ((descent (- height ascent)))
                (when (or (null max-descent) (> descent max-descent))
                  (setq max-descent descent))
                (prog1
                    (make-instance 'cached-glyph :ascent ascent
                                                 :descent descent
                                                 :width cursor-dx
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
    do (setf (aref rows i) nil))
  (setf (content-cache/num-rows cache) 0))

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
        with line-count = (cluffer:line-count buf)
        with row-index = (editor-pane/scroll-pos pane)
        with cache-row-index = 0
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
                                         :clipping-region clip-region)
                        (clim:copy-from-pixmap pixmap x y width (+ ascent descent) pane x y))
                 do (progn
                      (incf x width)
                      (setf old-glyph-list (cdr old-glyph-list)))
                 finally (when (and (or (null old-cache-row)
                                        (< x (content-cache-row/width old-cache-row)))
                                    (< x region-x2))
                           (clim:draw-rectangle* pixmap x y region-x2 (+ y ascent descent)
                                                 :filled t
                                                 :ink clim:+white+)
                           (clim:copy-from-pixmap pixmap x y (- region-x2 region-x1) (+ ascent descent) pane x y))))
             (incf y (+ ascent descent))
             (setf (aref cache-rows cache-row-index) new-cache-row)
             (incf row-index)
             (incf cache-row-index))
        until (>= row-index line-count)
        finally (progn
                  ;; Check if there is old content that needs to be cleared at the end of the buffer
                  (when (< cache-row-index (content-cache/num-rows cache))
                    (let* ((last-cache (aref cache-rows (1- (content-cache/num-rows cache))))
                           (clear-bottom-y (+ (content-cache-row/y last-cache)
                                              (content-cache-row/ascent last-cache)
                                              (content-cache-row/descent last-cache)))
                           (rect-x (max 0 region-x1)))
                      (clim:draw-rectangle* pixmap
                                            rect-x y region-x2 clear-bottom-y
                                            :filled t :ink clim:+white+)
                      (clim:copy-from-pixmap pixmap rect-x y (- region-x2 region-x1) (- clear-bottom-y y) pane rect-x y)))
                  ;; Update the number of cached rows
                  (setf (content-cache/num-rows cache) cache-row-index))))))

(defmethod clim:handle-repaint ((pane editor-pane) region)
  (multiple-value-bind (x1 y1 x2 y2)
      (clim:bounding-rectangle* region)
    (clim:draw-rectangle* pane x1 y1 x2 y2 :ink clim:+white+ :filled t)
    (recompute-and-repaint-content pane :force-repaint t :region region)
    (unless (editor-pane/pixmap pane)
      (recompute-and-repaint-content pane))
    (clim:copy-from-pixmap (editor-pane/pixmap pane) x1 y1 (- x2 x1) (- y2 y1) pane x1 y1)))

(defmethod clim:handle-event ((pane editor-pane) (event clim:key-release-event))
  (log:trace "key event: ~s" event)
  (with-current-frame pane
    (process-key-event *global-keymap* event)
    (recompute-and-repaint-content *frame*)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keyboard stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-keyboard-event (event)
  "Given keyboard event EVENT, generate the corresponding key descriptor."
  (let ((modifiers (clim:event-modifier-state event)))
    (flet ((m (type name)
             (if (eql (logand modifiers type) type) (list name) nil)))
      (list* :key
             (clim:keyboard-event-key-name event)
             (append (m clim:+shift-key+ :shift)
                     (m clim:+control-key+ :control)
                     (m clim:+meta-key+ :meta)
                     (m clim:+super-key+ :super)
                     (m clim:+hyper-key+ :hyper)
                     (m climi::+alt-key+ :alt))))))

(defun process-key-event (keymap event)
  (alexandria:when-let ((parsed (parse-keyboard-event event)))
    (log:trace "parsed: ~s" parsed)
    (unless (process-key keymap parsed)
      (alexandria:when-let ((v (clim:keyboard-event-character event)))
        (insert-string (string v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some standard editing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-edit-function move-right (() ((:key :|f| :control) (:key :right)))
  (buffer-move-right (editor-pane/cursor *frame*)))

(define-edit-function move-left (() ((:key :|b| :control) (:key :left)))
  (buffer-move-left (editor-pane/cursor *frame*)))

(define-edit-function move-up (() ((:key :|p| :control) (:key :up)))
  (buffer-move-up (editor-pane/cursor *frame*)))

(define-edit-function move-down (() ((:key :|n| :control) (:key :down)))
  (buffer-move-down (editor-pane/cursor *frame*)))

(define-edit-function delete-left (() ((:key :backspace)))
  (buffer-delete-left (editor-pane/cursor *frame*)))

(define-edit-function delete-right (() ((:key :|d| :control) (:key :delete)))
  (buffer-delete-right (editor-pane/cursor *frame*)))

(define-edit-function beginning-of-line (() ((:key :|a| :control) (:key :home)))
  (buffer-beginning-of-line (editor-pane/cursor *frame*)))

(define-edit-function end-of-line (() ((:key :|e| :control) (:key :end)))
  (buffer-end-of-line (editor-pane/cursor *frame*)))

(define-edit-function insert-newline (() ((:key :return)))
  (insert-string (format nil "~c" #\Newline)))

(defun insert-string (string)
  (buffer-insert-string (editor-pane/cursor *frame*) string))
