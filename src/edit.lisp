(in-package :clim-edit)

(defclass edit-pointer-documentation-view (clim:pointer-documentation-view)
  ())

(defvar +edit-pointer-documentation-view+ (make-instance 'edit-pointer-documentation-view))

(defclass editor-pane (clim:basic-gadget)
  ())

(defmethod clim:handle-repaint ((pane editor-pane) region)
  (multiple-value-bind (x1 y1 x2 y2)
      (clim:bounding-rectangle* region)
    (clim:draw-rectangle* pane x1 y1 x2 y2 :ink clim:+white+ :filled t)))

(defun make-editor-pane ()
  (clim:make-pane 'editor-pane))

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
