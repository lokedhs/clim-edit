(in-package :clim-edit)

(defclass keymap ()
  ((map :initform (make-hash-table :test 'equal)
        :reader keymap/map)))

(defgeneric process-key (pane keymap event)
  (:method (pane keymap event)
    ;; The default implemnetation does nothing
    nil)
  (:documentation "Dispatches a key event using the given keymap"))

(defmethod process-key (pane (keymap keymap) (event clim:key-release-event))
  (let* ((key (clim:keyboard-event-key-name event))
         (mapping (gethash key (keymap/map keymap))))
    (cond ((null mapping)
           ;; No mapping, try to process as plain keyboard input
           (alexandria:when-let ((v (clim:keyboard-event-character event)))
             (insert-string (string v))))
          ((symbolp mapping)
           (funcall mapping))
          ((typep mapping 'keymap)
           (log:warn "Prefix keys are not implemented"))
          (t
           (log:warn "Unknown mapping type: ~s" mapping)))))

(defun define-key (keymap mapping fn)
  (setf (gethash mapping (keymap/map keymap)) fn))

(defvar *global-keymap* (make-instance 'keymap))

(defmacro define-edit-function (name ((&rest args) (&rest mappings)) &body body)
  (check-type name symbol)
  `(progn
     (defun ,name (,@args)
       ,@body)
     ,@(loop
         for mapping in mappings
         collect `(define-key *global-keymap* ',mapping ',name))))
