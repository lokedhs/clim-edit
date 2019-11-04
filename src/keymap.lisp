(in-package :clim-edit)

(defclass keymap ()
  ((map :initform (make-hash-table :test 'equal)
        :reader keymap/map)))

#+nil
(defgeneric process-key (pane keymap event)
  (:method (pane keymap event)
    ;; The default implemnetation does nothing
    nil)
  (:documentation "Dispatches a key event using the given keymap"))

#+nil
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun valid-key-p (key)
    (and (listp key)
         (eq (first key) :key)
         (symbolp (second key)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun normalise-key-name (key)
    (ecase (car key)
      (:key (cons (car key) (sort (cdr key) #'string<))))))

(defun process-key (keymap key)
  (alexandria:if-let ((mapping (gethash (normalise-key-name key) (keymap/map keymap))))
    (progn
      (funcall mapping)
      t)
    ;; ELSE: No mapping
    nil))

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
         do (unless (valid-key-p mapping)
              (error "Invalid key: ~s" mapping))
         collect `(define-key *global-keymap* ',(normalise-key-name mapping) ',name))))
