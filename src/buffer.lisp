(in-package :clim-edit)

(defclass buffer (cluffer-standard-buffer:buffer)
  ()
  (:documentation "A buffer contains all the editable content.
Note that the cursor is not part of the buffer, as many
frames can display the same buffer."))
