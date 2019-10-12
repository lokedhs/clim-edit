(defsystem clim-edit
  :name "simple-edit"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Simple CLIM-based editor"
  :depends-on (:alexandria
               :mcclim
               :log4cl
               :cluffer)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "buffer")
                                     (:file "edit")))))
