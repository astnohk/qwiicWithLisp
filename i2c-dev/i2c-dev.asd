(defsystem "i2c-dev"
    :defsystem-depends-on (:cffi-grovel)
    :depends-on (:cffi)
    :serial t
    :components ((:file "package")
                 (:cffi-grovel-file "i2c-dev-grovel")
                 (:file "i2c-dev")))
