;;;; shadchen.asd

(asdf:defsystem #:shadchen
  :serial t
 :depends-on (#:lisp-unit)
  :components ((:file "package")
               (:file "shadchen")))

