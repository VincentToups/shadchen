;;;; shadchen.asd

;;; Copyright 2012, Vincent Toups
;;; This program is distributed under the terms of the GNU Lesser 
;;; General Public License (see license.txt).

(asdf:defsystem #:shadchen
  :serial t
 :depends-on (#:lisp-unit)
  :components ((:file "package")
               (:file "shadchen")))

