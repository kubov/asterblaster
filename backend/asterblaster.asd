(in-package #:cl-user)

(defpackage #:asterblaster-system
    (:use #:cl #:asdf))

(in-package #:asterblaster-system)

(defsystem asterblaster
    :name "asterblaster"
    :author ""
    :version "0.1"
    :licence "GNU GPL v3 or later"
    :description ""
    :depends-on
    (:clws :hu.dwim.defclass-star+hu.dwim.def :cl-json :alexandria)
    :properties ((#:author-email . "example@example.com"))
    :serial t
    :components ((:file "package")
                 (:file "server")))
