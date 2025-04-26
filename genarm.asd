;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :asdf)

(defsystem "genarm"
  :description "Describe genarm here"
  :author "Artyom Bologov"
  :homepage "https://github.com/aartaka/genarm"
  :bug-tracker "https://github.com/aartaka/genarm/issues"
  :source-control (:git "https://github.com/aartaka/genarm.git")
  :license  "BSD-2 Clause"
  :version "0.0.0"
  :serial t
  :pathname "source/"
  :components ((:file "package")
               (:file "genarm")))
