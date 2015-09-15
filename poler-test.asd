#|
  This file is a part of poler project.
  Copyright (c) 2015 carrotflakes (carrotflakes@gmail.com)
|#

(in-package :cl-user)
(defpackage poler-test-asd
  (:use :cl :asdf))
(in-package :poler-test-asd)

(defsystem poler-test
  :author "carrotflakes"
  :license "LLGPL"
  :depends-on (:poler
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "poler"))))
  :description "Test system for poler"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
