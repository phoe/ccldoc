;;; Copyright 2014 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(defpackage :ccldoc.tests
  (:use :cl :ccldoc)
  (:export #:run-tests))

(in-package :ccldoc-tests)

(defun run-tests (&optional (package *package*))
  (mapc (alexandria:compose #'funcall #'print)
        (uiop:while-collecting (collect)
          (do-symbols (symbol package)
            (let ((name (symbol-name symbol)))
              (when (and (string= "TEST-" (subseq name 0 5))
                         (fboundp symbol))
                (collect symbol))))))
  (values))
