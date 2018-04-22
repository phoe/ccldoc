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

(in-package :ccldoc.tests)

(defun test-with-ccldoc-packages ()
  (unwind-protect
       (progn
         (ccldoc::with-ccldoc-packages
           (read-from-string "(%ccldoc-test-package:foo
                               %ccldoc-test-package:bar
                               %ccldoc-test-package:baz)"))
         (let ((package (find-package '%ccldoc-test-package)))
           (assert (not (null package)))
           (flet ((test (x) (nth-value 1 (find-symbol x package))))
             (assert (eq :external (test "FOO")))
             (assert (eq :external (test "BAR")))
             (assert (eq :external (test "BAZ"))))))
    (when (find-package '%ccldoc-test-package)
      (delete-package '%ccldoc-test-package))))
