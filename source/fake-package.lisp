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

(in-package :ccldoc)

#|
CCLDoc needs to be able to support symbols which are not present in the current
Common Lisp image but are nonetheless mentioned in the documentation.
For this purpose, CCLDoc has a horrible kludge that works, but will likely
interact with
|#
;; Need to be able to support lisp names that include symbols in packages
;; that don't exist in the current image.
;; For now, this horrible kludge...
;; TODO: make sure symbols not needed once DOM is built, and delete the
;; fake packages once compilation done.
(defvar *ccldoc-fake-packages* nil)

(defun ccldoc-package (name)
  (let ((package (make-package name :use nil)))
    (push package *ccldoc-fake-packages*)
    (import nil package)
    package))

(defmacro with-ccldoc-packages (&body body)
  "Evaluates BODY with CCLDoc-specific handlers that allow automatic creation
of any packages or symbols that do not exist in the current Common Lisp image.
\
The BODY may be evaluated multiple times and therefore should not contain any
side effects."
  `(loop
     (handler-case (return (progn ,@body))
       (no-such-package (c)
         (let ((pkg-name (package-error-package c)))
           (unless (and (stringp pkg-name) (not (find-package pkg-name)))
             (error c))
           (ccldoc-package pkg-name)))
       (external-symbol-not-found (c)
         (let ((name (external-symbol-not-found-symbol-name c))
               (package (external-symbol-not-found-package c)))
           (unless (member package *ccldoc-fake-packages*)
             (error c))
           (export (intern name package) package))))))
