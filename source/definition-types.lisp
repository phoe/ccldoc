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

(def-definition-type :type ()
                     :id-prefix "t_"
                     :function #'identity)

(def-definition-type :class (:type)
                     :id-prefix "c_"
                     :function #'symbol-dspec-name)

(def-definition-type :condition (:type)
                     :id-prefix "c_")

(def-definition-type :function () :id-prefix "f_"
                     :function (lambda (name)
                                 (let ((name (std-dspec-name 'function name)))
                                   (when (or (non-nil-symbolp name) (setf-function-name-p name))
                                     name))))

(def-definition-type :macro (:function)
                     :id-prefix "m_")

(def-definition-type :generic-function (:function)
                     :id-prefix "f_")

(def-definition-type :lap-macro (:function)
                     :id-prefix "f_")

;; TODO this does not work with EQL-specializers
(def-definition-type
    :method ()
    :id-prefix "m_"
    :function (lambda (name)
                (multiple-value-bind (gf-name quals specs)
                    (ccl::method-def-parameters name)
                  (if gf-name
                    `(,gf-name
                      ,@quals
                      ,(mapcar
                        (lambda (s) (if (typep s 'class) (class-name s) s))
                        specs))
                    ;; TODO: For now allow symbols because the only use we
                    ;; have is with symbols, but in the future disallow this.
                    (and (symbolp name) name)))))

(def-definition-type :variable ()
                     :id-prefix "v_"
                     :function #'symbol-dspec-name)

(def-definition-type :reader-macro ()
                     :id-prefix "r_"
                     :function (lambda (name)
                                 (if (characterp name)
                                     (string name)
                                     (and (typep name 'sequence)
                                          (every #'characterp name)
                                          (<= 1 (length name) 2)
                                          (coerce name 'string)))))

(def-definition-type :package ()
                     :id-prefix "p_"
                     :function (lambda (name) (std-dspec-name 'package name)))

(def-definition-type :toplevel-command ()
                     :id-prefix "tc_"
                     :function #'string-dspec-name)

(def-definition-type :hemlock-variable ()
                     :id-prefix "hv_"
                     :function #'string-dspec-name)

(def-definition-type :hemlock-command ()
                     :id-prefix "hc_"
                     :function #'string-dspec-name)
