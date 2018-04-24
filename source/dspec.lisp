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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; dspecs
;;;

;; a dspec is the canonicalized name of a definition.  They are used as clause name, and hence must
;; obey EQUALP as the equivalence relationship.
;; TODO convert to DSPEC=

#|
We are searching for definition specs in two ways: by their type, and by their
name.
|#

(defparameter *dspec-types* nil)

(defstruct (dspecinfo (:type list))
  type ;; KEYWORD ;; must be first so can use assq
  type-name ;; STRING
  id-prefix ;; STRING
  parent-type ;; KEYWORD
  function ;; FUNCTION-DESIGNATOR
  ;; TODO: function-form that is EQUAL-comparable
  )

(defun register-dspec-type (type parent-type type-name id-prefix function)
  (assert (keywordp type))
  (assert (or (null parent-type) (keywordp parent-type)))
  (when id-prefix
    (assert (and (stringp id-prefix)
                 (> (length id-prefix) 0)
                 (alpha-char-p (char id-prefix 0))
                 (every (lambda (c) (or (alphanumericp c) (find c "_-.")))
                        id-prefix))
            () "Invalid id-prefix: ~S" id-prefix))
  (when function
    (assert (or (symbolp function) (typep function 'function))))
  (let* ((info (make-dspecinfo :type type
                               :type-name type-name
                               :id-prefix id-prefix
                               :parent-type parent-type
                               :function function))
         (old (assq type *dspec-types*)))
    (when (and old (not (equal old info)))
      (warn "Redefining dspec-type ~s" type))
    (when old (setq *dspec-types* (remove old *dspec-types*)))
    (push info *dspec-types*)
    type))

(defun dspec-type-for-type-name (type-name)
  (dspecinfo-type (find type-name *dspec-types* :key #'dspecinfo-type-name :test #'equalp)))

(defun info-for-dspec-type (type)
  (or (assq type *dspec-types*) (error "Unknown dspec type ~s" type)))

(defun id-prefix-for-dspec-type (type)
  (let ((info (info-for-dspec-type type)))
    (or (dspecinfo-id-prefix info)
        (when-let (parent-type (dspecinfo-parent-type info))
          (id-prefix-for-dspec-type parent-type))
        "x_")))

(defun parent-type-for-dspec-type (type)
  (dspecinfo-parent-type (info-for-dspec-type type)))

(defun function-for-dspec-type (type)
  (let ((info (info-for-dspec-type type)))
    (or (dspecinfo-function info)
        (when-let (parent-type (dspecinfo-parent-type info))
          (function-for-dspec-type parent-type))
        #'identity)))

(defun canonicalize-definition-name (type name)
  (funcall (function-for-dspec-type type) name))

(defmacro ccldoc:def-definition-type (type (&optional parent-type) &key type-name id-prefix function)
  (let* ((type (and type (intern (symbol-name type) :keyword)))
         (parent-type (and parent-type (intern (symbol-name parent-type) :keyword)))
         (type-name (or type-name (let ((*print-case* :capitalize))
                                    (substitute #\Space #\- (princ-to-string type))))))
    `(register-dspec-type ,type ,parent-type ,type-name ,id-prefix ,function)))

(defun std-dspec-name (ccl-type name)
  (definition-base-name (definition-type-instance ccl-type) name))

(defun symbol-dspec-name (name)
  (and (symbolp name) name))

(defun string-dspec-name (name)
  (and (stringp name) name))

(def-definition-type :type ()
  :id-prefix "t_"
  :function #'identity)

(def-definition-type :class (:type)
  :id-prefix "c_"
  :function #'symbol-dspec-name)

(def-definition-type :condition (:class)
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

(def-definition-type :method ()
  :id-prefix "m_"
  :function (lambda (name)
              (multiple-value-bind (gf-name quals specs) (method-def-parameters name)
                (if gf-name
                  `(,gf-name ,@quals
                             ,(mapcar (lambda (s) (if (typep s 'class) (class-name s) s)) specs))
                  ;; TODO: For now allow symbols because the only use we have is with symbols, but in the future disallow this.
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


(defun dspec-type-name-p (type)
  (or (eq type t) (assq type *dspec-types*)))

(defstruct (dspec (:constructor %make-dspec) (:predicate dspecp))
  (type t :type (satisfies dspec-type-name-p)) ;; KEYWORD
  name ;; Lisp form comparable with EQUALP
  )

(defun dspec-type-name (dspec)
  (dspecinfo-type-name (info-for-dspec-type (dspec-type dspec))))

;; This is called with type and name as specified by the user, either in the
;; docentry or in a reference to one.
(defun make-dspec (type name)
  (assert (symbolp (desym type)))
  (let* ((ctype (intern (string type) :keyword))
         (cname (canonicalize-definition-name ctype name)))
    (unless cname
      (let* ((dwimmed (and (stringp name)
                           (with-ccldoc-packages (read-from-string name))))
             (dwimmed-cname
               (and dwimmed (canonicalize-definition-name ctype dwimmed))))
        (unless dwimmed-cname
          (error "Invalid ~s definition name ~s" type name))
        (setq cname dwimmed-cname)))
    (%make-dspec :type ctype :name cname)))

(defun make-wild-dspec (name)
  ;; can't canonicalize if don't know type.
  (%make-dspec :type t :name name))

(defun wild-dspec-p (name)
  (and (dspecp name) (eq (dspec-type name) t)))

(defun dspec-subtypep (type super)
  (or (eq super t)
      (eq type super)
      (when-let (parent (parent-type-for-dspec-type type))
        (dspec-subtypep parent super))))
