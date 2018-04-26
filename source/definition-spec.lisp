(defclass definition-spec () ())

(defclass function-definition-spec (definition-spec)
  ((%name :initarg :name
          :reader name)
   (%namespace :initform :function
               :reader namespace
               :allocation :class)
   (%namespace-name :initform "Function"
                    :reader namespace-name
                    :allocation :class)
   (%prefix :initform "f_"
            :reader prefix
            :allocation :class)
   (%canonicalize-function :initform #'identity
                           :reader canonicalize-function
                           :allocation :class)
   (%canonicalize-function-form :initform 'identity
                                :reader canonicalize-function-form
                                :allocation :class))
  (:default-initargs :name (error "Must provide NAME.")))

(defmethod initialize-instance :after ((instance definition-spec) &key)
  (unless (slot-boundp instance 'namespace-name)
    (let ((*print-case* :capitalize))
      (setf (slot-value instance 'namespace-name)
            (substitute #\Space #\- (princ-to-string (namespace instance)))))))

(defun definition-spec= (spec-1 spec-2)
  (check-type spec-1 definition-spec)
  (check-type spec-2 definition-spec)
  (and (equalp (name spec-1) (name spec-2))
       (string= (namespace-name spec-1) (namespace-name spec-2))
       (eq (namespace spec-1) (namespace spec-2))
       (string= (prefix spec-1) (prefix spec-2))
       (equal (canonicalize-function-form spec-1)
              (canonicalize-function-form spec-2))))
