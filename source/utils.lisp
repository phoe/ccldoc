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

(defun gensymp (thing)
  "Returns true iff THING is a gensym (an uninterned symbol)."
  (and (symbolp thing) (null (symbol-package thing))))

(defun desym (thing)
  (if (gensymp thing)
    (symbol-name thing)
    thing))

;; this went through many iterations, but currently settled on keywords...
(defun op-name (sym &optional intern)
  (and (symbolp (desym sym))
       (if intern
         (intern (symbol-name sym) :keyword)
         (find-symbol (symbol-name sym) :keyword))))

(defun operator= (operator-1 operator-2)
  "Returns true iff the two CCLDOC operator are equivalent to each other."
  (and (symbolp (desym operator-1))
       (symbolp (desym operator-2))
       (string= (symbol-name operator-1) (symbol-name operator-2))))

(defun normalize-whitespace (string)
  "Returns a copy of the string with normalized whitespace. All occurrences
of whitespace in the string are turned into spaces; additionally, multiple
consecutive whitespace characters are collapsed into one."
  (check-type string string)
  (let ((res (make-array (length string) :element-type 'character
                                         :fill-pointer 0)))
    (loop for lastch = #\x then ch
          for ch across string
          do (unless (whitespacep ch)
               (when (whitespacep lastch)
                 (vector-push #\Space res))
               (vector-push ch res)))
    (let* ((length (fill-pointer res))
           (start (if (and (> length 0) (eql #\Space (aref res 0))) 1 0)))
      (subseq res start length))))

(defun concat-by (sep &rest strings)
  "Concatenates all STRINGS with SEP inserted between each pair of them."
  (let* ((sep-seq (if (typep sep 'sequence) sep (string sep)))
         (args (loop for string in strings
                     unless (eql (length string) 0)
                       collect string and collect sep-seq)))
    (apply #'concatenate 'string (butlast args))))
