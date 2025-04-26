;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-2 Clause

(in-package :genarm)

;; Stolen from Nclasses.
(defmacro define-generic (name (&rest method-args) &body (documentation . body))
  `(let ((generic (defgeneric ,name (,@(mapcar #'first (mapcar #'ensure-list method-args)))
                    (:method (,@method-args)
                      ,@body)
                    (:documentation ,documentation))))
     (setf (documentation (fdefinition ',name) t) ,documentation)
     (ignore-errors
      (setf (documentation ',name 'function) ,documentation))
     generic))

(defun rand-elt (seq)
  (let ((idx (random (length seq))))
    (elt seq idx)))

(defparameter *person* 3)

(defmacro person-case (1-case 2-case 3-case 11-case 22-case 33-case)
  `(case *person*
     (1 ,1-case)
     (2 ,2-case)
     (3 ,3-case)
     (11 ,11-case)
     (22 ,22-case)
     (33 ,33-case)))

(defun memqual (x list)
  (member x list :test #'equal))

(define-generic operation (op &rest args)
  "Do complex built-in OPeration/function on ARGS.
Ensure that the ARGS get `resolve'-d before applyin the OP."
  (declare (ignore args))
  (cerror "Skip operation" "Unknown operation ~s" op))

(define-generic resolve ((form string))
  "Resolve the FORM to the end word/phrase."
  form)

(defmethod resolve ((form symbol))
  (let ((indexed (getf *structures* form)))
    (if indexed
        (resolve indexed)
        (error "Unknown symbol: ~s" form))))

(defmethod resolve ((form list))
  "List FORM is either a sentence/phrase or a special operation"
  (flet ((resolve-list (x)
           (ensure-list (resolve x))))
    (if (ignore-errors (find-method #'operation '() `((eql ,(first form)))))
        (apply #'operation (first form) (rest form))
        (mapcan #'resolve-list form))))

(defmethod resolve ((form (eql :n)))
  (rand-elt *nouns*))

(defmethod resolve ((form (eql :m)))
  (person-case
   "եմ" "եu" "է"
   "ենք" "եք" "են"))

(defmethod resolve ((form (eql :a)))
  (rand-elt *adjectives*))

(defmethod resolve ((form (eql :v)))
  (rand-elt *verbs*))

(defmethod resolve ((form (eql :av)))
  (rand-elt *adverbs*))

(defmethod resolve ((form (eql :p)))
  (let ((pronoun (rand-elt '("ես" "մենք" "դու" "դուք" "նա" "սա" "նրանք" "սրանք"))))
    (setf *person*
          (or (case (intern (string-upcase pronoun) :keyword)
                (:ես 1)
                (:մենք 11)
                (:դու 2)
                (:դուք 22)
                ((:նա :սա) 3)
                ((:նրանք :սրանք) 33))
              3))
    pronoun))

(defmethod operation ((op (eql :*)) &rest args)
  (let ((count (random 2))
        (form (first args)))
    (loop for i below count
          collect (resolve form))))

(defmethod operation ((op (eql :?)) &rest args)
  (if (zerop (random 2))
      (resolve (first args))
      nil))

(defmethod operation ((op (eql :or)) &rest args)
  (resolve (rand-elt args)))

(defmethod operation ((op (eql :ar)) &rest args)
  (strcat (resolve (first args))
          (if (= *person* 3)
              (rand-elt '("ը" "ս" "դ"))
              "")))

(defmethod operation ((op (eql :pt)) &rest args)
  (let ((resolved (resolve (first args))))
    (cond
      ((memqual resolved '("գալ" "տալ"))
       (strcat resolved "իս"))
      (:else (frob-substrings  '("ել" "ալ") "ում")))))

(defmethod operation ((op (eql :sft)) &rest args)
  (strcat (resolve (first args)) "ու"))

(defmethod operation ((op (eql :ft)) &rest args)
  (flet ((sbutlast (s)
           (subseq s 0 (1- (length s)))))
    (let* ((resolved (resolve (first args)))
           (butlast (sbutlast resolved)))
      (strcat "կ" (if (string-suffix-p resolved "ել")
                      (strcat (sbutlast butlast) "ի")
                      (strcat
                       butlast (person-case
                                "մ" "u" ""
                                "նք" "ք" "ն")))))))

(defmethod operation ((op (eql :pst)) &rest args)
  (flet ((sbutlast (s)
           (subseq s 0 (1- (length s)))))
    (let* ((resolved (resolve (first args)))
           (butlast (sbutlast resolved)))
      (cond
        ((memqual resolved (append '("եմ" "եծ" "է" "ենք" "եք" "են")
                                   '("չեմ" "չեծ" "չէ" "չենք" "չեք" "չեն")))
         (cond
           ((string-suffix-p resolved "եմ") "էի")
           ((string-suffix-p resolved "ես") "էիր")
           ((string-suffix-p resolved "է") "էր")
           ((string-suffix-p resolved "ենք") "էինք")
           ((string-suffix-p resolved "եք") "էիք")
           ((string-suffix-p resolved "են") "էին")))
        ((or (string-suffix-p resolved "անալ")
             (string-suffix-p resolved "անել")
             (string-suffix-p resolved "ենալ"))
         (strcat (sbutlast (sbutlast butlast))
                 (person-case
                  "ցա" "ցար" "ցավ"
                  "ցանք" "ցաք" "ցան")))
        ((string-suffix-p resolved "նել")
         (strcat (sbutlast (sbutlast (sbutlast butlast)))
                 (person-case
                  "ա" "ար" "ավ"
                  "անք" "աք" "ան")))
        ((string= resolved "գալ")
         (person-case
          "եկա" "եկար" "եկավ"
          "եկանք" "եկաք" "եկան"))
        ((string= resolved "տալ")
         (person-case
          "տվեցի" "տվեցիր" "տվեց"
          "տվեցինք" "տվեցիք" "տվեցին"))
        ((string= resolved "ուտել")
         (person-case
          "կերա" "կերար" "կերավ"
          "կերանք" "կերաք" "կերան"))
        (:else
         (strcat (sbutlast butlast)
                 (person-case
                  "ցի" "ցիր" "ց"
                  "ցինք" "ցիք" "ցին")))))))

(defmethod operation ((op (eql :nt)) &rest args)
  (let ((modal (resolve (first args))))
    (strcat "չ" (if (equal "է" modal) "ի" modal))))

(defmethod operation ((op (eql :pass)) &rest args)
  (frob-substrings
   (resolve (first args)) '("ել" "ալ")
   #'(lambda (match frob)
       (funcall frob (strcat "վ" match)))))

(defun generate (&optional (id :sentence))
  (let* ((*person* 3)
         (resolved (ensure-list (resolve id))))
    (strcat
     (reduce #'(lambda (acc val)
                 (strcat acc " " val))
             (rest resolved)
             :initial-value (first resolved))
     "։")))

(generate :simple-past)
