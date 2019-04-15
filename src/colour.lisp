;;; -*- mode: Common-Lisp; coding: utf-8-unix -*-

(in-package :jlk.gui)

(defun check-valid-colour-string (colour-def)
  (unless (scan "^[0-9A-Fa-f]{6}$" colour-def)
    (error "Invalid colour string \"~a\"" colour-def)) )

(defclass <colour> ()
  ((names :initarg :names)
   (r :initarg :r)
   (g :initarg :g)
   (b :initarg :b)
   (a :initarg :a
      :initform #xFF)))

(defun %parse-colour-string (colour-def)
  (check-valid-colour-string colour-def)
  (values (parse-integer (subseq colour-def 0 2) :radix 16)
          (parse-integer (subseq colour-def 2 4) :radix 16)
          (parse-integer (subseq colour-def 4 6) :radix 16)
          #xFF))

(defun %hsv->rgb (h s v)
  (list h s v)
  (error "TODO"))

(defun %make-user-colour (colour-def &optional names)
  "Create a colour. Valid formats incude:
- hex string
- plist :r :g :b [:a]
- plist :h :s :v [:a]
- list r g b [a]
"
  (match colour-def
    ((ppcre "^([0-9A-f]{6})$" s)
     (multiple-value-bind (r g b a)
         (%parse-colour-string s)
       (make-instance '<colour> :r r :g g :b b :a a :names names)))

    ((plist :r r :g g :b b :a a)
     (make-instance '<colour> :r r :g g :b b :a a :names names))

    ((plist :r r :g g :b b)
     (make-instance '<colour> :r r :g g :b b :a #xFF :names names))

    ;; TODO HSVA

    ((plist :h h :s s :v v)
     (%hsv->rgb h s v))

    ((list (and r
                (type (integer 0 255)))
           g b a)
     (make-instance '<colour> :r r :g g :b b :a a :names names))

    ((list r g b)
     (make-instance '<colour> :r r :g g :b b :a #xFF :names names))

    (otherwise (error "Invalid colour-def \"~a\"" colour-def))))

(defun random-colour ()
  (make-instance '<colour>
                 :r (random 256)
                 :g (random 256)
                 :b (random 256)
                 :a #xFF))

(defmacro defpalette (name colour-defs)
  "Define a new package `name' containing only the colours defined in colour-defs.

`colour-def' can be in any format acceptable to `%make-user-colour'

The value of the symbol in package `name' is in the format returned by `%make-user-colour'

WARNING: This will delete the package `name'
"
  (check-type name symbol)
  (let ((package-name (symbol-name name))
        (colour (gensym)))
    `(progn ;; (when (find-package ,package-name)
            ;;   (delete-package (find-package ,package-name)))
            (unless (find-package ,package-name)
              (make-package ,package-name :use nil))
            ,@(iter
                (for (name colour-def) in colour-defs)
                (collect (etypecase name
                           (string
                            `(progn (intern ,name ,package-name)
                                    (setf (symbol-value
                                           (find-symbol ,name
                                                        ,package-name))
                                          (%make-user-colour ',colour-def (list ,name)))
                                    (export (find-symbol ,name ,package-name)
                                            (find-package ,package-name))))
                           (list
                            ;; ensure synonyms share the same colour object
                            `(let ((,colour (%make-user-colour ',colour-def (list ,@name))))
                               ,@(iter
                                   (for n in name)
                                   (check-type n string)
                                   (collect `(intern ,n ,package-name))
                                   (collect `(setf (symbol-value
                                                    (find-symbol ,n
                                                                 ,package-name))
                                                   ,colour))
                                   (collect `(export (find-symbol ,n ,package-name)
                                                     (find-package ,package-name))))))))))))
