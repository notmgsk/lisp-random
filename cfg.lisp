(defpackage :lisp-random.cfg
  (:use :common-lisp))

(in-package :lisp-random.cfg)

(defvar *input* nil
  "The input language/IR")

(setq *input* '((= x (+ y 1))
                (= z (+ x 4))
                (if e1
                    (= z (* z 4))
                    (= y 1))
                (if e2
                    (if e3
                        (= p 4)
                        (= l 1))
                    (= v (1)))))

(defclass edge () ())

(defclass conditional-edge (edge)
  ((true-block :initarg :true-block :accessor true-block)
   (false-block :initarg :false-block :accessor false-block)
   (condition :initarg :condition :accessor condition)))

(defun make-conditional-edge (true-block false-block condition)
  (make-instance 'conditional-edge
                 :true-block true-block
                 :false-block false-block
                 :condition condition))

;; TODO unconditional subclass of conditional?
(defclass unconditional-edge (edge)
  ((to-block :initarg :to-block :accessor to-block)))

(defun make-unconditional-edge (to-block)
  (make-instance 'unconditional-edge :to-block to-block))

(defclass cfg-block () ())

(defclass basic-cfg-block (cfg-block)
  ((id :initarg :id :accessor id)
   (outgoing :initarg :outgoing :accessor outgoing)
   (contents :initarg :contents :accessor contents :type cons)))

(let ((id 0))
  (defun make-basic-block (contents &key outgoing)
    ;; TODO Confirm that contents can be a basic block
    (make-instance 'basic-cfg-block
                   :id (incf id)
                   :contents contents
                   :outgoing outgoing)))

(defun link-blocks (from-block to-block)
  (setf (incoming to-block) from-block))

(defun unlink-blocks (from-block to-block)
  (when (equalp (incoming to-block) from-block)
    (link-blocks to-block nil)))

(defun block-from-ir-item (item &key outgoing)
  (cond
    ((null item) nil)
    ((eq '= (car item))
     (list (make-basic-block (list item)
                             :outgoing (make-unconditional-edge outgoing))))
    ((eq 'if (car item))
     (destructuring-bind (c e ti fi) item
       ;; TODO Stop returning lists everywerhe.
       (let ((tb (block-from-ir-item ti :outgoing outgoing))
             (fb (block-from-ir-item fi :outgoing outgoing)))
         (append (list (make-basic-block (list c e)
                                         :outgoing (make-conditional-edge (car tb) (car fb) e)))
                 tb fb))))
    (t
     (error "U wot"))))

(defun print-block (block &optional s)
  (loop :for item :in (contents block)
        :do (format s "~a " item)))

(defun terminal-block-p (block)
  (or (null (outgoing block))
      (typecase (outgoing block)
        (unconditional-edge (null (to-block (outgoing block))))
        (conditional-edge (and (null (outgoing (true-block (outgoing block))))
                               (null (outgoing (false-block (outgoing block)))))))))

(defun print-block-graphviz (block &optional s)
  (let ((id (id block)))
    (when (not (terminal-block-p block))
      (cond
        ((typep (outgoing block) 'conditional-edge)
         ;; TODO fix lists everywhere
         (format s "~D -> ~D~%" id (id (true-block (outgoing block))))
         (format s "~D -> ~D~%" id (id (false-block (outgoing block)))))               
        (t
         (format s "~D -> ~D~%" id (id (to-block (outgoing block)))))))))

(defun print-cfg-graphviz (cfg &optional s)
  (format s "digraph {~%")
  (loop :for block :in cfg
        :for id := (id block)
        :do (format s "~D [label=<~a>]~%" id (with-output-to-string (s)
                                               (print-block block s))))
  (loop :for block :in cfg
        :for id := (id block)
        :do (print-block-graphviz block s))
  (format s "}~%"))

(defun build-cfg (ir)
  (cond
    ((null ir) '())
    ((= 1 (length ir))
     (block-from-ir-item (first ir)))
    (t
     (let* ((rest-cfg (build-cfg (rest ir)))
            (this-cfg (block-from-ir-item (first ir) :outgoing (first rest-cfg))))
       (append this-cfg rest-cfg)))))

(defun build-cfg (ir)
  (loop :for block :in ir
        :for rem := (rest ir) :then (rest rem)
        :append (block-from-ir-item block :outgoing (when rem (build-cfg rem)))))

(defun save-cfg-graphviz (cfg fname)
  (with-open-file (s fname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (print-cfg-graphviz cfg s)))
