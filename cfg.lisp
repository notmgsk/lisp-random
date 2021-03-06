;; TODO Wow. Make it sensible. Differentiate between a "basic" block
;; and a "not basic" block.
;;
;; TODO Add other control structures (e.g. when, while) implemented in
;; terms of a general structure (e.g. if?)
;;
;; TODO Collect successive basic blocks into a new block type, which
;; consists of basic blocks and possibly ends in a control-type block.

(defpackage :lisp-random.cfg
  (:use :common-lisp))

(in-package :lisp-random.cfg)

(defvar *input* nil
  "The input language/IR")

(setq *input* '((if e1
                    (if e2
                        (= s1)
                        (= s2))
                    (if e3
                        (if e4
                            (= s3))
                        (= s4)))
                (= s5)))

(defun make-random-cfg (max-nest max-depth)
  (let* ((if-prob 0.4)
         (if-single-prob 0.25)
         (stop-prob 0.1))
    (labels ((make-if (e)
               (decf max-nest)
               (prog1
                   (list 'if
                         e
                         (build-random-ir)
                         (build-random-ir))
                 (incf max-nest)))
             (make-if-single (e)
               (decf max-nest)
               (prog1 
                   (list 'if e (build-random-ir))
                 (incf max-nest)))
             (make-= (s1)
               (decf max-depth)
               (list '= s1))
             (build-random-ir ()
               (if (and (not (zerop max-nest))
                        (< (random 1.0) if-prob))
                   (if (< (random 1.0) if-single-prob)
                       (make-if-single (string (gensym "e")))
                       (make-if (string (gensym "e"))))
                   (make-= (string (gensym "s"))))))
      (build-cfg
       (loop :while (< stop-prob (random 1.0))
             :while (not (zerop max-depth))
             :append (list (build-random-ir)))))))

(defclass edge () ())

(defclass conditional-edge (edge)
  ((true-block :initarg :true-block :accessor true-block)
   (false-block :initarg :false-block :accessor false-block)
   (conditional :initarg :condition :accessor conditional)))

(defun make-conditional-edge (true-block false-block conditional)
  (make-instance 'conditional-edge
                 :true-block true-block
                 :false-block false-block
                 :conditional conditional))

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

;; TODO ???
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
    ((eq 'if (car item))
     (destructuring-bind (c e ti &optional fi) item
       ;; TODO Stop returning lists everywerhe.
       (let ((tb (block-from-ir-item ti :outgoing outgoing))
             (fb (block-from-ir-item fi :outgoing outgoing)))
         (append (list (make-basic-block (list c e)
                                         :outgoing
                                         (make-conditional-edge (car tb) (if fb (car fb) outgoing) e)))
                 tb fb))))
    (t
     (list (make-basic-block (list item)
                             :outgoing (make-unconditional-edge outgoing))))))

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
         (when (true-block (outgoing block))
           (format s "~D -> ~D [label=true] ~%" id (id (true-block (outgoing block)))))
         (when (false-block (outgoing block))
           (format s "~D -> ~D [label=false] ~%" id (id (false-block (outgoing block))))))               
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

(defun save-cfg-graphviz (cfg fname)
  (with-open-file (s fname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (print-cfg-graphviz cfg s)))
