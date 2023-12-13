(defun test-1 ()
    (let ((cl-symbol (collect_examples_by_class 'symbol-1 *examples*)))
      (terpri)
      (print "The instances in Class 1 are")
      (terpri)
      (print (get cl-symbol 1))
      (terpri)
      (print "The instances in Class 2 are")
      (terpri)
      (print (get cl-symbol 2))
      (terpri)
      (print "The instances in Class 3 are")
      (terpri)
      (print (get cl-symbol 3))
      (terpri)
      (print "The instances in Class 4 are")
      (terpri)
      (print (get cl-symbol 4))
      (terpri)
      (print "The instances in Class 5 are")
      (terpri)
      (print (get cl-symbol 5))
      (terpri)
      (print "The instances in Class 6 are")
      (terpri)
      (print (get cl-symbol 6))
      (terpri)
      (print "The instances in Class 7 are")
      (terpri)
      (print (get cl-symbol 7))
      (terpri)
      ))

;Copyright 1998 Tom M. Mitchell.  This code may be freely distributed
;and used for any non-commericial purpose, as long as this copyright
;notice is retained.  The author assumes absolutely no responsibility
;for any harm caused by bugs in the code.

(setf *print-length* nil)
(setf *print-level* nil)

;;; General utility functions
;; use (print.entity instance) to see the definition of entity 

(defun put.value (attribute instance val)
  "assign a value to an attribute of an instance"
  (setf (get instance attribute) val))

(defun get.value (attribute instance)
  "retrieve the value of attribute of instance"
  (get instance attribute))

(defun print.entity (instance)
  "print the description of instance to the screen"
  (print (symbol-plist instance)))


(defun data_setup (datafile)
(setq *examples* nil)
(setq our-input-stream (open datafile :direction :input))
(setq *attributes* (read our-input-stream))
(setq *att-class* (car (read our-input-stream)))
(setq *att-values* (read our-input-stream))
   
(loop for d in (read our-input-stream) do
      (setf *examples* (cons (first d) *examples*))
      (loop for attribute in (append *attributes* (list *att-class*))
	     as value in (cdr d)
	     do (put.value attribute (first d) value)))
(put.value 'legal.values *att-class* *att-values*)
(setq *examples* (reverse *examples*))
(close our-input-stream))


;;; Top level ID3 Decision Tree learning algorithm
;
; Tree Representation: each non-terminal tree node is a list of the form 
;  (attribute (value1 subtree1)(value2 subtree2)...)
;  where subtree-n is either a non-terminal node, or a value signifying the 
;  target value associated with that terminal node

(defun id3 (examples target.attribute attributes)
;
  "TARGET.ATTRIBUTE is the attribute to be predicted by the learned tree.
   EXAMPLES are training examples.  ATTRIBUTES is a list of attributes (not
   including TARGET.ATTRIBUTE) that may be included in the learned decision 
   tree.
   Returns: a decision tree that predicts the TARGET.ATTRIBUTE over EXAMPLES"
  (let (firstvalue a partitions)
    (setq firstvalue (get.value target.attribute (first examples)))
;    (break "in id3")
    (cond 
     ;; if every example has the same target attribute value, return it as
     ;; a leaf node
     ((every #'(lambda(e)(eq firstvalue (get.value target.attribute e)))
             examples)*att-values*
      firstvalue)
     ;; if no attributes, return the most common target attribute value
     ((null attributes)
      (most.common.value target.attribute examples))
     ;; otherwise, pick the best attribute, partition training data, and call
     ;; ID3 recursively to grow subtrees below this node
     (t
      (setq partitions
            (loop for a in attributes collect (partition a examples)))
      (setq a (choose.best.partition target.attribute partitions))
      (cons (first a)
            (loop for branch in (cdr a) collect
                  (list (first branch)
                        (id3 (cdr branch) 
                             target.attribute 
                             (remove (first a) attributes)))))))))

                  
(defun partition (attribute instances)
  "returns a partion of INSTANCES according to their values for ATTRIBUTE. 
   Returns a list (attribute (value1 e11 e12 ...)(value2 e21 e22 ...)...)"
  (let (result vlist v)
    (loop for e in instances do
          (setq v (get.value attribute e))
          (if (setq vlist (assoc v result))
            (rplacd vlist (cons e (cdr vlist)))
            (setq result (cons (list v e) result))))
    (cons attribute result)))

(defun choose.best.partition (target.attribute partitions)
  "return the partition with the highest information gain.  
   PARTITIONS is of the form ((attribute1 (val1 e11 e12 ...)(val2 e21 e22 ...)...)
                              (attribute2 (...  ...........)(...  ...  )...)).
   Note for efficiency, we compute only the expected value of the entropy of the
   partition, because this is the only term in information gain that varies from
   one attribute to another"
  (let ((lowest.exp.entropy 9999) exp.entropy best.partition)
    (loop for att.partition in partitions do
          (when (< (setq exp.entropy 
                         (expected.entropy target.attribute (cdr att.partition)))
                   lowest.exp.entropy)
            (setq lowest.exp.entropy exp.entropy)
            (setq best.partition att.partition)))
    best.partition))

(defun expected.entropy (att partition)
  "returns the sum over possible values of att of the quantity
    number.of.instances.with.this.value x sample.entropy.of.this.partition"
  (loop for p in partition sum
        (* (length (cdr p))
           (loop for v in (get.value 'legal.values att) sum
                 (let ((vcount (loop for e in (cdr p) count 
                                     (eq v (get.value att e))))
                       proportion)
                   (setq proportion (/ vcount (length (cdr p))))
;;                   (format t "p: ~S, vcount: ~d, proportion: ~S~%"
;;                           p vcount proportion)
                   (* -1.0 proportion 
                      (if (zerop proportion) 0 (log proportion 2))))))))

(defun most.common.value (attribute instances)
;  (break "in most-common-value")
  (let ((length 0) longest)
    (loop for p in (cdr (partition attribute instances)) do
          (when (> (length p) length)
            (setq length (length p))
            (setq longest p)))
    (car longest)))

(defun entropy (p)
  (+ (* -1.0 p (log p 2))
     (* -1.0 (- 1 p) (log (- 1 p) 2))))
                   

(defun print.tree (tree &optional (depth 0))
  (tab depth)
  (format t "~A~%" (first tree))
  (loop for subtree in (cdr tree) do
        (tab (+ depth 1))
        (format t "= ~A" (first subtree))
        (if (atom (second subtree))
          (format t " => ~A~%" (second subtree))
          (progn (terpri)(print.tree (second subtree) (+ depth 5))))))
(defun tab (n)
  (loop for i from 1 to n do (format t " ")))

(defun tree-match (value lst) 
  (cond ((null lst) nil)
	((equal value (caar lst))(cadar lst))
	(t (tree-match value (cdr lst)))))

(defun classify (elt tree1)
  ; elt is an instance and tree1 is a decision tree
  ; returns the classification of elt given by tree1
  ; 1 is returned as the majority class upon failure to classify
  ; failure to classify is due to a comceptual error in the original code
  (cond ((null tree1) 8)
	(t (let* ((att (car tree1))
		  (elt-value (get elt att))
		  (tree-value (tree-match elt-value (cdr tree1))))
	     (cond ((null tree-value) nil)
		   ((numberp tree-value)tree-value)
		   (t (classify elt tree-value)))))))

(defun collect_examples_by_class (cl-symbol instances)
; instances is a list of data instances from *examples*
; cl-symbol is a symbol whose properties are the classes of the
;     instances in *examples*
;     return cl-symbol with the value of each property set to
;     a list of the elements of instances whose class is that property
;YOU MUST WRITE THIS FUNCTION

; loop through instances
; add instance to corresponding class property
; adds items to cl-symbol twice if function is ran twice
    (defun collect-helper (cl-symbol instances)
    (cond ((null instances) nil)
          ((null (get cl-symbol (get (car instances) 'TYPE?))) (setf (get cl-symbol (get (car instances) 'TYPE?)) (list (car instances))))
          (t (setf (get cl-symbol (get (car instances) 'TYPE?)) (cons (car instances) (get cl-symbol (get (car instances) 'TYPE?)))))
    )
    (cond ((null instances) nil)
          (t (collect-helper cl-symbol (cdr instances)))
    )
    cl-symbol
    )
    (setf (symbol-plist cl-symbol) nil) ; clears symbol from any past properties
    (collect-helper cl-symbol instances)
  )

;(defun get_stratified_set (class-symbol percent)
; class-symbol has the class values as its properties
; returns percent of the overall dataset stratified by class value
; includes at least 1 instance from each class
; YOU MUST WRITE THIS FUNCTION  

  
 ;)

 (defun get_stratified_set (class-symbol percent)
  (process-class class-symbol percent 1))

(defun process-class (class-symbol percent class)
  (if (> class 7)
      nil
      (let* ((instances (get class-symbol class))
             (required (max 1 (floor (* percent (length instances)))))
             (selected (select-random instances required)))
        (append selected (process-class class-symbol percent (1+ class))))))

(defun select-random (list n)
  (if (or (<= n 0) (null list))
      nil
      (let ((index (random (length list))))
        (cons (nth index list) 
              (select-random (remove-nth list index) (1- n))))))

(defun remove-nth (list n)
  (if (= n 0)
      (cdr list)
      (cons (car list) (remove-nth (cdr list) (1- n)))))



  (defun testing_23 (tree testcases)
; testcases is a list of instances from *examples* to be used as testcases
; tree is a decision tree  
; returns the number of instances in testcases that are classified correctly
; YOU MUST WRITE THIS FUNCTION
  (defun test_helper (testcases count)
      (cond
        ((null testcases) count)
        (t (let* ((case (car testcases))
                  (predicted (classify case tree))
                  (actual (get case 'TYPE?)))
          (test_helper (cdr testcases) (cond ((null predicted) count) ((= predicted actual) (1+ count)) (t count)))
          )
        )
      )
  )
  (test_helper testcases 0)
)



(defun holdout_test (dataset percent class-symbol)
; dataset is the overall dataset
; percent is a decimal number between .01 and .99 specifying
;   the percent of dataset that should be used for training
; class-symbol is the symbol whose properties were established
;   by collect_examples_by_class
; the training instances should be selected randomly, must be stratified,
;   but must contain at least one instance from each class  
; return a three element list where the first element is the success rate (as
;     a decimal number) produced by testing on the instances in dataset
;     that were not used for training, the second element is the training set,
;     and the third element is the test set
; YOU MUST WRITE THIS FUNCTION
  (let* ((train (get_stratified_set class-symbol percent))
        (tree (id3 train *att-class* *attributes*))
        (test (set-difference dataset train)))
    (list (float (/ (testing_23 tree test) (list-length test))) train test))
)

  (defun repeated_holdout_test (dataset percent n class-symbol)
; dataset is the overall dataset
; percent is a decimal number between .01 and .99 specifying
;   the percent of dataset that should be used for training
; class-symbol is the symbol whose properties were established
;   by collect_examples_by_class
; n is the number of independent executions of holdout_test
; return a 2-element list where the first element is a decimal number
;     giving the average success rate over n runs and the second
;     element is a list of the success rates on each of the n
;     individual runs    
; YOU MUST WRITE THIS FUNCTION
    (defun holdout_helper (dataset percent n class-symbol average rates)
      (let* ((holdout (holdout_test dataset percent class-symbol))
              (new_avg (+ (car holdout) average))
              (new_list (append rates (list (car holdout)))))
        (cond ((= 1 n) (list new_avg new_list))
              (t (holdout_helper dataset percent (1- n) class-symbol new_avg new_list)))
    
    ))
    (let ((holdout (holdout_helper dataset percent n class-symbol 0 (list nil))))
      (list (/ (car holdout) n) (cdadr holdout)))
)




	    
 


