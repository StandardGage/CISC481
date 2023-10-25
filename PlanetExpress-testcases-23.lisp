

(defun testing ()
  (defun test (planet-1 planet-2 weight)
  (PlanetExpress planet-1 planet-2 weight
		 'goal_test_PE? 'get_successors_PE
		 'get_estimate_cost_to_goal_PE))

(defun test_A ()
					; very easy one action test
  (terpri)
  (terpri)
  (print 'Test-1)
  (terpri)
  (print '(test "crent" "durant" 5))
  (terpri)
  (print (test "crent" "durant" 5))
  (terpri)
  (print '********************************************************************)
  
  )
  
(defun test_B ()
(terpri)
(terpri)
(print 'Test-2)
(terpri)
 ; easy one action test
  (print '(test "phoe" "brattle" 6))
  (terpri)
  (print (test "phoe" "brattle" 6))
  (terpri)
  (print '********************************************************************)
  
  )

(defun test_C ()
  ; easy one action test
(terpri)
(terpri)
(print 'Test-3)
(terpri)
(print '(test "bernar" "durant" 2))
(terpri)
(print (test "bernar" "durant" 2))
(terpri)
  (print '********************************************************************) 
)
  
(defun test_D ()
  (terpri)
(terpri)
(print 'Test-4)
(terpri)
  ; two action test where first successor of bangor is not the one that is best
(print '(test "baggor" "durant" 7))
(terpri)
(print (test "baggor" "durant" 7))
(terpri)
  (print '********************************************************************) 
)

(defun test_E ()
  (terpri)
(terpri)
(print 'Test-5)
(terpri)
  ; one action test where there are 2 means to get to septer
  ; and Rocket is better than Transporter
(print '(test "mars" "septer" 7))
(terpri)
(print (test "mars" "septer" 7))
(terpri)
  (print '********************************************************************) 
)

(defun test_F ()
  (terpri)
(terpri)
(print 'Test-6)
(terpri)
  ; in this test, a better path is found to a node on the open list
(print '(test "phoe" "urie" 25))
(terpri)
 (print (test "phoe" "urie" 25))
(terpri)
  (print '********************************************************************) 
)

(defun test_G ()
  (terpri)
(terpri)
(print 'Test-7)
(terpri)
; longer problem
(print '(test "phoe" "venus" 25))
(terpri)
(print (test "phoe" "venus" 25))
(terpri)
  (print '********************************************************************) 
)

(defun test_H ()
  (terpri)
(terpri)
(print 'Test-8)
(terpri)
(print '(test "roan" "sumver" 2))
(terpri)
(print (test "roan" "sumver" 2))
(terpri)
  (print '********************************************************************) 
)

(defun test_I ()
  (terpri)
(terpri)
(print 'Test-9)
(terpri)
  ; path of length 4
  ; finds a better path to node on open list several times
(print '(test "phoe" "delve" 1000))
(terpri)
(print (test "phoe" "delve" 1000))
(terpri)
  (print '********************************************************************) 
)

  ;(setf path (make-pathname :name "myoutput"))
 ; (setf output481 (open path :direction :output :if-exists :supersede))
   (print "These are the CS481 test cases for PlanetExpress project")
   (print "These are the CS481 test cases for PlanetExpress project")
   (terpri)
   (print (test_A))
   (print (test_B))
   (print (test_C))
   (print (test_D))
   (print (test_E))
   (print (test_F))
   (print (test_G))
   (print (test_H))
   (print (test_I))
   (print 'Done )
				  
   )
 (defun Planet-tests  ()
(with-open-file (*standard-output*
                 "output_PlanetExpress"
                 :direction :output
                 :if-exists :supersede)
     (testing)))

