 
; -------------------------------------------------------------------
;
; Database Management
;
; -------------------------------------------------------------------

(defun database_setup ()
  (defun assign-properties  (carrier-list coordinates)
    (cond((and(null carrier-list) (null coordinates))nil)
	 ((null carrier-list)
	  (setf (get (intern (caar coordinates)) 'x-coord) (cadar coordinates))
	  (setf (get (intern (caar coordinates)) 'y-coord) (caddar coordinates))
	  (setf (get (intern (caar coordinates)) 'z-coord) (car(cdddar coordinates)))
	  (assign-properties carrier-list (cdr coordinates)))
           
         (t (setf (get (intern (cadar carrier-list)) 'next-planet)
		  (append (cond ((equal (caar carrier-list) 'ROCKET)
				 (mapcar #'(lambda(x) (list x 'ROCKET))
					 (caddar carrier-list)))
				((equal (caar carrier-list) 'TRANSPORTER)
				 (mapcar #'(lambda(x) (list x 'TRANSPORTER))
					 (caddar carrier-list)))
				((equal (caar carrier-list) 'LASER)
				 (mapcar #'(lambda(x) (list x 'LASER))
					 (caddar carrier-list))))
				(get (intern (cadar carrier-list)) 'next-planet)))
	    (assign-properties (cdr carrier-list) coordinates))))
          
            
  (let((coordinates
	'(("baggor" 10 20 30)
	  ("crent"  20 40 50)
	  ("erant"  30 60 100)
	  ("durant" 35 60 110)
	  ("mars" 0 1500 2000)
	  ("septer" 150 1020 4050)
	  ("venus" 300 5000 400)
	  ("centra" 360 1260 6000)
	  ("phoe" 50 20 1)
	  ("milnet" 51 21 2)
	  ("picket" 900 3000 9020)
	  ("pratt" 52 22 2)
	  ("brattle" 70 30 100)
	  ("urie" 1500 150 100)
	  ("wheger" 1500 750 260)
	  ("partin" 1770 300 4030)
	  ("aria" 1890 1280 3000)
	  ("threm" 2100 120 1010)
	  ("morph" 1800 450 600)
	  ("delve" 2100 750 3040)
	  ("sumver" 2250  1050 500)
	  ("roan" 2340 1020 400)
	  ("westal" 2370 60 70)
	  ("mialk" 2400 0 7000)
	  ("pitts" 2400 1200 4030)
	  ("ithmus" 2610 1260 3020)
	  ("baltic" 2640 990 403)
	  ("newth" 2670 1020 506)
	  ("imia" 2700 1050 403)
	  ("bartwal" 2850 1290 4000)
          ("bernar"  1300 303 200)
	  ("plait" 3000 1440 3070)))

       (carriers
	'(
	  (ROCKET "mars" ("venus" "septer" "aria"))
	  (TRANSPORTER "mars" ("septer" "pitts" "pratt"))
	  (ROCKET "phoe" ("milnet"))
	  (TRANSPORTER "phoe" ("brattle"))
	  (ROCKET "milnet" ("pratt"))
	  (TRANSPORTER "pratt" ("urie"))
          (ROCKET "brattle" ("urie"))
	  (ROCKET "urie" ("baggor" "venus"))
	  (TRANSPORTER "urie" ("venus"))
	  (ROCKET "baggor" ("crent" "erant"))
          (TRANSPORTER "erant" ("mars" "venus"))
	  (TRANSPORTER "crent" ("durant"))
	  (ROCKET "venus" ("sumver" "delve"))
	  (ROCKET "roan" ("westal" "baltic" "aria"))
	  (TRANSPORTER "roan" ("baltic" "sumver"))
	  (LASER "roan" ("venus"))
	  (ROCKET "bernar" ("baltic" "venus"))
	  (LASER "bernar" ("crent" "durant"))
	  
	  )))
        
     (assign-properties  carriers coordinates)))

