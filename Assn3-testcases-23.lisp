

(defun testing ()
 
   (print "These are the CS481 test cases for Assn3-23"   )
   (print "These are the CS481 test cases for Assn3-23"   )
   (terpri)
   (print  '(intersect '(426) '(2 a 30 6))    )
   (print  (intersect '(426) '(2 a 30 6))    ) 
   (print '(intersect '(5 a j 4 c) '(b k 4 j 5))   )
   (print  (intersect '(5 a j 4 c) '(b k 4 j 5))   )
   (print '(intersect '(a b r f e) '(b r f e a))   )
   (print (intersect '(a b r f e) '(b r f e a))   )
   
   (print '(remove_duplicates '(3 5 a h 6 4 a 5 7 5))   )
   (print (remove_duplicates '(3 5 a h 6 4 a 5 7 5))   )
   (print '(remove_duplicates '(7 f 4 f 7 d d f))   )
   (print (remove_duplicates '(7 f 4 f 7 d d f))   )
   (print '(remove_duplicates '(7 f 4 8 c d))   )
   (print (remove_duplicates '(7 f 4 8 c d))   )

   (print '(get_distance "septer" "venus")   )
   (print (get_distance "septer" "venus")   )
   (print '(get_distance "mars" "venus")   )
   (print (get_distance "mars" "venus")   )
   (print '(get_distance "mars" "urie")   )
   (print (get_distance "mars" "urie")   )

   (print '(get_planets_by_means "mars" 'ROCKET)   )
   (print (get_planets_by_means "mars" 'ROCKET)   )
   (print '(get_planets_by_means "mars" 'TRANSPORTER)   )
   (print (get_planets_by_means "mars" 'TRANSPORTER)   )
   (print '(get_planets_by_means "roan" 'LASER)   )
   (print (get_planets_by_means "roan" 'LASER)   )
   (print '(get_planets_by_means "bernar" 'LASER)   )
   (print (get_planets_by_means "bernar" 'LASER)   )
   (print '(get_planets_by_means "bernar" 'ROCKET)   )
   (print (get_planets_by_means "bernar" 'ROCKET)   )

   (print '(construct_action_list "baggor")   )
   (print (construct_action_list "baggor")   )
   (print '(construct_action_list "mars")   )
   (print (construct_action_list "mars")   )
   (print '(construct_action_list "bernar")   )
   (print (construct_action_list "bernar")   )
    (print '(construct_action_list "roan")   )
    (print (construct_action_list "roan")   )
    
   
   T )

