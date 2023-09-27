
; problem 1
(defun intersect (l1 l2)
  (cond ((null l1) nil)
        ((member (car l1) l2) ; If the first element of l1 is in l2
         (cons (car l1) (intersect (cdr l1) l2))) ; Add it to the result
        (t (intersect (cdr l1) l2)))) ; continue with the rest of l1

; problem 2
(defun remove_duplicates (lst)
  (cond ((null lst) nil)
        ((member (car lst) (cdr lst)) ; If the first element is duplicated in the rest of the list.
         (remove_duplicates (cdr lst))) ; Skip the first element and continue with the rest.
        (t (cons (car lst) (remove_duplicates (cdr lst))))) ; Otherwise, keep the first element and continue with the rest.
)

; space problems

; problem 1
(defun get_distance (planet1 planet2)
  (let* ((planet1-symbol (intern planet1))
         (planet2-symbol (intern planet2))
         (x1 (get planet1-symbol 'x-coord))
         (y1 (get planet1-symbol 'y-coord))
         (z1 (get planet1-symbol 'z-coord))
         (x2 (get planet2-symbol 'x-coord))
         (y2 (get planet2-symbol 'y-coord))
         (z2 (get planet2-symbol 'z-coord))
         (distance (round (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2) (expt (- z2 z1) 2))))))
    distance))


; problem 2
(defun get_planets_by_means (planet means-of-travel &optional (next-planet-list (get (intern planet) 'next-planet)))
    (cond ((null next-planet-list) nil)
          ((eq (cadr (car next-planet-list)) means-of-travel) (cons (caar next-planet-list) (get_planets_by_means planet means-of-travel (cdr next-planet-list))))
          (t (get_planets_by_means planet means-of-travel (cdr next-planet-list))))
)

; problem 3
(defun construct_action_list (planet &optional (next-planet-list (get (intern planet) 'next-planet)))
  (cond
    ((null next-planet-list) nil) ; Base case: if the list is empty, return nil
    ; Create an action tuple for the first planet in the next-planet list
    (t (cons `(,(get_distance planet (caar next-planet-list)) ,planet ,(caar next-planet-list) ,(cadar next-planet-list)) 
             (construct_action_list planet (cdr next-planet-list))))
  )
)

