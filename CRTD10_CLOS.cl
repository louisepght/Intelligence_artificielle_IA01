;;CLOS représentation objet


(defclass figure() 
  (($nom :accessor nom :initarg :nom)
   ($couleur :accessor couleur :initarg :couleur)
   )
  )

(defclass point () 
  (($x :accessor x :initarg :x)
   ($y :accessor y :initarg :y))
  )


(defclass cercle (figure) 
  (($centre :accessor centre :initarg :centre :type point)
   ($rayon :accessor rayon :initarg :rayon :type int)
   )
  )

(defclass carre () 
  (($sommethautgauche :accessor sommethautgauche :initarg :sommethautgauche :type point)
   ($longueurW :accessor longueurW :initarg :longueurW)
   )
  )

(defclass rectangle () 
  (($sommethautgauche :accessor sommethautgauche :initarg :sommethautgauche :type point)
   ($longueurW :accessor longueurW :initarg :longueurW)
   ($hauteurH :accessor hauteurH  :initarg :hauteurH )
   )
  )


(defclass triangle () 
  (($M1 :accessor M1 :initarg :M1 :type point)
   ($M2 :accessor M2 :initarg :M2 :type point)
   ($M3 :accessor M3 :initarg :M3 :type point)
   )
  )

(defclass polygone () 
  (($sommets :accessor sommets :initarg :sommets :type list))
  )

;;créer l'origine O (0,0) 
;;créer un carré de sommethautgauche (3,4) et de coté 3

(setq $0 (make-instance 'point :x 0 :y 0))
(describe $0)

(setq $2 (make-instance 'point :x 3 :y 4))
(setq $1 (make-instance 'carre :sommethautgauche $2 :longueurW 3))
(describe $1)
(describe (sommethautgauche $1))

;;;;;;;;;;;;;;;;; DUPLICATE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod Translate ((p point) dx dy)
  (let ((tp (make-instance 'point :x (+(x p)dx) :y (+(y p)dy))))
    (return-from Translate tp)
    )
)
(setq tp (Translate $0 1 2))
(describe tp)
;;;;;;;;;;;;;;;;;;;;;;;;Translate Point;;;;;;;;;;;;;;;;;
(defmethod Translate ((p point) dx dy)
  (setf (x p)(+ (x p) dx))
  (setf (y p)(+ (y p) dy))
  )
(setq tp (Translate $0 1 2))
(describe tp)
(describe $0)
;;;;;;;;;;;;;;;;;;;;Translate Carre;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod Translate ((c carre) dx dy)
  (Translate (sommethautgauche c) dx dy)
  )
;;;;;;;;;;;;;;;;;;;;Duplicate Carre;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod Translate ((p carre) dx dy)
  (let ((tp (make-instance 'carre :sommethautgauche (Translate (sommethautgauche p)  dx dy) :longueurW (longueurW p))))
  (return-from Translate tp)
  ))
(setq carret (Translate $1 2 3))
(describe carret)
;;;;;;;;;;;;;;;;;;;;;;;  SYMX   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod Symx ((c carre))
    (Translate c 0 (- 0 (- (*(y (sommethautgauche c))2)(longueurW c))))
)
(setq sym (Symx $6))
(describe sym)


(setq $5 (make-instance 'point :x 3 :y 4))
(setq $6 (make-instance 'carre :sommethautgauche $5 :longueurW 3))
(describe $6)
(describe (sommethautgauche $6))

;;;;;;;;;;;;;;;;; SYMY;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod Symy ((c carre))
    (Translate c (- 0 (+ (*(x (sommethautgauche c))2)(longueurW c))) 0)
  )
(setq sym (Symy $6))
(describe sym)

;;;;;;;;;;;;;;;; SYMO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod SymO ((c carre))
  (Translate c (- 0 (+ (*(x (sommethautgauche c))2)(longueurW c)))(- 0 (- (*(y (sommethautgauche c))2)(longueurW c))))
  )
(setq sym (SymO $6)) 




    





