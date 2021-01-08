;;Louise Poughet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *etats* '((1 1 1 1)(1 1 -1 1)(1 -1 1 1)(1 -1 1 -1)
                (-1 -1 -1 -1)(-1 -1 -1 1)(-1 1 -1 1)
                (-1 -1 1 -1)(-1 1 -1 -1)(1 1 1 -1)
                ))            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *actions* '((2 0 0 0) (2 0 2 0) (2 0 0 2) (2 0 0 0) (2 2 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *etatsnonvalide* '((-1 1 1 -1)(-1 1 1 1)(-1 -1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *predecesseur* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun successeurs(etat) 
  (let ((succ (list))(res nil))
        (dolist (a *actions*)
          (setq res (calcul etat a))
          (if (member res *etats* :test #'equal)
              (progn (if (not(member res succ :test #'equal))(push res succ))
                (if (not(member (list res etat) *predecesseur* :test #'equal))
                                         (push (list res etat) *predecesseur*))
            )
          )
          )
    (return-from successeurs (reverse succ))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calcul(etat a)
  (if (equal (car etat) 1) 
      (list(-(car etat)(car a))(-(cadr etat)(cadr a))(-(caddr etat)(caddr a))(-(cadddr etat)(cadddr a)))
    (list(+(car etat)(car a))(+(cadr etat)(cadr a))(+(caddr etat)(caddr a))(+(cadddr etat)(cadddr a)))

    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rech-prof(etat etat-visite)
  (if (equal etat '(-1 -1 -1 -1)) (progn (push etat etat-visite) (print(reverse etat-visite))))
    (push etat etat-visite) 
  (dolist (fils (successeurs etat))
    (if (member fils etat-visite :test #'equal)
        nil
        (progn 
          (rech-prof fils etat-visite)
          )
      )
    )
  )

;;;meilleur affichage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rech-prof(etat etat-visite)
  (if (equal etat '(-1 -1 -1 -1)) (progn (push etat etat-visite) (afficher (reverse etat-visite))))
    (push etat etat-visite) 
  (dolist (fils (successeurs etat))
    (if (member fils etat-visite :test #'equal)
        nil
        (progn 
          (rech-prof fils etat-visite)
          )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(rech-prof '(1 1 1 1) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun afficher (liste)
    (dolist (x liste)
      (format t "~s~%" x)
      )
  (format t "~%~%")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(defun rech-larg1 (candidate-list old-states)
  (if (null candidate-list) nil 
    (progn
  (if (member '(-1 -1 -1 -1) candidate-list :test #'equal)
      (afficher (reverse (append '((-1 -1 -1 -1)) old-states))))
      
   (rech-larg1 (diff (flatten
                      (mapcar (lambda (xx)(successeurs xx)) candidate-list))
                      old-states)
               (append candidate-list old-states)))
      )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recherche-chemin(chemin etat ei)
  (if (not(member etat chemin :test #'equal))
      (progn (push etat chemin)
      (if (equal etat ei)
          (print chemin)
        (progn 
          (dolist (element *predecesseur*) 
            (if (equal (car element) etat)
                (recherche-chemin chemin (cadr element) ei))
            )
          )
        )
        )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *predecesseur* '((B A) (C A)(D B) (E B) (E C) (F C)(K E)))
(recherche-chemin nil 'K 'A)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
            
(defun flatten (L) 
  (if L (append (car L) (flatten(cdr L)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun diff (L M)
  (cond ((null L) nil)
        ((member (car L) M :test 'equal) (diff (cdr L) M))
        (t  (cons (car L) (diff (cdr L) M))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun rech-larg (etat) 
  (setq *predecesseur* nil)
  (rech-larg1 (list etat) nil)
  (recherche-chemin nil '(-1 -1 -1 -1) '(1 1 1 1))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(rech-larg '(1 1 1 1))


