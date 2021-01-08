;;;TD5 Louise Poughet
;;système expert
;;;Base de règles 

B et D et E =>F
R2 D et G => A
R3 C et F => A
R4 C => D
R5 D => E
R6 A => H
R7 B => X
R8 X et C => A


(setq baseregle '(((B D E)F)
                  ((D G)A)
                  ((C F)A)
                  ((C) D)
                  ((D)E)
                  ((A)H)
                  ((B)X)
                  ((X C)A) 
                  ))

(setq basefait '(B))

;;;retourne les premisses d'une règle
(defun premisses (r)
  (car r)
  )
(premisses '((B D E)F))

;;; retourne les conclusions d'une règle
(defun conclusion (r)
  (car(cdr r))
  )
(conclusion '((B D E)F))

;;;première version : question test si f est vrai 
(defun question (f)
  (format t "Est ce que ~S ~% vrai T ou faux NIL?" f)
  (setq res (read))
  (if (eq res T) T NIL
    )
  )

;;;deuxième version : on sépare la question du test :

(defun question (f)
  (format t "Est ce que ~S ~% vrai T ou faux NIL?" f)
  (princ ">")
  (setq res (read))
  (if (vrai? res) (push f basefait))
  
  )

(question "Orange est une couleur?")

;;;retourne T si f est vrai NIL sinon.
(defun vrai? (res) 
  (if (eq res T) 
    T
    NIL
    ) 
  )
 
;;;(regles-candidates f base) retourne la liste des règles dont la conclusion est f. 
(defun regles-candidates (f base) 
  (let (list-regles (list))
    (dolist (regle base)
      (if (eq f (conclusion regle))
        (push regle list-regles)
      )
      )
    list-regles
    )
  )
(regles-candidates 'A baseregle)



;;;;;;;;;;;;;;;;;;;;;;ne sert pas;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun recherche-premisses (RC)
  (let ((prem (list)))
    (dolist (item rc) 
      (push (premisses item) prem) 
      )
    (reverse prem)
    )
  )
(recherche-premisses (regles-candidates 'A baseregle))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun appartenanceBF (but basefait) 
  (if (member but basefait) T NIL
    )
  )

(appartenanceBF 'B basefait) ;;T
(appartenanceBF 'D basefait) ;;NIL

;;;Fonction chemin (base-fait, but, base-regle) 




(defun verifier (base-fait but base-regle) 
  (let  ((arret nil))
    (if (appartenanceBF but base-fait) 
        
        
      (setq arret T)
    
      
    (let ((rc (regles-candidates but base-regle)))
      (print rc)
      (if (null rc) (progn (print "question")(question but))
        ((dolist (r rc)   
          (print r)
          (setq arret (verifierET base-fait r base-regle))
          (if (equal arret T)(return))
          )
         (if (not arret) (setq arret (question but))))
      ))    
    
    
    )
    (return-from verifier arret)
    )
  )




(defun verifierET(base-fait r base-regle) 
  (let ((arret T)(prem (premisses r)))
    (if (prem)
    (dolist (p prem) 
      (setq arret (verifier base-fait p base-regle))
      
      (if (eq arret NIL)(return))
      
      )
      )
    arret
    )
  )

(premisses '(((X C) A) ((C F) A) ((D G) A)))



(verifier basefait 'z baseregle)
;;on remarque qu'on peut donner un but qui n'est pas dans la base de règles.

(verifier basefait 'A baseregle)
(verifier basefait 'C baseregle)
        
;;;;;comportement complètement instable














