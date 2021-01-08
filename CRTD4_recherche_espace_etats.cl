;Recherche dans un espace d'état 

(setq *lab* '((E 1)(1 E 2)(2 7)(3 6)(4 5)(5 12 4)(6 7 3)
              (7 8 6 2)(8 9 7)(9 10 8)(10 15 11 9)(11 10 12 14 )
              (12 11 5)(13 20)(14 11)(15 10 16)(16 15 17)(17 18 16)
              (18 19 17)(19 20 18)(20 13 19 S)(S 20)))



(defun successeur (etat lab) 
  (cdr(assoc etat lab)
       )
  )

(successeur 'E *lab*)

(defun diff (a b)
  (let ((liste(list)))
    (dolist (x a) 
      (if (not(member x b))
          (push x liste)
        )
      )
    liste ;;(return liste) erreur not inside a block named nil, pourquoi?
    )      
  )


(diff '(a b c) '(b c)) ;;=> (A)

(defun avance(etat sortie parcours lab)
     (let ((succ(diff (successeur etat lab) parcours)))
       (cond 
        ((null etat) nil)
        ((eq etat 's)(print "sortie")(print(reverse parcours)))
        (succ(dolist(e succ)
            (if (not(member e parcours))
                (avance e sortie (push e parcours) lab)))
           )
        (t "echec")
        )
       parcours
     )
    )

(avance 'E 'S '(E) *lab*)

;;renvoie (1 E) en plus, je ne sais pas pourquoi.


;;;;;;;;;;;;;;;;;;;;;;;;;en largeur;;;;;;;;;;;;;;;;;;;;;;;;
(defun rech-larg1(candidate-list old-states) 
  (print "**candidats**")
  (print candidate-list)
  (cond 
   ((null candidate-list) nil) ;;aucun candidat
   ((member 'S candidate-list)(return-from rech-larg1 "Succès")) ;; si la sortie fait partie des candidat, on a trouvé la sortie
   (t (rech-larg1(diff (flatten ;;on a des candidats successeurs, on relance la recherche en largeur avec toute la liste des succeusseurs des candidats.
                        (mapcar (lambda (xx)(successeurs xx laby)) candidate-list))
                       old-states)
                  (append candidate-list old-states)))))
;;flatten = mets à plat une liste.

(defun rech-larg (etat)
  (rech-larg1(list etat) nil))

(defun diff(L M)
  ;;différence ensembliste
  (cond ((null L) nil)
        ((member (car L) M :test 'equal)(diff (cdr L) M))
        (t (cons (car L) (diff (cdr L) M)))))

(defun flatten (L)
  (if L (append (car L)(flatten (cdr L)))))
;;s'il y a qqchose dans la liste, on fait une fusion du premier élément de la liste avec la liste
;;mettre à plat la liste : enlever tous les niveaux dans la liste.

(rech-larg 'e)










