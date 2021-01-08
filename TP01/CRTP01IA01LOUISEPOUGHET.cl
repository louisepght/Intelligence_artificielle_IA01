;;;TP01 Louise Poughet TD1

;;;EXERCICE 1
;;;retourne la liste constituée des trois arguments dans l'ordre inverse 
(defun reverseA (arg1 arg2 arg3) 
  (list arg3 arg2 arg1))
;;;test de la fonction
(reverseA 1 2 3)


;;;retourne la liste inversée de 1,2 ou 3 éléments.
(defun reverseB(L)
  (cond ((= (length L) 3) (reverseA (car L)(cadr L)(caddr L)))
         ((= (length L) 2) (list (cadr L) (car L)))
        (= (length L) 1) L)) 
   )
  )
;;;test
(reverseB '(1 2))
(reverseB '(1))
(reverseB '(1 2 3))



;;;retourne la liste dont les atomes ont été doublés.
(defun double(L)
  (setq newlist (list))
  (dolist (x L)
    (push x newlist)
    (if (atom x) (push x newlist)
      )
    )
    (reverse newlist)
)
;;;test
(double '((1 2) 3 (4 5) 6))




;;;retourne BRAVO si les 3 premiers éléments de la liste L sont des nombres sinon PERDU
(defun nombres3(L) 
  (if (and (numberp(car L)) (numberp (cadr L)) (numberp (caddr L))) 
      (print "BRAVO")
    (print "PERDU")
    )
  )

;;;test
(nombres3 '( 1 2 3 R S 4)) ;;;BRAVO
(nombres3 '( 1 2 E R S 4)) ;;;PERDU




;;;retourne la liste inversée de L
(defun monreverse (L) 
  (if (=(length L) 1)
  	L
    (append (monReverse (cdr L)) (list (car L)))
    )
)
;;;test
(monreverse '( a b (c d) e f))



;;;retourne vrai si L est un palindrome
(defun palindrome(L) 
  (if (EQUAL (monreverse L) L) (print"T") nil))
(palindrome '(x a m a x)) ;;;T
(palindrome '(x a m a s)) ;;;nil


;;;retourne la liste composée des éléments sucessifs des deux listew passées en arguments.
(defun grouper (L M)
  (if (and (not (null L)) (not (null M))) 
      (cons (list (car L) (car M)) (grouper (cdr L)(cdr M)))
    )
)
(grouper '(1 2 3) '(4 5 6))








;;;EXERCICE 2
;;;retourne la liste des couples composés des éléments de la liste fournie en paramètre et  de leur triple.
(defun list-triple-couple (L)
    (grouper L (mapcar #'(lambda(L) (* L 3)) L))
  )
(list-triple-couple '(0 2 3 11))







;;;EXERCICE 3
;;;retourne la valeur d'une clé dans une liste d'association et nil si la personne n'existe pas 
(defun myassoc(cle a-list)
  (dolist (e a-list)
    (if (equal cle (car e))
        (return e)
      ))
  )
;;;test
(myassoc 'Pierre '((Yolande 25) (Pierre 22) (Julie 45)))
(myassoc 'Yves '((Yolande 25) (Pierre 22) (Julie 45)))



;;;retourne la liste des clés d'une A-liste
(defun cles(a-list)
  (mapcar #' car a-list
    )
  )
 ;;;test
(cles '((Yolande 25) (Pierre 22) (Julie 45)))


;;;retourne une A-liste à partir d'une liste de clés et d'une liste de valuers. 
(defun creation (listeCles listeValeurs)
  (grouper listeCles listeValeurs)
  )
;;;test 
(creation '(Yolande Pierre Julie) '(25 22 45))
(creation '(Yolande Pierre Julie) '(25 22 45 33))








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;EXERCICE 4 
;;;fonction qui retourne le nom de la tombe
(defun nom(tombe)
  (car tombe))
;;;test
(nom '("Bécaud" 2001 (45 17) 2000 30))


;;;fonction qui retourne l'année d'inhumation 
(defun an-inhum(tombe) 
  (cadr tombe)
  )
;;;test
(an-inhum '("Bécaud" 2001 (45 17) 2000 30))

;;;fonction retournant l'emplacement de la tombe num = 17
(defun num(tombe)
  (cadr (caddr tombe))
  )
;;;test
(num '("Bécaud" 2001 (45 17) 2000 30))


;;;fonction retournant la rangée de la tombe rangée = 45
(defun rangee(tombe)
  (car (caddr tombe))
  )
;;;test
(rangee '("Bécaud" 2001 (45 17) 2000 30))


;;;fonction retournant l'année de début de location
(defun debut-loc(tombe)
  (cadddr tombe)   ;;;renvoie le 4ième élément de la liste passée en argument
  )
;;;test
(debut-loc '("Bécaud" 2001 (45 17) 2000 30))


;;;fonction retournant la durée de location de la tombe
(defun duree-loc(tombe)
  (cadr(cdddr tombe))
  )
;;;test
(duree-loc '("Bécaud" 2001 (45 17) 2000 30))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;question 2 

;;;;définition de la base : 
(setq Base '((pere-lachaise("Bécaud" 2001 (45 17) 2000 30) 
                            ("Desproges" 1988 (11 6) 1988 30)
                            ("Grappelli" 1997 (85 23) 1997 5) 
                            ("Morrison" 1971 (6 12) 1971 30)
                            ("Mouloudji" 1994 (42 9) 1990 15) 
                            ("Nohain" 1981 (89 14) 1979 15)
                            ("Oussekine" 1986 (85 37) 1986 5) 
                            ("Petrucciani" 1999 (11 26) 1999 15)
                            ("Popesco" 1993 (85 16) 1985 30) 
                            ("Signoret" 1985 (44 7) 1980 30)
                            ("Zavatta" 1993 (11 16) 1993 15)
                            ("Desproges" 2018 (11 6) 1988 30)
                            )
             (paris ("Bulag" 2009 (50 17) 2000 30) ("Dupuy" 1988 (11 6) 1988 30))
             (londres("wilson" 2003 (50 30) 2001 40))
             )
      )



;;définition d'une fonction trouver-cim, retoune la liste des tombes pour un cimetière donné
(defun trouver-cim (nomcim base)
  (dolist (cim base)
    (if (equal (car cim) nomcim) (return (cdr cim)) NIL
      )
    )
  )
;;test 
(trouver-cim 'pere-lachaise Base)


;;;retourne le nom de la personne qui est enterrée à une certaine localisation et dans un certain cimetière
;;;et "emplacement non attribué" si la localisation n'est attribué à personne.
(defun qui-est-la (local nomcim cim)
  (setq listtombe (trouver-cim nomcim cim))
  (dolist (tombe listtombe)
    (if (and (equal (car local)(rangee tombe)) 
             (equal (cadr local)(num tombe)))
        (return (nom tombe)) 
      '"emplacement non attribué"
          )
    )
)
;;;test 
(qui-est-la '(11 16) 'pere-lachaise base) ;;;retourne Zavatta


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;question 3

(defun prevoyant? (tombe) 
  (if (< (debut-loc tombe) (an-inhum tombe) )
      T
    NIL
    )
  )
(prevoyant? '("Zavatta" 1993 (11 16) 1993 15)) ;;;NIL
(prevoyant? '("Zavatta" 1993 (11 16) 1991 15)) ;;;T
(prevoyant? '("Grappelli" 1997 (3 10) 1997 5)) ;;;NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;question 4

(defun nb-prevoyants(nomcim cim)
  (setq tombes (trouver-cim nomcim cim))
   (setq nombre 0) ;on définit un compteur
  (dolist (tombe tombes)
    (if (eq (prevoyant? tombe) T)
           (setq nombre (+ 1 nombre)) ;on incrémente le compteur
      )
    )
  nombre
)

(nb-prevoyants 'pere-lachaise Base)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;question 5

(defun annuaire(nomcim nrang cim)
  (let ((liste (list)) (tombes (trouver-cim nomcim cim))) ;création d'une liste
    (dolist (tombe tombes)  
      (if (equal nrang (rangee tombe))
          (push (nom tombe) liste)                               ;ajout de la tombe à la liste
        )                             
      )
    (reverse liste)
    )
  )
;;;test 
(annuaire 'pere-lachaise 85 Base)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;question 6 
;;jeu de variable, recherche de doyen et benjamin en parallèle : pour ne parcourir la liste qu'une seule fois
;;retoune une liste avec le doyen et le benjamin du cimetière 

(defun doyen-benjamin(nomcim cim)
  (let ((tombes (trouver-cim nomcim cim))
        (listd (list))(listb (list))
        (date 0)
        (doyen 2020)(benj 0)
        ) ;;;ok
    (dolist (tombe tombes)
      (setq date (an-inhum tombe))
      (cond
       ((and (> date benj)(< date doyen))
        (progn (setq benj date)
          (setq doyen date)
          ))
       ((> date benj)(progn (setq benj date) 
                       (setq listb tombe)))
        ((< date doyen)(progn (setq doyen date)(setq listd tombe))
       )
       )
      )
    (append listd listb)
    
    )
  )
;;; boucle infinie
;;test 
(doyen-benjamin 'pere-lachaise Base)
  

  
  
  





      










