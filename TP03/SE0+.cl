;;Louise Poughet 
;;TP03 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; base de donnees ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;la base de donnee est une fonctionnalité en plus pour déduire directement s il existe des 
;; circonstances aggravantes : si le nom du prevenu est connu, alors il y a circonstance aggravantes, 
;; sinon il faut demander à l'utilisateur. 
(setq *basedonnees* '(Masson))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; constantes      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *const_procede* '(faux_nom 
                        fausse_qualite 
                        abus_de_qualite 
                        mise_en_scene 
                        utilisation_document
                        intervention_d_un_tiers
                        mensonge boniment
                        utilisation_violence
                        menace_violence))
(setq *const_but* '(obtention_decharge 
                    obtention_engagement 
                    obtention_argent 
                    remise_bien 
                    obtention_signature 
                    revelation_secret 
                    renonciation))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; base de regles    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *baseregle*'(
        (((majorite nil)) ((emprisonnement non_traite) (amende non_traite)))
        (((majorite oui)(procede faux_nom)) (tromperie oui))
        (((majorite oui)(procede fausse_qualite))(tromperie oui))
        (((majorite oui)(procede abus_de_qualite))(tromperie oui))
        (((majorite oui)(procede mise_en_scene))(tromperie oui))
        (((majorite oui)(procede utilisation_document))(tromperie oui))
        (((majorite oui)(procede intervention_d_un_tiers))(tromperie oui))
        (((majorite oui)(procede mensonge))(tromperie nil))
        (((majorite oui)(procede boniment))(tromperie nil))
        (((tromperie oui)(but obtention_decharge))(delit escroquerie))
        (((tromperie oui)(but obtention_engagement))(delit escroquerie))
        (((tromperie oui)(but obtention_argent))(delit escroquerie))
        (((tromperie oui)(but remise_bien))(delit escroquerie))
        (((tromperie NIL))((emprisonnement NIL)(amende NIL)))
        (((majorite oui)(procede utilisation_violence))(violence oui))
        (((majorite oui)(procede menace_violence))(violence oui))
        (((violence oui)(but obtention_argent))(delit extorsion))
        (((violence oui)(but remise_bien))(delit extorsion))
        (((violence oui)(but obtention_engagement))(delit extorsion))
        (((violence oui)(but obtention_signature))(delit extorsion))
        (((violence oui)(but revelation_secret))(delit extorsion))
        (((violence oui)(but renonciation))(delit extorsion))
        (((delit escroquerie)(circonstancesaggravantes NIL)) ((emprisonnement 5)(amende 375000)))
        (((delit escroquerie)(circonstancesaggravantes oui)) ((emprisonnement 7)(amende 750000)))
        (((delit extorsion)(circonstancesaggravantes NIL)) ((emprisonnement 7)(amende 100000)))
        (((delit extorsion)(circonstancesaggravantes oui)) ((emprisonnement 10)(amende 100000)))
        )
    )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Fonctions de service;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;premisses;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;retourne les prémisses d'une règle

(defun premisses (r)
  (car r)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;conclusion;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;retourne la conclusion d'une règle

(defun conclusion (r)
  (car(cdr r))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; regle_applicable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;renvoie un booléen : t si la règle est applicable NIL sinon

(defun regle_applicable (regle)
  ;;si une premisse n'appartient pas à la base de fait alors la règle n'est pas applicable
  (let ((regleapplicable? T))
    (dolist (p (premisses regle))
    (if (not (member p *basefait* :test #'equal))
        (setq regleapplicable? NIL))
      )
    (return-from regle_applicable regleapplicable?)
    )
)
;;test 
(regle_applicable '(((majorite oui)(procede boniment))(tromperie nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; push_regles_applicable_basefait ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;ajoute les conséquences des regles applicables à la base de règle.
(defun push_regles_applicable_basefait (*baseregle* nom)
  (dolist (r *baseregle*)
    (if (eq (regle_applicable r) T) 
          (push (conclusion r) *basefait*)
      )
    )
  (return-from push_regles_applicable_basefait (reverse *basefait*))
  )
;;test
(push_regles_applicable_basefait *baseregle*)

;;;;;;;;;;;;;;;;;;;;;;;;;; questions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pose la question sur l'age de l'individu
(defun question_age ()
  (format t "Entrez l'age du prevenu :" )
  (setq res (read))
  (if (< res 18) 
      (push '(majorite NIL) *basefait*)
    (push '(majorite OUI) *basefait*)
    )
  )

;;demande le procede utilise a l'utilisateur
(defun question_procede () 
  (format t "~& Entrez le procede utilise : ~& ")
  (format t "~& Copiez coller un element de la ~& liste suivante dans le champ dentree : ~s ~& >  " *const_procede*)
  (setq res (read))
  (if (member res *const_procede*)
      (push (list 'procede res) *basefait*)
    (progn
      (format t "~& Entrez un procede valide. ~& ")
      (question_procede)
      )
    )
  )
(question_procede)

;;demande le but du prevenu a l'utilisateur
(defun question_but () 
  (format t "~& Entrez le but du prevenu : ~& ")
  (format t "~& Copiez coller un element de la ~& liste suivante dans le champ dentree : ~s ~& >  " *const_but*)
  (setq res (read))
  (if (member res *const_but*)
      (push (list 'but res) *basefait*)
    (progn
      (format t "~& Entrez un but valide. ~& ")
      (question_but)
      )
    )
  )
(question_but)

;;demande s'il y circonstances aggravantes
(defun question_ag (nom) 
  (if (member nom *basedonnees*)
      (push '(circonstancesaggravantes oui) *basefait*)
    (progn 
      (format t " ~& Y a t il des circonstances aggravantes ? T ou NIL ~& > ")
      (setq res (read))
      (if (eq res T)
          (push '(circonstancesaggravantes oui) *basefait*)
        (push '(circonstancesaggravantes NIL) *basefait*)
          
     )
    )
    )
  )
(question_ag 'Durand)


;;la fonction question rassemble toutes les fonctions précédentes.
(defun question (nom) 
  (question_age)
  (question_procede)
  (question_but)
  (question_ag nom)
  )
(question)  
;;;;;;;;;;;;;;;;;;;;;;;;; Affichage ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;fonction rendant l'affichage plus agréable.

(defun affichage (regles nom)
  (let ((peinetrouve NIL))
    (dolist (r regles) 
      (if (equal (car r) 'procede)
          (format t " ~& Procede utilise par le prevenu : ~s ~& " (cadr r))
        )
                 
      (if (equal (car r) 'delit)
          (format t " ~& Délit commis : ~s ~&" (cadr r))
        )
        (if (listp (car r))
            (progn 
            (setq peinetrouve T)
          (if (numberp (cadr (car r)))
              (progn 
              (format t " ~& Peine encourue : ~& Emprisonnement : ~s ans, Amende : ~s euros ~&" (cadr (car r)) (cadr (cadr r)))
                (pushnew nom *basedonnees*)
                (format t " ~& Le prevenu ~s a ete ajoute a la base de donnees. ~& " nom)
                )
                (format t " ~& Peine encourue : ~& Emprisonnement : ~s , Amende : ~s ~&" (cadr (car r)) (cadr (cadr r)))
            )
              )
              )
        
          )
        (if (eq peinetrouve NIL) (format t "~& Nous n avons pas pu determiner de peine. ~&"))
        )
        )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   Moteur avant     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun moteuravant (nom)
  (setq *basefait* '())
  (question nom)
  (let ((regles (push_regles_applicable_basefait *baseregle* nom)))
    (if (eq regles NIL) 
        (progn
          (print "ok")
        (format t " ~& Impossible de donner un resultat, nous avons besoin de plus d elements.")
       ) 
          )
    (affichage regles nom)
    )
  )

;;base de faits exemples :
;;premier exemple : 
;;[age = 40], [Procédé = mensonge], [But = obtention_argent]

(moteuravant 'Masson)


;;un individu de 45ans, fait 
;;usage de la violence dans le but d’obtenir de 
;;l’argent d’une personne handicapée (circonstance aggravante)

(moteuravant 'Durand)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;    Chainage arriere en profondeur    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  regles-candidates   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;rentourne la liste des règles dont la conclusion est conc.
(defun regles-candidates (conc *baseregle*)
  (let (list-regles (list))
    (dolist (r *baseregle*)
      (if (equal conc (conclusion r))
           (push r list-regles)
          
            NIL
            )
        
        )
        
  list-regles
  )
    )
  
(regles-candidates '((emprisonnement 10)(amende 100000)) *baseregle*)
(regles-candidates '(tromperie oui) *baseregle*)

;;;;;;;;;;;;;;;;;;;;;;;;;;; appartenanceBF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;renvoie T si le but fait partie de la base de fait, NIL sinon. 

(defun appartenanceBF (but basefait) 
  (let ((ok T))
    (dolist (x basefait) ;;tout ce qui est dans la base de fait
      (if (not (member x but :test #'equal))  ;; doit se trouver dans le chemin
          (setq ok NIL)
        )
    )
    (return-from appartenanceBF ok)
    )
  )


;;;;;; tests 
(setq *bf* '((ca oui)(p f)(m oui)))
(setq *chemin* '((ca oui)(p f)(joile oui)(truc much)(machin oui)(m oui)))
(appartenanceBF *chemin* *bf* )
>T

(setq *bf* '((PROCEDE BONIMENT) (MAJORITE OUI)))
(setq *chemin* '((PROCEDE BONIMENT) (MAJORITE OUI) (TROMPERIE NIL)))
(appartenanceBF *chemin* *bf* )
>T


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;vérification de l'arborescence des règles en profondeur
(defun verifier (basefait chemin baseregle but) 
  (let  ((arret nil))
    (if (appartenanceBF chemin basefait)
        (progn 
          (setq arret T)
          (return-from verifier T)
          )

      (let ((rc (regles-candidates but baseregle)))
        (if (null rc)
            (print "")
        (dolist (r rc)   
          (setq arret (verifierET basefait r baseregle chemin))
          (if (equal arret T)(return))
          )
      ))
    )
    (print arret)
    )
  )


;;vérification des "ET" des prémisses des règles (en profondeur)
(defun verifierET(basefait r baseregle chemin)
  (let ((arret T)(prem (premisses r)))
    (if prem
        (progn
        (dolist (p prem) 
          (push p chemin) 
          )
          (dolist (p prem)
            (setq arret (verifier basefait chemin baseregle p))
            (if (equal arret T)
                (return-from verifierET T)
   )
        )))
    (return-from verifierET arret)
    )
  )


;;;;;;;;;;;;premier test
(setq but '((emprisonnement NIL)(amende NIL)))

(setq *basefait* '((majorite oui)(procede boniment)))
>T

(setq *basefait* '((MAJORITE OUI) (PROCEDE MENSONGE)))
>T

(setq but '((emprisonnement 7)(amende 750000)))
>NIL
;;;;;;;;;;;;;;;second test

(setq but '((emprisonnement 7)(amende 750000)))
(setq *basefait* '((circonstancesaggravantes oui)(but obtention_argent)(procede faux_nom)))
>T

(verifier *basefait* '() *baseregle* but)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;affichage;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;améliore l'affichage de la peine
(defun affichage2 (basefait nom peine)
  (dolist (b basefait)
    (cond 
     ((equal (car b) 'procede)
        (format t "~& Procede utilisé par ~s : ~s ~&" nom (cadr b))
      )
     ((equal (car b) 'but)
        (format t "~& But du prévenu : ~s ~&" (cadr b))
      )
     ((equal (car b) 'circonstancesaggravantes)
        (format t "~& Circonstances aggravantes  ~s ~&" (cadr b))
      ))
    )
  (dolist (p peine)
  (if (equal (car p) 'emprisonnement)
      (format t "~& Emprisonnement : ~s ~&" (cadr p))
    (format t "~& Amende : ~s ~&" (cadr p))
    )
    )
  )

(affichage2 *basefait* 'Durand '((emprisonnement 10)(amende 100000)))
  
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; moteur arriere (en profondeur) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun moteurarriere (but basefait baseregle nom)
  ;;(question nom) ;;utilisation plus rapide en utilisant directement *basefait*
  (dolist (b but) 
    (if (verifier basefait '() baseregle b)
        (progn 
          (affichage2 basefait nom b)
          (return-from moteurarriere "Succes")
          )
      )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;; but ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq *but* '(((emprisonnement non_traite) (amende non_traite))
            ((emprisonnement NIL)(amende NIL))
            ((emprisonnement 5)(amende 375000))
            ((emprisonnement 7)(amende 750000))
            ((emprisonnement 7)(amende 100000))
              ((emprisonnement 10)(amende 100000))))

;;;test 1
(setq *basefait* '((MAJORITE OUI) (PROCEDE MENSONGE)))

;;;test 2 
(setq *basefait* '((circonstancesaggravantes oui)
                   (but obtention_argent)
                   (procede utilisation_violence)(majorite OUI)))



;;;;;;;;;;;;;;;;;;;;;;;;; execution ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(moteurarriere *but* *basefait* *baseregle* 'Durand)





    
        
                              
                          

              
  