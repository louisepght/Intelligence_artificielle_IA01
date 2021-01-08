;;Louise Poughet

;;NETL

;1- structure d'un noeud : 
; ((name Cyrano)(type personne)(arc-in <id arcs>)(arc-out <id-arcs>))
;2- structure d'un arc : 
; ((type is-a)(from <noeud from>)(to <noeud to>) 


(setq *nodes* '())
(setq *arcs* '())

(defnode 'personne 'Roxane) ; N0

(defnode 'personne 'Cyrano) ; N1

(defnode 'personne 'Christian) ; N2

(defnode 'personne 'deGuiche) ; N3

(defnode 'titre 'cadet_de_gascogne) ; N4

(defnode 'titre 'compte) ; N5

(defnode 'titre 'noble) ; N6

(defnode 'titre 'mondaine) ; N7

(defarc 'aime 'N1 'N0) ; A12

(defarc 'aime 'N2 'N0) ; A13

(defarc 'aime 'N3 'N0) ;A14

(defarc 'aime 'N0 'N2)  ;A15

(defarc 'is_a 'N0 'N7)  ;A16

(defarc 'is_a 'N1 'N4) ;A17

(defarc 'is_a 'N2 'N4) ;A18

(defarc 'is_a 'N3 'N5);A19

(defarc 'is_a 'N4 'N6);A20

(defarc 'is_a 'N5 'N6);A21

(mark-node 'N0 'M1) 
>>((MARK M1) (ARC-OUT A12 A11) (ARC-IN A10 A9 A8) (NAME ROXANE) (TYPE PERSONNE))
(mark-node 'N6 'M2)
>>((MARK M2) (ARC-IN A17 A16) (NAME NOBLE) (TYPE TITRE))


(get-marked-nodes 'M1)
>>(N0)

(get-marked-nodes 'M2)
>>(N6)

(get-result 'M1 'aime 'M2)
;;;debug non terminé

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun defnode (type name)
  (let ((id (gentemp "N")))   
    (set id (list (list 'name name)(list 'type type)))
    (pushnew id *nodes*)
    (return-from defnode id)
)
  )
;1 - générer un identifiant grâce à gentemp 
;2 - ajouter les listes des attributs à cet identifiant avec "set" 
;3 - ajouter cet identifiant à la liste globale des noeuds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun defarc (type from to)
  (let ((id (gentemp "A")))
    (set id (list (list 'type type)
                  (list 'from from)
                  (list 'to to))
         )
    (addv from 'arc-out id)
    (addv to 'arc-in id)
    (pushnew id *arcs*)
    id
)
)
;1 - générer un identifiant avec gentemp 
;2 - ajouter les listes à cet identifiant avec "set" 
;3 - ajouter l'arc généré aux propriétés arc-out arc-in de from et to.

;exemple : 
(is-a A B) 
> A : ((type is-a)(from A)(to B))
;=> l'arc A est un arc entrant pour B et un arc sortant pour A.

;;;;;;;;;;;;;;;;;;;;;;;;   getv    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getv (id prop) 
  (cdr (assoc prop (symbol-value id)))
  )
(getv N0 'type) 
>> assoc va chercher la liste pour laquelle la clé est 'type et renvoie cette liste. 
>> on prend le cdr de cette liste pour avoir la valeur associée à la clé : (('cle <valeur>)('cle <valeur>))

;;;;;;;;;;;;;;;;;;;;;;;;;   addv    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun addv(id prop val)
  (let ((pair (assoc prop (symbol-value id))))
    (setv id prop (cons val (cdr pair)))))



(defun addv (id prop val) 
  (let ((pair (assoc prop (symbol-value id))))
    (if pair 
        (setf (cdr pair)(cons val (cdr pair)))
      (push (list prop val)(symbol-value id))
      )))

; 1 - assoc va chercher la propriété pour laquelle on veut ajouter une valeurs : notamment pour arc-out, arc-in
; 2 - Si la propriété existe déja : 
;;;	on remplace par concaténation (cons) la nouvelle valeur à la liste existante
;;;	sinon : on ajoute directement la propriété et sa valeur à l'identifiant. 

ex : 
on veut ajouter à N14 ((name Roxane) (type personne)) un arc entrant : (arc-in A20) : 
(addv N14 'arc-in A20) 
>>((name Roxane) (type personne) (arc-in A20))

on veut ajouter à N14 ((name Roxane)(type personne)(arc-in A20)) un nouvel arc entrant : (arc-in A35) : 
(addv N14 'arc-in A20)
>>((name Roxane) (type personne) (arc-in A20 A35))


;;;;;;;;;;;;;;;;;;;;;;;;    setv     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun setv (id prop val) 
  (let ((pair (assoc prop (symbol-value id))))
  (set id (cons (cons prop val) 
                (remove pair (symbol-value id) :test #'equal)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mark-node(node mark) 
  (addv node 'mark mark)
  )

; on ajoute simplement la propriété (mark M1) à un noeud. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun is-marked?(node mark)
  (if (member mark (getv node 'mark) :test #'equal)
      (return-from is-marked? T))
  (return-from is-marked? NIL)
  
  )
;prédicat pour savoir si un sommet est marqué 
;1 - si le sommet est déjà marqué on retourne vrai sinon faux.
structure d'un noeud : ((mark M0)(name Roxane)(type personne))
> pour savoir si la marque M0 est membre de la liste, on doit regarder les valeur (d'où le getv). 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
direction = :direct ou :inverse
vague 1 : chercher tous les noeuds marques
vague 2 : chercher les noeuds interessants, successeurs 
vague 3 : chercher les noeuds successeurs etc.
Algo : 
marked-nodes < get marked nodes(mark) 
tant que marked nodes != nil 
      pour chaque noeud de marked nodes 
        si direction = :direct 
                 next-nodes < succeurs (noeud type-arc) 
         sinon 
                      next-nodes < predecesseurs (noeud type-arc) 
        si next-node != nil 
             pour chaque noeud n de next-nodes
                       si n non marque par mark 
                                        marquer n 
                                          et new-marked-nodes < new-marked-nodes + n 
                       sinon rien
             fin du pour
         fin du si 
         marked-nodes< new-marked-nodes
      fin pour 
fin tant que 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wave (mark type-arc direction)
	(let (
			(marked-nodes (get-marked-nodes mark)) ;; noeuds marqués par mark 
			(next-nodes nil)
			(new-marked-nodes nil)
		)
		
   (loop while marked-nodes ;; tant qu'on a des noeuds marqués
         do
         	
			(dolist (node marked-nodes) ;; pour chaque noeud marqué
		        (if (eq direction 'direct) ;;si on choisit la propagation dans le sens direct
		        	(setq next-nodes (successeurs node type-arc)) ;; on prend les successeurs. 
		        	(setq next-nodes (predecesseurs node type-arc))
		        )
		       
	        	(dolist (n next-nodes) ;; pour chaque successeur
	        		(when (is-marked n mark) 
	        			(progn
	        				(mark-node n mark) ;; marquage de n 
	        				(setq new-marked-nodes (cons n new-marked-nodes)) ;; ajout de n aux nouveaux noeuds marqués. 
	        			)
	        		)
	        		
	        	)
		    )
		    (setq mark-nodes new-marked-nodes)
		    (setq new-marked-nodes nil)	
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun successeurs (node type-arc) 
  (let ((res (list)))(arcs nil)
    (setq arcs (getv node 'arc-out))
    (dolist (arc arcs) 
      (if (eq (cadr (assoc 'type (symbol-value arc)))type-arc)
          (pushnew (car (getv arc 'to)) res)
        )
      )
    (return-from successeurs res)
    )
  )
; But : trouver les successeurs d'un noeud par rapport à un type d'arc. 
; 1 - les noeuds successeurs sont aux bouts des arc-out => on récupère ces arcs. 
; 2 - pour chaque arc récupéré, 
		si le type de l'arc est celui cherché alors on ajoute la valeur associé à 'to de l'arc en question 
		sinon on recommance 
    

(defun predecesseurs (node type-arc) 
  (let ((res (list)))(arcs nil)
    (setq arcs (getv node 'arc-in))
    (dolist (arc arcs)
      (if (eq (cadr (assoc 'type (symbol-value arc)))type-arc)
          (pushnew (car (getv arc 'to)) res)
        )
      )
    (return-from predecesseurs res)
    )
  )
; but : trouver les prédécesseurs d'un noeud par rapport à un type d'arc. 
; 1 - de la meme manière que pour les successeurs, on va cherche les arc entrant dans ce noeuds. 
; 2 - pour chaque arc entrant
		si le type de l'arc correspond à celui passé en paramètre. 
		sinon on recommance. 
		
;;;;;;;;;;;;;;;;;;;;;;à tester ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-marked-nodes (mark)
    (let ((marked-nodes(list)))
      (dolist (n *nodes*)
        (if (is-marked? n mark)
            (pushnew n marked-nodes)
          )
        )
      (return-from get-marked-nodes marked-nodes)
      )
  )
; 1 - trouver les noeuds marqués par "mark" donc M0 par exemple. 
	Pour cela, on parcourt les noeuds, 
	pour chaque noeud, on regarde s'il est marqué pae M0, si oui et s'il n'est pas déjà dans la liste des noeuds marqués, on l'ajoute à cette liste. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-result (mark1 type-arc mark2) 
  (let (
        (wave1 (wave mark1 'is_a :direct)) 
        (wave2 (wave mark2 'is_a :inverse))
        (result NIL)
        )
    (dolist (origine wave1) 
      (dolist (cible (successeurs origine type-arc))
        (if (is-marked? cible mark2) 
            (pushnew (list (cadr (assoc 'NAME (symbol-value origine))) type-arc
                           (cadr (assoc 'NAME (symbol-value cible)))) result)
          )
        )
      )
    (format T "~%~%Le(s) sommet(s) marqué(s) par ~S qui ~S le(s) sommet(s) marqué(s) par ~S est/sont :~%" mark1 type-arc mark2)
    (return-from get-result result)
    )
  )


si get results est autre chose que nil, la réponse sera oui si on arrive 
à trouver des arcs 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Simulation du marquage : 
1) marquer Roxane par M1, 
marquer Noble par M2. 
2) on propage M2 le long des arcs is-a dans le sens inverse.
3) on regarde si il y a des arcs "aime" ayant pour origine M1 
et pour extrémité M2. 


