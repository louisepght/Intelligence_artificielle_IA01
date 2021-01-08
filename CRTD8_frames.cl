;Louise Poughet 
; Frames 

;déclaration et définition de la liste globale *frames*
(setq *frames* nil)



;définition du frame Elephant
(setq Elephant '(Elephant (type (value concept))
                          (is-a (value etre))
                          (color (default « grey »))
                          (age (if-needed ask-user)(if-added check-age))
                          (poids (if-needed computer-weight-from-age))
                          (affichage (if-added draw-elephant)
                                     (if-removed erase-elephant))))


;ajout du frame Elephant a la liste *frames*
(push 'Elephant *frames*)




;definition de la fonction make-indicidu qui créer un individu et l'ajoute à la liste *frames*
(defun make-individu (frame concept &rest prop-val)
  (let ((id (gentemp "F"))(allowed-slots (mapcar 'car (cdr (symbol-value concept)))) slot value fn)
    (unless (member concept *frames*)(error "le concept nexiste pas"))
      (set id (list frame (list 'is-a (list 'value concept)) ; frame = elephant, concept = être
                    (list 'type (list 'value 'individu))))
      (loop (unless prop-val ; sauf si prop val est nul on continue dans la boucle
          (return nil))
        (setq slot (pop prop-val)) ;slot = age 
        (setq value (pop prop-val)) ; value = (if-needed ask-user)
        (when (member slot allowed-slots)
          (setq fn (cadr (assoc 'if-added (cdr (assoc slot (cdr (symbol-value concept))))))) ;; regarde si il y a un démon if added 
          (setq value (if fn 
                          (funcall fn slot value) 
                           value)
              ) ;; appel le démon fn s'il existe. 
          (when value 
          (set id (append (symbol-value id) (list(list slot (list 'value value))))) ;; ajout des slots et value à id. 
          )
        
          )
        )
    (pushnew id *frames*)
    id
    
    )
  )


;création de plusieurs individus
(make-individu 'Clyde 'Elephant 'color "rose" 'age 5)
(make-individu 'Heloise 'Elephant 'age 10)
(make-individu 'Maurice 'Elephant)




;on vérifie que l'age d'un Elephant est compris entre 0 et 100 ans.
(defun check-age (slot value) 
  (if (and (< value 100)(> value 0)) value) 
  )
  

;définition de la fonction qui va chercher la valeur que l'on souhaite pour un individu donné
(defun get-slot-value (individu slot) 
  (if (not (NULL(get-slot-value-by-individu individu slot)))
      (return-from get-slot-value (get-slot-value-by-individu individu slot))
       (progn
      (if (not (NULL (get-slot-value-by-type individu slot)))
                   (return-from get-slot-value (get-slot-value-by-type individu slot))
                   )
            
        (return-from get-slot-value (get-slot-value-by-demons individu slot))          
          )
      )
  )



;exemples 
(get-slot-value 'Clyde 'type)
>>individu
(get-slot-value 'Heloise 'age)
>>10 
(get-slot-value 'Heloise 'color)
>>("grey")
(get-slot-value 'Clyde 'poids)
>>"Il pese environ 1000kg"





;;définition des fonctions utilisées par get-slot-value

;définition d'une fonction qui retrouve l'identifiant d'un individu dans *frames* à partir de son nom
(defun get-id-individu (individu)  
  (dolist (frame *frames*)
          (let ((ind frame))
            (if (eq (car (symbol-value ind)) individu) (return-from get-id-individu ind)))
            )
  )

                


;retrouve la valeur correspondant à un individu qui n'est pas définit dans l'individu mais dans sa classe mère (ici Elephant)
(defun get-slot-value-by-type (individu slot) 
  (let ((typeisa 
         (cadr (cadr (assoc 'is-a (cdr (symbol-value (get-id-individu individu))))))))
    (if (eq (car (cadr (assoc slot (cdr (symbol-value typeisa))))) 'default)
        (return-from get-slot-value-by-type (cdr (cadr (assoc 'color (cdr (symbol-value typeisa))))))
      )
    )
  )

(get-slot-value-by-type 'Heloise 'color)
>>("grey")


;retrouve la valeur correspondant à un individu qui se trouve dans le frame de l'individu
(defun get-slot-value-by-individu (individu slot) 
      (dolist (frame *frames*)
    (let ((ind frame))
      (if (eq (car (symbol-value ind)) individu)
          (let ((a (assoc slot (cdr (symbol-value ind)))))
        (if a
            (return-from get-slot-value-by-individu (cadr (cadr a)))
          )
            )
        )
      )
        )
  )
(get-slot-value-by-individu 'Clyde 'age)
>> 5



;retrouve la valeur du slot correpondant d'un individu par les démons de la classe mère
(defun get-slot-value-by-demons (individu slot) 
  (let ((num (get-id-individu individu))(typeisa (cadr (cadr (assoc 'is-a (cdr (symbol-value (get-id-individu individu))))))) fn)
    (if (eq (car (cadr (assoc slot (cdr (symbol-value typeisa))))) 'if-needed)
        (progn
        (setq fn (cadr (cadr (assoc slot (cdr (symbol-value typeisa))))))
      (funcall fn individu slot)
          )
      )
    
    )
  )
(get-slot-value-by-demons 'Clyde 'poids)
>>"Il pese environ 1000kg"



;calcul le poids d'un Elephant à partir de son age
(defun computer-weight-from-age (individu slot)
  (let ((ind (symbol-value (get-id-individu individu))) aetas pondere fn)
    (if (assoc 'age (cdr ind))
        (progn 
        (setq aetas (cadr (cadr (assoc 'age (cdr ind)))))
          (cond 
           ((and (> aetas 0)(< aetas 1))(setq pondere "Il pese environ 100 kg"))
           ((and (>= aetas 1)(< aetas 10)(setq pondere "Il pese environ 1000 kg ")))
           ((and (>= aetas 10)(< aetas 50)(setq pondere "Il pese environ 4000 kg")))
           ((and (>= aetas 50)(< aetas 100)(setq pondere "Il pese environ 3500 kg")))
           )
          )
      (progn
        (get-slot-value-by-demons individu 'age)
        )
       )
      
    pondere
    )
  )



;définition de la fonction ask-user dans le cas ou on a pas l'age
(defun ask-user (num slot)
  (let ((ind num) res)
    (format t "Quel est l age de ~s ? ~%" (car ind))
    (setq res (read))
    (append num (list (list 'age (list 'value res))))
    )
  )
 


 


    


                       
                      
                                       
        
  