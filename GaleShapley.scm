; Projet partie 4 - Scheme
; Par : Meagan Partington 300416906
; Anastasia Sardosvkyy 300426037


; get-resident-info
; return les infos d'un resident a partir de son RID dans la liste des residnets
(define (get-resident-info rid rlist)
  (cond ((null? rlist) '()) ; si la liste est vide sa retourne une liste vide
         ((= (car (car rlist)) rid) (car rlist)) ; si l'id du premier resident correspond sa le retourne
         (else (get-resident-info rid (cdr rlist)))))) ; sinon cherche dans le reste de la liste


; get-program-info
; return les infos d'un programme a partir de son PID dans la liste des programmes
(define (get-program-info pid plist)
  (cond ((null? plist) '())  ; si la liste est vide return une liste vide
        ((equal? (car (car plist)) pid) (car plist))  ; si l'ID du premier programme match return it
        (else (get-program-info pid (cdr plist))))) ; sinon cherche dans le reste de la liste



; rank-helper
; fonction qui parourt le ROL et return la position du resident (a partir de 0)
(define (rank-helper rid rol position)
  (cond ((null? rol) -1) ; si ont ne peut pas trouver le resident sa return -1
        ((= (car rol) rid) position)  ; ont peut trouver le resident donc on return sa position
        (else (rank-helper rid (cdr rol) ; sinon continue avec le reste
                           (+ position 1))))) ; incremente la position


; rank
; return le rang d'un resient dans le ROL d'un programme (premier rang commence a 0)
(define (rank rid pinfo)
  (rank-helper rid (cadddr pinfo) 0)) ; commence la recherche a la position 0


; member-of-match
; verifie si un resident est dans la liste des residents apparies d'un seul programme
(define (member-of-match rid residents)
  (cond ((null? residents) #f)  ; liste vide donc resident non trouve
        ((= (car (car residents)) rid) #t) ; le car de la paire est l'ID du resident
        (else (member-of-match rid (cdr residents))))) ; sinon continue dnas le reste


; matched?
; return vrai si le resident est apparier dans au moins un programme de la liste d'appariements
(define (matched? rid matches)
  (cond ((null? matches) #f)  ; liste vide alors resident non apparie
        ((member-of-match rid (cadr (car matches))) #t) ; verifie dans le programme courant
        (else (matched? rid (cdr matches))))) ; sinon cherche dans le reste


; get-match
; return l'entrer d'appariement associer a un programme dans la liste des appariements
(define (get-match pid matches)
  (cond ((null? matches) '())  ; liste vide alors aucun appariement trouver
        ((equal? (car (car matches)) pid) (car matches)) ; si l'ID match ont return l'entrer
        (else (get-match pid (cdr matches))))) ; sinon ont chrche dans le reste


; insert-sorted
; insere uune paire dans la liste des residents trier par rang du moins preferer
(define (insert-sorted pair residents)
  (cond ((null? residents) (list pair)) ; liste vide donc ajour la paire seule
        ((>= (cdr pair) (cdr (car residents))) ; si le rang est plus grand ou egal
         (cons pair residents)) ; insere avant l'element courant
        (else (cons (car residents) ; sinon ont garde l'element courany
                    (insert-sorted pair (cdr residents)))))) ; insere dans le reste


; add-resident-to-match
; ajoute un resident et son rang a la liste des residents apparies d'un programme
(define (add-resident-to-match pair match)
  (list (car match)  ; garde l'ID du programme
        (insert-sorted pair (cadr match)))) ; insere le resident dans l'ordre