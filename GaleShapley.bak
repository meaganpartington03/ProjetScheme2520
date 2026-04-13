;#lang scheme
#lang racket

; Projet partie 4 - Scheme
; Par : Meagan Partington 300416906
; Anastasia Sadosvkyy 300426037

; Fonctions fournies par le prof pour lire les fichiers CSV

(define (read-f filename) (call-with-input-file filename
(lambda (input-port)
(let loop ((line (read-line input-port)))
(cond
 ((eof-object? line) '())
 (#t (begin (cons (string-split (clean-line line) ",") (loop (read-line input-port))))))))))

(define (format-resident lst)
  (list (car lst) (cadr lst) (caddr lst) (cdddr lst)))

(define (format-program lst)
  (list (car lst) (cadr lst) (string->number (caddr lst)) (map string->number(cdddr lst))))

(define (clean-line str)
  (list->string
   (filter (lambda (c) (not (or (char=? c #\") (char=? c #\[) (char=? c #\]) )))
           (string->list str))))

(define (read-residents filename)
(map (lambda(L) (format-resident (cons (string->number (car L)) (cdr L)))) (cdr (read-f filename))))

(define (read-programs filename)
(map format-program (cdr (read-f filename))))


; Definition des listes de residents et de programmes
(define PLIST (read-programs "programSmall.csv"))
(define RLIST (read-residents "residentSmall.csv"))


; get-resident-info
; return les infos d'un resident a partir de son RID dans la liste des residnets
(define (get-resident-info rid rlist)
  (cond ((null? rlist) '()) ; si la liste est vide sa retourne une liste vide
         ((= (car (car rlist)) rid) (car rlist)) ; si l'id du premier resident correspond sa le retourne
         (else (get-resident-info rid (cdr rlist))))) ; sinon cherche dans le reste de la liste


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
        ((and (number? rid)
        (number? (car rol))
        (= (car rol) rid)) position) ; ont peut trouver le resident donc on return sa position
        (else (rank-helper rid (cdr rol) ; sinon continue avec le reste
                  (+ position 1))))) ; incremente la position


; rank
; return le rang d'un resient dans le ROL d'un programme (premier rang commence a 0)
(define (rank rid pinfo)
  (if (not (number? rid)) 9999 (rank-helper rid (cadddr pinfo) 0)))


; member-of-match
; verifie si un resident est dans la liste des residents apparies d'un seul programme
(define (member-of-match rid residents)
  (cond ((null? residents) #f) ((and (number? rid) ; liste vide donc resident non trouve
        (= (car (car residents)) rid)) #t) ; le car de la paire est l'ID du resident
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


; update-matches
; remplace l'entree d'un programme dans la liste d'appariements, ou l'ajoute si elle n'existe pas encore
(define (update-matches pid new-match matches)
  (cond ((null? matches) (list new-match))       ; pas trouve, ajoute a la fin
        ((equal? (car (car matches)) pid) (cons new-match   ; trouve, remplace l'entree
                                               (cdr matches)))
        (else (cons (car matches)       ; sinon, garde l'entree courante
                    (update-matches pid new-match (cdr matches)))))) ; et continue dans le reste


; offer : fonction de l'algorithme de McVitie-Wilson
; le resident propose au premier programme de sa liste de preferences, retourne la liste d'appariements mise a jour
(define (offer rinfo rlist plist matches)
  (let ((prefs (cadddr rinfo)))  ; recupere la liste de preferences
    (if (null? prefs)
        matches      ; plus de programmes a essayer
        (let* ((pid (car prefs))           ; premier programme prefere
               (pinfo (get-program-info pid plist))          ; info du programme cible
               (trimmed-rinfo (list (car rinfo) (cadr rinfo) ; cree un rinfo sans le programme
                                    (caddr rinfo) (cdr prefs))))    ;qu'on vient d'essayer
          (evaluate trimmed-rinfo pinfo rlist plist matches)))))    ;envoie a evaluate pour decision


; evaluate : fonction de l'algorithme de McVitie-Wilson
; evalue si un resident peut etre accepte par un programme
;si le programme est plein, compare le nouveau resident avec le moins prefere des residents actuels et expulse si besoin
(define (evaluate rinfo pinfo rlist plist matches)
  (if (null? pinfo) matches ; s'il n'y a pas d'autre programmes à évaluer on retourne 'matches'
      (let* (
          ; Si rinfo est pair AND si le premier élément du rinfo est un nombre
          ; on rétourne le premier élément, si non on rétourne zéro
          (rid (if (and (pair? rinfo) (number? (car rinfo))) (car rinfo) 0))
          (pid (car pinfo)) ; rechercher le program ID à partir du pinfo
          (quota (caddr pinfo)) ; rechercher le quota du pinfo (3eme élément du pinfo)
          (r (let ((val (rank rid pinfo))) ; calculer le rank du resident pour le program
            (if (and (number? val) (>= val 0)) val 9999))) ; si le nombre est valide, on le garde
                                                           ; sinon on l'assigne un mauvais rang (9999)

          (pair (cons rid r)) ; on crée un pair

          (current-match (get-match pid matches)) ; trouver le match actuel pour se programme
          (current-residents (if (null? current-match) '() (cadr current-match)))) ; extraire les résidents actuels
          ; s'il n'y a pas une match, on retourne un liste vide, sinon on prend le deuxième élément

        ; les cas
        (cond
          ((null? current-match) ; si un program n'a pas du match, on crée un nouveau match avec se rédesint
           (update-matches pid (list pid (list pair)) matches))

          ((< (length current-residents) quota) ; si le program n'est pas remplie, ajoute le résident
           (update-matches pid (add-resident-to-match pair current-match) matches))

          ; Si le program est remplie et le résident est meillieure que le plus pire résident
          ((and (not (null? current-residents)) (< r (cdr (car current-residents))))
           (let*
              ((worst (car (car current-residents))) ; trouver le ID du plus pire résident
                (worst-rinfo (get-resident-info worst rlist)) ; trouver l'info du résident
                  (safe-worst (if (and (pair? worst-rinfo) (number? (car worst-rinfo))) worst-rinfo rinfo)) ; vérification

                  (new-match (add-resident-to-match pair (list pid (cdr current-residents)))) ; supprimer le plus pire résident
                                                                                              ; remplacer par le nouveau résident
                  (updated-matches (update-matches pid new-match matches))) ; mise à jour du matches
             (offer safe-worst rlist plist updated-matches))) ; offre le plus pire résident à des autres programmes

          (else (offer rinfo rlist plist matches)))))) ; si le résident n'est pas accepté, essaie un autre programme


; gale-shapley : fonction principale de l'algorithme de McVitie-Wilson
; appelle offer pour chaque resident de la liste et retourne la liste finale des appariements

(define (gale-shapley rlist plist matches)
  (if (null? rlist) matches ; si la liste est vide, rétourne matches
      (let* (
             (current (car rlist)) ; prend le premier résident du liste
             (result (offer current rlist plist matches)) ; essaie d'offrir le résident au programmes
             (safe-result (if (list? result) result matches)) ; si une résultat (result) est valide, on va l'utiliser, 
                                                              ; sinon on utilise matches
            )
        (gale-shapley (cdr rlist) plist safe-result)))) ; passer le reste des résidents dans la fonction gale-shapley


(define (get-not-matched-list rlist matches)
  (cond
    ((null? rlist) '()) ; si le liste des résidents est null, retourne une liste vide
    ((matched? (car (car rlist)) matches) ; vérifie si le résident est matched ou non
     (get-not-matched-list (cdr rlist) matches)) ; appelle la fonction encore
    (else ; sinon
     (cons (car (car rlist)) ; ajouter le ID du résident au liste
           (get-not-matched-list (cdr rlist) matches))))) ; fonction récursive, continue avec le reste du liste

(define (display-program-matches match rlist plist)
  (let ((pid (car match)) (residents (cadr match))) ; trouver l'info nécessaire
    (for-each ; pour chaque...
     (lambda (pair)
       (let* ((rid (car pair)) ; trouver le rid du résident
              (rinfo (get-resident-info rid rlist)) ; trouver l'info du résident
              (pinfo (get-program-info pid plist))) ; trouver l'info du programme

        ; Display toute l'information nécessaire
         (display (caddr rinfo)) (display ",") ;
         (display (cadr rinfo)) (display ",")
         (display rid) (display ",")
         (display pid) (display ",")
         (display (cadr pinfo))
         (newline)))
         residents))) ; pour chaque résident

(define (display-not-matched not-matched rlist) ; même chose que display-program-matches, mais on trouver l'info
                                                ; des résident qui n'ont pas des matches
  (for-each
   (lambda (rid)
     (let ((rinfo (get-resident-info rid rlist)))
       (display (caddr rinfo)) (display ",")
       (display (cadr rinfo)) (display ",")
       (display rid) (display ",")
       (display "XXX,NOT_MATCHED")
       (newline)))
   not-matched))

(define (get-total-available-positions matches plist)
  (let loop ((plist plist) (total 0))
    (if (null? plist) total ; s'il n'y a pas des programmes qui reste, rétourne les positions disponibles (total)
        (let* ((pid (car (car plist))) ; trouver le program ID
               (quota (caddr (car plist))) ; trouver le quota
               (match (get-match pid matches)) ; trouver les match courrants
               (filled (if (null? match) 0 (length (cadr match))))) ; combien des résidents sont assignés, si'il n'y a pas de match - 0
                                                                    ; s'il y a un match, on retourne la longueur du liste des résidents
          (loop (cdr plist) (+ total (- quota filled))))))) ; aller aux prochain programme et calculer le total


; gale-shapley-print 
; fonction de plus haut niveau qui affiche les resultats de l'algorithme de Gale-Shapley
; (fournie par le professeur, adaptee pour notre implementation)

(define (gale-shapley-print rlist plist)
  (let* ((matches (gale-shapley rlist plist '()))
        (not-matched-list (get-not-matched-list rlist matches)))
    (for-each (lambda(m)
        (display-program-matches m rlist plist)) matches)
    (display-not-matched not-matched-list rlist)
    (display "Number of unmatched residents: ")
        (display (length not-matched-list)) (newline)
    (display "Number of positions available: ")
    (display (get-total-available-positions matches plist))
    (newline)))


; Commande à éxécuter
;(gale-shapley RLIST PLIST '())
(gale-shapley-print RLIST PLIST)
