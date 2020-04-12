(load "state.scm")

;; Initialization of game states:
;; ------------------------------------------------------------------
;; WTWL (word-to-wordlist)
;; -----------------------
;; INPUT: WORD, a word (represented as what? a string?).
;; OUTPUT: The list of letters in that word.
;; -----------------------
;; for now I am just hard-coding in the word in "main.scm",
;; so this function is incomplete.
(define wtwl
  (lambda (word) word))

(define hit-init
  (lambda (wordlist)
    (cond ((null? wordlist) '())
	  (else (cons 0 (hit-init (cdr wordlist))))
	  )))

(define init-state
  (lambda (word)
    (let ((wordlist (wtwl word)))
      (make-state wordlist (hit-init wordlist) 0)
      )))
;; ------------------------------------------------------------------


;; Determining if a game state is a loss or a win:
;; ------------------------------------------------------------------
(define lost?
  (lambda (state)
    (>= (get-mistakes state) 6)))

(define won?
  (lambda (state)
    (letrec ((wt (lambda (listy)
		   (or (null? listy)
		       (and (= (car listy) 1)
			    (wt (rest listy)))))))
      (wt (get-hitlist state)))))
;; ------------------------------------------------------------------


;; Making a guess:
;; ------------------------------------------------------------------
(define guess
  (lambda (letter state)
    (define newhlmaker
      (lambda (wl hl)
	(cond ((null? wl) '())
	      ((eq? letter (car wl)) (cons 1 (newhlmaker (cdr wl) (cdr hl))))
	      (else (cons (car hl) (newhlmaker (cdr wl) (cdr hl))))
	      )))
    (let* ((newhitlist (newhlmaker (get-wordlist state) (get-hitlist state)))
	   (newmistakes (if (member letter (get-wordlist state))
			  (get-mistakes state)
			  (+ (get-mistakes state) 1)))
	   )
      (make-state
	(get-wordlist state)
	newhitlist
	newmistakes
	))))
;; ------------------------------------------------------------------
