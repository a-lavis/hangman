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
    (let* ((wordlist (get-wordlist state))
	   (mem (member letter wordlist)))
      (make-state
	wordlist
	((if mem
	   (letrec ((newhlmaker
		      (lambda (wl hl)
			(if (null? wl) '()
			  (let ((hit (if (eq? letter (car wl)) 1 (car hl))))
			    (cons hit (newhlmaker (cdr wl) (cdr hl))))
			  ))))
	     (lambda (hitlist) (newhlmaker wordlist hitlist)))
	   identity)
	 (get-hitlist state))
	(+ (get-mistakes state) (if mem 0 1))
	))))
;; ------------------------------------------------------------------
