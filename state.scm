(define make-state
  (lambda (wordlist hitlist mistakes)
    (list wordlist hitlist mistakes)))

;; getters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define get-wordlist
  (lambda (state)
    (car state)))
(define get-hitlist
  (lambda (state)
    (car (cdr state))))
(define get-mistakes
  (lambda (state)
    (car (cdr (cdr state)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
