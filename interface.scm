;; An alternative to run.scm:
;; instead of running at the command line, provides kind of an API (kinda lol)

;; things there should be an interface into:
;;  - Setting a word
;;  - Guessing a letter
;;  - Knowing if you've won, lost, or need to keep playing

(load "main.scm")

(define state (init-state '()))

(define set-state! ;; reset state with a particular word
  (lambda (word)
    (set! state (init-state word))))

(define guess!
  (lambda (letter)
    (set! state (guess letter state))
    (cond ((lost? state) 0) ;; 0 == lost
	  ((won?  state) 1) ;; 1 == won
	  (else          2) ;; 2 == still playing
	  )))
