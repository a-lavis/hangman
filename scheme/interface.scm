;; An alternative to run.scm:
;; instead of running at the command line, provides kind of an API (kinda lol)

;; things there should be an interface into:
;;  - Setting a word
;;  - Guessing a letter
;;  - Knowing if you've won, lost, or need to keep playing
;;  - Knowing what letters you've gotten
;;  - Knowing how many mistakes you've made

(load "main.scm")

(define game (init-state '()))

(define set-state! ;; reset game with a particular word
  (lambda (word)
    (set! game (init-state word))))

(define guess!
  (lambda (letter)
    (set! game (guess letter game))
    (list
      ;; won/lost/keep playing
      (cond ((lost? game) 0) ;; 0 == lost
	    ((won?  game) 1) ;; 1 == won
	    (else         2) ;; 2 == still playing
	    )
      ;; what letters you've gotten
      (map (lambda (w h)
	   (if (= h 1) w '_))
	 (get-wordlist game) (get-hitlist game))
      ;; how many mistakes you've made
      (get-mistakes game))))
