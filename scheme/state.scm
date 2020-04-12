;; maker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
