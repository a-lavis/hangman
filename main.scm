(load "state.scm")

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

(define print-frame
  (lambda (state)
    (let* ((mif (lambda (x s1 s2) (if (<= x (get-mistakes state)) s1 s2)))
	   (head (mif 1 "O" " "))
	   (tors (mif 2 "|" " "))
	   (larm (mif 3 "--" "  "))
	   (rarm (mif 4 "--" "  "))
	   (lleg (mif 5 "/" " "))
	   (rleg (mif 6 "\\" " "))
	   )
      (printf "
   ------
   |    |
   ~A    |
 ~A~A~A  |
  ~A ~A   |
 ---------

" head larm tors rarm lleg rleg)
	)))

(define print-letters
  (lambda (state)
    (define acc
      (lambda (wl hl)
	(cond ((null? wl) void)
	      ((= (car hl) 1)
	       (printf " ~A " (car wl))
	       (acc (cdr wl) (cdr hl)))
	      (else
		(printf " _ ")
		(acc (cdr wl) (cdr hl)))
	      )))
    (acc (get-wordlist state) (get-hitlist state))
    (newline)
    ))

(define print-state
  (lambda (state)
    (newline)
    (printf "###############################")
    (newline)
    (print-frame state)
    (print-letters state)
    (newline)
    ))



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

(define play-loop
  (lambda (game)
    (printf "hey you, take a guess:  ")
    (let* ((letter (read))
	   (update-game (guess letter game)))
      (print-state update-game)
      (cond ((lost? update-game)
	     (printf "you lost\n\n"))
	    ((won? update-game)
	     (printf "you won!\n\n"))
	    (else
	      (newline)
	      (play-loop update-game))))))

(define start-game
  (lambda ()
    ;; todo:
    ;;   (printf "give me a word:  ")
    ;; then replace hardcoded word with (read)
    (let ((game (init-state '(s c h e m e))))
      (print-state game)
      (play-loop game))))

(start-game)
