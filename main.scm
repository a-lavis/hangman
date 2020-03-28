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



(define guess-nd
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

(define game (init-state '(s c h e m e)))
(print-state game)

(define guess!
  (lambda (letter)
    (set! game (guess-nd letter game))
    (print-state game)
    (cond ((lost? game)
	   (printf "you lost\n\n")
	   (exit))
	  ((won? game)
	   (printf "you won!\n\n")
	   (exit)))))

(define play-loop
  (lambda ()
    (printf "hey you, take a guess:  ")
    (guess! (read))
    (newline)
    (play-loop)))

(play-loop)
