(define accumulate
  (lambda (op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))))))

(define flatmap
  (lambda (proc seq)
    (accumulate append '() (map proc seq))))

(define permutations
  (lambda (s)
    (if (null? s)
        (list '())
        (flatmap (lambda (x)
                   (map (lambda (p) (cons x p))
                        (permutations (remove x s))))
                 s))))

(define OPS (quote (+ - * /)))

(define getList
  (lambda (la n)
    (if (= n 0)
        (car la)
        (getList (cdr la) (- n 1)))))

(define getAllOP
  (lambda (i j k)
    (cond
      ((and (= i 3) (= j 3) (= k 3)) (list (getOneList i j k)))
      ((and (= j 3) (= k 3)) (cons (getOneList i j k) (getAllOP (+ i 1) 0 0)))
      ((= k 3) (cons (getOneList i j k) (getAllOP i (+ j 1) 0)))
      (else (cons (getOneList i j k) (getAllOP i j (+ k 1)))))))

(define getOneList
  (lambda (i j k)
    (list (getList OPS i) (getList OPS j) (getList OPS k))))

(define remove
  (lambda (item s)
    (cond
      ((null? s) (quote ()))
      ((equal? item (car s)) (cdr s))
      (else
        (cons (car s)
              (remove item (cdr s)))))))

(define game24
  (lambda (la lb)
    (if (null? lb)
        (car la)
        (case (car lb)
          ((quote +) (operatored + la lb))
          ((quote -) (operatored - la lb))
          ((quote *) (operatored * la lb))
          ((quote /) (operatored / la lb))))))

(define operatored
  (lambda (op la lb)
    (let ((x (game24 (cdr la) (cdr lb))))
         (cond
           ((null? x) (quote ()))
           ((and (equal? op /) (= x 0))
             (quote ()))
           (else (op (car la) x))))))

(define m-v
  (lambda (la lb)
    (cond
        ((null? la) (quote ()))
        (else
          (let ((result (game24 (car la) lb)))
               (cond
                 ((null? result) (m-v (cdr la) lb))
                 ((or (= result -24) (= result 24))
                   (display (car la))
          		   (display lb)
                   (newline))
                 (else
                   (m-v (cdr la) lb))))))))

(define m-m
  (lambda (la lb)
    (cond
      ((null? lb) (quote ()))
      (else
        (m-v la (car lb))
        (m-m la (cdr lb))))))

(m-m (permutations (quote (8 10 6 3)))
     (getAllOP 0 0 0))
