(define search
  (lambda (grid stop-count)
    (search2 grid 1 stop-count)))
(define search2
  (lambda (grid count stop-count)
    (pause pause-num)
    (let ((x (robot-x))
          (y (robot-y))
          (frontiers 
            (filter 
              (lambda (n) 
                (not (= (length n) 0))) 
              (get-frontiers grid (robot-x) (robot-y)))))
      (draw-visited x y)
      (draw-frontiers frontiers)
      (set-node! grid x y 1)
      (queue-frontiers frontiers)
      (draw-moved-robot (robot-x) (robot-y))
      (if (or
            (equal? robot goal)
            (>= count stop-count))
          (cleanhouse x y)
          (begin
            (set! robot (nextMove))
            (let ((ref (search2 grid (+ count 1) stop-count)))
              (if (contains frontiers ref)
                  (cleanhouse x y)
                  ref)))))))
(define draw-frontiers
  (lambda (frontiers)
    (if (not (null? frontiers))
        (begin
          (draw-frontier (car (car frontiers)) (cadr (car frontiers)))
          (draw-frontiers (cdr frontiers))))))
(define contains
  (lambda (lst val)
    (not (equal? (find 
      (lambda (x)
        (equal? x val)) lst) #f))))
(define nextMove
  (lambda ()
    (let ((pt (delete)))
      (if (= (get-node grid (car pt) (cadr pt)) 1)
          (nextMove)
          pt))))
(define cleanhouse
  (lambda (x y)
    (draw-path-node x y)
    (list x y)))
(define queue-frontiers
  (lambda (frontiers)
    (if (not (null? frontiers))
        (begin
          (insert (list (car frontiers) steps))
          (queue-frontiers (cdr frontiers))))))
(define get-frontiers
  (lambda (grid x y)
    (let ((frontiers '()))
      (append
        (list
          (if (and (> x 0) (< (get-node grid (- x 1) y) 1))
              (list (- x 1) y)
              '())
          (if (and (< x (- num-col-row 1)) (< (get-node grid (+ x 1) y) 1))
              (list (+ x 1) y)
              '()))
        (list
          (if (and (> y 0) (< (get-node grid x (- y 1)) 1))
              (list x (- y 1))
              '())
          (if (and (< y (- num-col-row 1)) (< (get-node grid x (+ y 1)) 1))
              (list x (+ y 1))
              '()))))))
(define pause
  (lambda (count)
    (if (<= count 0)
        0
        (pause (- count 1)))))