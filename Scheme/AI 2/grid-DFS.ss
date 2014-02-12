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
      (display count)
      (newline)
      (draw-visited x y)
      (draw-frontiers frontiers)
      (set-node! grid x y 1)
      (push (list y))
      (push (list x))
      (draw-moved-robot (robot-x) (robot-y))
      (if (or
            (equal? robot goal)
            (>= count stop-count))
          (cleanhouse))
      (doMoves grid frontiers count stop-count)
      (pop)
      (pop))))
(define draw-frontiers
  (lambda (frontiers)
    (if (not (null? frontiers))
        (begin
          (draw-frontier (car (car frontiers)) (cadr (car frontiers)))
          (draw-frontiers (cdr frontiers))))))
(define doMoves
  (lambda (grid frontiers count stop-count)
    (if (and
          (not (equal? robot goal))
          (not (null? frontiers)))
        (begin
          (set! robot (car frontiers))
          (search2 grid (+ count 1) stop-count)
          (doMoves grid (cdr frontiers) count stop-count)))))
(define cleanhouse
  (lambda ()
    (if (not (list? (top)))
        (begin
          (draw-path-node (pop) (pop))
          (cleanhouse)))))
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