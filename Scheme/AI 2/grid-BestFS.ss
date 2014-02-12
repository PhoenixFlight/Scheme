(define search
  (lambda (grid stop-count)
    (search2 grid 0 stop-count (list robot))))

(define cleanhouse
  (lambda (path)
    (if (null? path)
        #t
        (begin
          (draw-path-node (car (car path)) (cadr (car path)))
          (cleanhouse (cdr path))))))

(define load-frontiers
  (lambda (grid path)
    (let ((x (robot-x))
          (y (robot-y)))
      (if (and (> x 0) (< (get-node grid (- x 1) y) 1))
          (begin
            (insert (list (list (- x 1) y) (append path (list (list (- x 1) y)))))
            (draw-frontier (- x 1) y)))
      (if (and (< x (- num-col-row 1)) (< (get-node grid (+ x 1) y) 1))
          (begin
            (insert (list (list (+ x 1) y) (append path (list (list (+ x 1) y)))))
            (draw-frontier (+ x 1) y)))
      (if (and (> y 0) (< (get-node grid x (- y 1)) 1))
          (begin
            (insert (list (list x (- y 1)) (append path (list (list x (- y 1))))))
            (draw-frontier x (- y 1))))
      (if (and (< y (- num-col-row 1)) (< (get-node grid x (+ y 1)) 1))
          (begin
            (insert (list (list x (+ y 1)) (append path (list (list x (+ y 1))))))
            (draw-frontier x (+ y 1)))))))

(define search2
  (lambda (grid count stop-count path)
    (pause pause-num)
    (draw-visited (robot-x) (robot-y))
    (set-node! grid (robot-x) (robot-y) 1)
    (draw-moved-robot (robot-x) (robot-y))
    ;(if (< (vector-length heap) 1)
    ;    #f
        (if (or (equal? robot goal)
                (>= count stop-count))
            (cleanhouse path)
            (begin
              (load-frontiers grid path)
              (let ((next (depqueue)))
                (set! robot (car next))
                (search2 grid (+ count 1) stop-count (cadr next)))))))

(define pause
  (lambda (count)
    (if (<= count 0)
        0
        (pause (- count 1)))))