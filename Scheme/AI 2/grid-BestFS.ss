(define search
  (lambda (grid stop-count)
    (search2 grid 0 stop-count 0)))

(define get-frontiers
  (lambda (grid x y)
    (let ((frontiers '()))
      (filter 
              (lambda (n) 
                (not (= (length n) 0)))
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
                    '())))))))

(define load-frontiers 
  (lambda (frontiers path)
    (if (not (null? frontiers))
        (begin
          (insert (list (car frontiers) (+ path 1)))
          (draw-frontier (caar frontiers) (cadar frontiers))
          (load-frontiers (cdr frontiers) path)))))

(define cleanhouse
  (lambda (x y path)
    (draw-path-node x y)
    (list (list x y) path)))

(define contains
  (lambda (lst val)
    (not (equal? (find 
      (lambda (x)
        (equal? x val)) lst) #f))))

(define search2
  (lambda (grid count stop-count path)
    (let ((x (robot-x))
          (y (robot-y)))
      (pause pause-num)
      (draw-visited x y)
      (set-node! grid x y 1)
      (draw-moved-robot x y)
      (if (or (equal? robot goal)
              (>= count stop-count))
          (cleanhouse x y path)
            (let ((frontiers (get-frontiers grid x y)))
              (load-frontiers frontiers path)
              (if (< (vector-length heap) 1)
                  (begin
                    (display "NO PATH FOUND")
                    #f)
                  (let ((next (depqueue)))
                    (set! robot (car next))
                    (let ((ref (search2 grid (+ count 1) stop-count (cadr next))))
                      (if (and (list? ref)
                               (contains frontiers (car ref))
                               (< path (cadr ref)))
                          (cleanhouse x y path)
                          ref)))))))))

(define pause
  (lambda (count)
    (if (<= count 0)
        0
        (pause (- count 1)))))