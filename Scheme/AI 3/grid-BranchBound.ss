(define searchbb
  (lambda (grid stop-count)
    (car (search2bb grid 0 stop-count 0))))

(define get-frontiersbb
  (lambda (grid x y)
    (let ((frontiers '()))
      (filter
       (lambda (n)
         (not (= (length n) 0)))
       (append
        (list
         (if (and (> x 0) (= (get-node grid (- x 1) y) 1))
             (list (- x 1) y)
             '())
         (if (and (< x (- num-col-row 1)) (= (get-node grid (+ x 1) y) 1))
             (list (+ x 1) y)
             '()))
        (list
         (if (and (> y 0) (= (get-node grid x (- y 1)) 1))
             (list x (- y 1))
             '())
         (if (and (< y (- num-col-row 1)) (= (get-node grid x (+ y 1)) 1))
             (list x (+ y 1))
             '())))))))

(define load-frontiersbb
  (lambda (frontiers path)
    (if (not (null? frontiers))
        (begin
          (insert (list (car frontiers) (+ path 1)))
          ;(draw-frontier (caar frontiers) (cadar frontiers))
          (load-frontiersbb (cdr frontiers) path)))))

(define cleanhousebb
  (lambda (x y path route)
    ;(draw-path-node x y)
    (list (append (list (list x y)) route) path)))

(define contains
  (lambda (lst val)
    (not (equal? (find
                  (lambda (x)
                    (equal? x val)) lst) #f))))

(define search2bb
  (lambda (grid count stop-count path)
    (let ((x (robot-x))
          (y (robot-y)))
      ;(pause pause-num)
      ;(draw-visited x y)
      (set-node! grid x y 2)
      ;(draw-moved-robot x y)
      (if (or (equal? robot target)
              (>= count stop-count))
          (begin
            (set-node! grid x y 1)
            (cleanhousebb x y path '()))
          (let ((frontiers (get-frontiersbb grid x y)))
            (load-frontiersbb frontiers path)
            (if (< (vector-length heap) 1)
                (begin
                  (display "NO PATH FOUND IN BB")
                  (set-node! grid x y 1)
                  #f)
                (let ((next (depqueue)))
                  (set! robot (car next))
                  (let ((ref (search2bb grid (+ count 1) stop-count (cadr next))))
                    (set-node! grid x y 1)
                    (if (and (list? ref)
                             (contains frontiers (caar ref))
                             (< path (cadr ref)))
                        (cleanhousebb x y path (car ref))
                        ref)))))))))

(define pause
  (lambda (count)
    (if (<= count 0)
        0
        (pause (- count 1)))))