#lang racket

(require 2htdp/universe
         2htdp/image
         racket/block
         threading)

;TODO Tweak magic numbers
(define window-height 500)
(define window-width 500)
(define term-velocity 10)
(define space-velocity -10)
(define acceleration 2)
(define player-height 40) ;TODO If we make this the circle height, half gets cut off

(define max-bodies-onscreen 15)
(define pipe-body-width 20)
(define pipe-body-height (/ window-height max-bodies-onscreen))
(define pipe-opening-height 2) ;in bodies
(define max-bodies-per-pipe (floor (* (- max-bodies-onscreen pipe-opening-height) 3/4)))
(define dist-between-pipes 100)

;images
(define pipe-end
  (rectangle (+ pipe-body-width 10) pipe-body-height "solid" "dark-red"))
(define pipe-body
  (rectangle pipe-body-width pipe-body-height "solid" "red"))
(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))
;image helpers
;make-pipe-img : boolean positive-integer -> image
(define (make-pipe-img is-upwards num-bodies)
  (define bodies (build-list num-bodies
                         (lambda (x)
                           pipe-body)))
  (if is-upwards
      (apply above ;TODO Lookup how apply works
             pipe-end
             bodies)
      (apply above
             (append bodies
             (list pipe-end)))))


;Structs
(struct pipe (x height top-img bot-img))
(struct world-state (x y velocity score running pipes))

;Struct functions
(define (init-pipe)
  (define num-top-bodies (+ (random max-bodies-per-pipe) 1))
  (pipe window-width
        (* (+ num-top-bodies 1) pipe-body-height);Add 1 for the end of the pipe
        (make-pipe-img #f num-top-bodies)
        (make-pipe-img #t (- max-bodies-onscreen pipe-opening-height num-top-bodies))))
(define (init-state)
  (world-state 0
               (/ window-height 2)
               0
               0
               #t
               '()))


;Callbacks
(define (create-UFO-scene state)
  (for/fold ([acc (place-image/align UFO
                                     50
                                     (world-state-y state)
                                     "left"
                                     "top"
                                     (rectangle window-width window-height "solid" "white"))])
            ([p (in-list (world-state-pipes state))])
    (match p
      [(struct* pipe ([x x] [height height] [top-img top-img] [bot-img bot-img]))
       (~> acc
           (place-image/align top-img x 0 "left" "top" _)
           (place-image/align bot-img x window-height "left" "bottom" _))])))

(define (keypress state key)
  (match key
    [" " (struct-copy world-state state [velocity space-velocity])]
    [else state]))

(define (update-state state)
  (if (world-state-running state)
      (match state 
        [(struct* world-state
                  ([x x] [y y] [score score] [velocity velocity] [pipes pipes]))
         (struct-copy world-state state
                      [x (+ x 1)]
                      [y (max (+ y velocity) 0)]
                      [velocity (min (+ velocity acceleration) term-velocity)]
                      [running ((+ y player-height) . < . window-height)]
                      [pipes (block
                              (define pipes* (map (lambda (curr-pipe) (struct-copy pipe curr-pipe [x (- (pipe-x curr-pipe) 1)])) pipes))
                              (define pipes** (filter (lambda (x) ((+ (pipe-x x) pipe-body-width) . > . 0)) pipes*))

                              (if (= (modulo x dist-between-pipes) 0)
                                   (cons (init-pipe) pipes**)
                                   pipes**))]
                               
                      [score score])]) ;TODO Update score
      
      state))

;main loop
(module+ main
(big-bang (init-state)
          (on-key keypress)
          (on-tick update-state)
          (to-draw create-UFO-scene window-width window-height)))
