; The Arduino blink test program

(define (setup)
  (set-pin-mode! 13 'c-output))

(define (loop)
  (set-pin! 13 'c-high)
  (delay 1000)
  (set-pin! 13 'c-low)
  (delay 1000))