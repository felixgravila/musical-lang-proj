#lang racket

(require "music-base.rkt")

; =====================
; == OOP BOILERPLATE ==
; =====================

(provide method-lookup send new-instance)
  
(define (method-lookup object selector)
  (cond ((procedure? object)
         (let ((result (object selector)))
           (if (procedure? result)
               result
               (error "Did not find any method")))
         )
        (else
         (error "Inappropriate object in method-lookup: " object))))

(define (send message obj . par)
  (let ((method (method-lookup obj message)))
    (apply method par)))

(define (new-instance class . parameters)
  (apply class parameters))


; =====================
; ===== UTILITIES =====
; =====================

(define (max a b)
  (if ( > a b)
      a
      b
      )
  )

; checks if element e is a note, pause, parralel- or sequential-comp
(define (valid-music-element? e)
  (cond
    ((not (procedure? e)) #F)
    ((note? e) #T)
    ((pause? e) #T)
    ((parallel-comp? e) #T)
    ((seq-comp? e) #T)
    (else #F)
    )
  )

; checks a list of components to verify
; that all are music elements
(define (check-valid-components components)
  (if (null? components) #T
      (let ((head (car components))
            (tail (cdr components))
            )
        (if (valid-music-element? head)
            (check-valid-components tail)
            #F
            )
        )
      )
  )

; preliminary check that verifies
; that parameters sent to a note are valid
; return 0 if not valid, number of parameters otherwise
(define (check-note-valid args)
  (if (null? args)
      0
      (if (and (>= (car args) 0)
               (<= (car args) 127))
          (if (null? (cdr args))
              1
              (if (and (>= (car (cdr args)) 0)
                       (<= (car (cdr args)) 8))
                  (if (null? (cdr (cdr args)))
                      2
                      (if (> (car (cdr (cdr args))) 0)
                          3
                          0
                          )
                      )
                  0)
              )
          0)
      )
  )

; sorts the basic duration list
; used for degree-of-polyphony
(define (sort-duration-list list)
  (sort list (lambda (x y) (< (car x) (car y))))
  )

; recurses through list while notes at the same time as the head
; returns count
(define (polyphonic-consecutive list head n)
  (if (null? list)
      n
      (if (< (car (car list)) (+ (car head) (cdr head)))
          (polyphonic-consecutive (cdr list) head (+ 1 n))
          n
          )
      )
  )
    
; helper function for degree-of-polyphony
; recurses through list and returns
; max between the polyphonic-consecutive from this point
; and the same function applied to the rest of the list
(define (deg-of-pol-h list)
  (if (null? list)
      0
      (max
       (polyphonic-consecutive (cdr list) (car list) 1)
       (deg-of-pol-h (cdr list))
       )
      )
  )


; =================
; === POLYPHONY ===
; =================

; returns the degree of polyphony of the song
; uses the build-basic-duration-list function that is functionally very similar to build-music
; it builds a list with all notes, notes expressed as (timestamp . duration)
; then sorts the list by timestamp and iterates through it to find the degree

(define (degree-of-polyphony song)
  (let ((slist (sort-duration-list (send 'build-basic-duration-list song))))
    (deg-of-pol-h slist)
    )
  )

; returns true if there is at most one note playing at a time
; otherwise false
(define (monophonic? song)
  (>= 1 (degree-of-polyphony song))
  )

; ================
; ===== NOTE =====
; ================

; helper function for note-actual
; this makes it easier to see variable param handling
; parameters
;     tone - mandatory
;     instrument - optional - default 1 Piano
;     duration - optional - default 960
(define (note . args)
  (let* ((argl (check-note-valid args))
         (_tone (if (>= argl 1) (car args) 0))
         (_inst (if (>= argl 2) (car (cdr args)) 1))
         (_dura (if (>= argl 3) (car (cdr (cdr args))) 960))
         )
    (if (= argl 0)
        (error "Invalid note arguments")
        (new-instance note-actual _tone _inst _dura)
        )
    )
  )
          

; actual note object
; never accessed directly
(define (note-actual _tone _instrument _duration)
  (let ((tone _tone)
        (instrument _instrument)
        (duration _duration)
        )
    (define (type-of) 'note)
    (define (gettone) tone)
    (define (getinstrument) instrument)
    (define (getduration) duration)
    (define (to-string) (list 'note tone instrument duration))
    (define (build-basic-duration-list . timestamp) ; explained under "POLYPHONY"
      (cons (cons (if (null? timestamp) 0 (car timestamp)) duration) '()))
    (define (build-music . timestamp) ; end of road, returns the note-abs-time-with-duration object
      (note-abs-time-with-duration (if (null? timestamp) 0 (car timestamp)) instrument tone 80 duration))
    (define (transpose delta) ; transposing adds delta to the tone
      (new-instance note (+ delta tone) instrument duration))
    (define (reinstrument _instrument) ; reinstrumenting changes the instrument to the new one
      (new-instance note tone _instrument duration))
    (define (scale factor) ; scaling multiplies the duration by the factor given
      (new-instance note tone instrument (inexact->exact (round (* factor duration))))) ; round and convert from real to int
    (lambda (message)
      (cond ((eq? message 'gettone) gettone)
            ((eq? message 'getinstrument) getinstrument)
            ((eq? message 'getduration) getduration)
            ((eq? message 'type-of) type-of)
            ((eq? message 'build-basic-duration-list) build-basic-duration-list)
            ((eq? message 'to-string) to-string)
            ((eq? message 'build-music) build-music) ; builds the list with which the midi is created
            ((eq? message 'transpose) transpose)
            ((eq? message 'reinstrument) reinstrument)
            ((eq? message 'scale) scale)
            (else (error "Message not understood"))))
    )
  )
  

(define (note? x)
  ( equal? (send 'type-of x) 'note ))


; =================
; ===== PAUSE =====
; =================

; pause with optional duration parameter
; if no param given, default to 960
(define (pause . _duration)
  (let (( duration (if (null? _duration) 960 (max 0 (car _duration)))))
    (define (type-of) 'pause)
    (define (getduration) duration)
    (define (to-string) (list 'pause duration))
    (define (build-basic-duration-list . timestamp) '()) ; explained under "POLYPHONY"
    (define (build-music . timestamp) '()) ; other end of road, returns the empty list.
    (define (transpose delta)
      (new-instance pause duration))
    (define (reinstrument _instrument)
      (new-instance pause duration))
    (define (scale factor)
      (new-instance pause (* factor duration)))
    (lambda (message)
      (cond ((eq? message 'getduration) getduration)
            ((eq? message 'type-of) type-of)
            ((eq? message 'build-basic-duration-list) build-basic-duration-list)
            ((eq? message 'to-string) to-string)
            ((eq? message 'build-music) build-music)
            ((eq? message 'transpose) transpose)
            ((eq? message 'reinstrument) reinstrument)
            ((eq? message 'scale) scale)
            (else (error "Message not understood"))))
    )
  )

(define (pause? x)
  ( equal? (send 'type-of x) 'pause ))




; =========================
; ===== PARALLEL COMP =====
; =========================

(define (parallel-comp . _components)
  (if (check-valid-components _components)
      (let ((components _components))
        (define (getcomponents) components)
        (define (type-of) 'parallel-comp)

        ;BUILD basic time set, explained under "POLYPHONY"
        (define (build-par-basic-dur-list components timestamp)
          (if (null? components) '()
              (if (pause? (car components))
                  (build-par-basic-dur-list (cdr components) timestamp) ;skip if pause
                  (let ((built (send 'build-basic-duration-list (car components) timestamp)))
                    (if (equal? 'note-abs-time-with-duration (car built)) ; if list with one list, append. If not, cons
                        (cons
                         built
                         (build-par-basic-dur-list (cdr components) timestamp) ; same timestamp start
                         )
                        (append
                         built
                         (build-par-basic-dur-list (cdr components) timestamp)
                         )
                        )
                    )
                  )
              )
          )
        (define (build-basic-duration-list . timestamp)
          (build-par-basic-dur-list components (if (null? timestamp) 0 (car timestamp)))) 

        ;BUILD actual music list
        (define (build-par-music components timestamp)
          (if (null? components) '()
              (if (pause? (car components))
                  (build-par-music (cdr components) timestamp) ;skip if pause
                  (let ((built (send 'build-music (car components) timestamp)))
                    (if (equal? 'note-abs-time-with-duration (car built)) ; if list with one list, append. If not, cons
                        (cons
                         built
                         (build-par-music (cdr components) timestamp) ; same timestamp start
                         )
                        (append
                         built
                         (build-par-music (cdr components) timestamp)
                         )
                        )
                    )
                  )
              )
          )
        (define (build-music . timestamp)
          (build-par-music components (if (null? timestamp) 0 (car timestamp))))

        ;APPLY TRANSFORM TO ALL
        ; function that applies transpose, reinstrument or scale to everything underneath
        ; the beauty of recursion does all the work
        (define (apply-transform-to-all transform-type value comps)
          (if (null? comps) '()
              (cons
               (send transform-type (car comps) value)
               (apply-transform-to-all transform-type value (cdr comps))
               )
              )
          )
    
        ;GET DURATION
        ; since it is a parallel comp, return the max length of any of the sub-components
        ; recursion does the rest
        (define (get-parallel-comp-duration components length)
          (if (null? components) length
              (get-parallel-comp-duration (cdr components) (max length (send 'getduration (car components))))
              )
          )
        (define (getduration)
          (get-parallel-comp-duration components 0))

        (lambda (message)
          (cond ((eq? message 'getcomponents) getcomponents)
                ((eq? message 'type-of) type-of)
                ((eq? message 'build-basic-duration-list) build-basic-duration-list)
                ((eq? message 'build-music) build-music)
                ((eq? message 'getduration) getduration)
                ((eq? message 'transpose) (lambda (d) (apply new-instance parallel-comp (apply-transform-to-all 'transpose d components))))
                ((eq? message 'reinstrument) (lambda (d) (apply new-instance parallel-comp (apply-transform-to-all 'reinstrument d components))))
                ((eq? message 'scale) (lambda (d) (apply new-instance parallel-comp (apply-transform-to-all 'scale d components))))
                (else (error "Message not understood"))))
        )
      (error "Not a valid configuration")
      )
  )

(define (parallel-comp? x)
  ( equal? (send 'type-of x) 'parallel-comp ))




; ===========================
; ===== SEQUENTIAL COMP =====
; ===========================

(define (seq-comp . _components)
  (if (check-valid-components _components)
      (let ((components _components)) ;TODO check valid
        (define (getcomponents) components)
        (define (type-of) 'seq-comp)

        ;BUILD basic list, explained under "POLYPHONY"
        (define (build-seq-basic-dur-list components timestamp)
          (if (null? components) '()
              (if (pause? (car components))
                  (build-seq-basic-dur-list (cdr components) (+ timestamp (send 'getduration (car components)))) ;skip if pause
                  (let ((built (send 'build-basic-duration-list (car components) timestamp)))
                    (if (equal? 'note-abs-time-with-duration (car built)) ; if list with one list, append. If not, cons
                        (cons
                         (send 'build-basic-duration-list (car components) timestamp)
                         (build-seq-basic-dur-list (cdr components) (+ timestamp (send 'getduration (car components))))
                         )
                        (append
                         built
                         (build-seq-basic-dur-list (cdr components) (+ timestamp (send 'getduration (car components))))
                         )
                        )
                    )
                  )
              )
          )
        (define (build-basic-duration-list . timestamp)
          (build-seq-basic-dur-list components (if (null? timestamp) 0 (car timestamp)))) 

        ;BUILD actual music list
        (define (build-seq-music components timestamp)
          (if (null? components) '()
              (if (pause? (car components))
                  (build-seq-music (cdr components) (+ timestamp (send 'getduration (car components)))) ;skip if pause
                  (let ((built (send 'build-music (car components) timestamp)))
                    (if (equal? 'note-abs-time-with-duration (car built)) ; if list with one list, append. If not, cons
                        (cons
                         (send 'build-music (car components) timestamp)
                         (build-seq-music (cdr components) (+ timestamp (send 'getduration (car components))))
                         )
                        (append
                         built
                         (build-seq-music (cdr components) (+ timestamp (send 'getduration (car components))))
                         )
                        )
                    )
                  )
              )
          )
        (define (build-music . timestamp)
          (build-seq-music components (if (null? timestamp) 0 (car timestamp))))

        ;APPLY TRANSFORM TO ALL
        ; same as for parallel-comp
        (define (apply-transform-to-all transform-type value comps)
          (if (null? comps) '()
              (cons
               (send transform-type (car comps) value)
               (apply-transform-to-all transform-type value (cdr comps))
               )
              )
          )

        ;GET DURATION
        ; since it's a sequential comp, sum the durations recursively.
        (define (get-sequential-comp-duration components length)
          (if (null? components) length
              (get-sequential-comp-duration (cdr components) (+ length (send 'getduration (car components))))
              )
          )
        (define (getduration)
          (get-sequential-comp-duration components 0))
    
        (lambda (message)
          (cond ((eq? message 'getcomponents) getcomponents)
                ((eq? message 'type-of) type-of)
                ((eq? message 'build-basic-duration-list) build-basic-duration-list)
                ((eq? message 'build-music) build-music)
                ((eq? message 'getduration) getduration)
                ((eq? message 'transpose) (lambda (d) (apply new-instance seq-comp (apply-transform-to-all 'transpose d components))))
                ((eq? message 'reinstrument) (lambda (d) (apply new-instance seq-comp (apply-transform-to-all 'reinstrument d components))))
                ((eq? message 'scale) (lambda (d) (apply new-instance seq-comp (apply-transform-to-all 'scale d components))))
                (else (error "Message not understood"))))
        )
      (error "Not a valid configuration")
      )
  )

(define (seq-comp? x)
  ( equal? (send 'type-of x) 'seq-comp ))


; ===================
; ===== TESTING =====
; ===================


;Creating test sequential composition
;Illustrating flexible note parameters
(define c-maj (new-instance seq-comp
                            (new-instance note 36)
                            (new-instance note 38 1)
                            (new-instance note 40 1 960)
                            (new-instance note 41 1)
                            (new-instance note 43)
                            (new-instance note 45 1 960)
                            (new-instance note 47 3)
                            (new-instance note 48)
                            )
  )

(display "The original song\n")
(send 'build-music c-maj)
(display "Its duration is currently ")
(send 'getduration c-maj)

(display "\nTesting the scaling function by making it a bit faster,\n")
(define c-maj-s (send 'scale c-maj 0.9))
(display "the reinstrument function by switching to instrument 6, \n")
(define c-maj-sr (send 'reinstrument c-maj-s 6))
(display "and the transpose function by transposing by a full octave\n")
(define c-maj-srt (send 'transpose c-maj-sr 12))
(send 'build-music c-maj-srt)

(display "Its duration is now ")
(send 'getduration c-maj-srt)

(display "\nTest if the sequential composition c-maj is monophonic: \n")
(display (if (monophonic? c-maj) "Yes it is.\n" "No it isn't.\n"))

;Creating test parallel composition
(define c-maj-chord (new-instance parallel-comp
                                  (new-instance note 36)
                                  (new-instance note 40)
                                  (new-instance note 79)
                                  (new-instance note 48)
                                  (new-instance note 88)
                                  (new-instance note 91)
                                  (new-instance note 96)
                                  (new-instance note 100)
                                  (new-instance note 103)
                                  )
  )


(display "\nTesting the degree of polyphony of c-maj-chord over 3 octaves\n")
(degree-of-polyphony c-maj-chord)

;Finally defining our canon song
(define canon
  (new-instance parallel-comp
                (new-instance seq-comp
                              (new-instance note 72 1 960)
                              (new-instance note 71 1 960)
                              (new-instance note 69 1 960)
                              (new-instance note 67 1 960)
                              (new-instance note 65 1 960)
                              (new-instance note 64 1 960)
                              (new-instance note 62 1 960)
                              (new-instance note 60 1 960))
                (new-instance seq-comp
                              (new-instance pause 1920)
                              (new-instance note 72 1 960)
                              (new-instance note 71 1 960)
                              (new-instance note 69 1 960)
                              (new-instance note 67 1 960)
                              (new-instance note 65 1 960)
                              (new-instance note 64 1 960)
                              (new-instance note 62 1 960)
                              (new-instance note 60 1 960))
                (new-instance seq-comp
                              (new-instance pause 3840)
                              (new-instance note 72 1 960)
                              (new-instance note 71 1 960)
                              (new-instance note 69 1 960)
                              (new-instance note 67 1 960)
                              (new-instance note 65 1 960)
                              (new-instance note 64 1 960)
                              (new-instance note 62 1 960)
                              (new-instance note 60 1 960))
                )
  )

(display "\nFinally the canon song:\n")
(send 'build-music canon)
(display "The degree of polyphony is ")
(degree-of-polyphony canon)

;Converting it and writing it to a file
(define song (send 'build-music canon))
(transform-to-midi-file-and-write-to-file! song "canon-song.midi")
(display "\nThe file \"canon-song.midi\" has been created.\n")