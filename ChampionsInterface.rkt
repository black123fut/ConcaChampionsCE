;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Champions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)

(open-graphics)

(define window (open-viewport "ConcaChampionsCE" 920 720))
(define window2 (open-pixmap "ConcaChampionsCE" 920 720))
(define players '(((300 150) (375 50) 13) ((400 300) (150 150) 16) ((300 200) (100 450) 20)  ))
;;(posicionI), fuerza-distancia
(define bola '((450 260) ))

;;(define linea (line 50 0 'red))
;;(define texto (text "2" 20 'white))

;;Marcador y generaciones
(define (escribirDatos marcador generacion)
  (begin
    ;;(overlay/xy texto 430 600 linea)
    ((draw-string window2) (make-posn 430 600) (number->string (car marcador)) "white")
    ((draw-string window2) (make-posn 490 600) (number->string (cadr marcador)) "white")
    ((draw-string window2) (make-posn 800 600) "Geracion: " "white")
    ((draw-string window2) (make-posn 820 615) (number->string generacion) "white")
    ))

;;Cancha
(define (dibujarCancha)
  (begin
    ((draw-solid-rectangle window2) (make-posn 0 0) 920 720 "black") ;;Fondo
    ((draw-solid-rectangle window2) (make-posn 0 0) 920 540 "green") ;;Cesped
    ((draw-solid-rectangle window2) (make-posn 0 0) 5 540 "white")   ;;Raya vertical
    ((draw-solid-rectangle window2) (make-posn 915 0) 5 540 "white") ;;Raya vertical
    ((draw-solid-rectangle window2) (make-posn 0 0) 920 5 "white")   ;;Raya horizontal
    ((draw-solid-rectangle window2) (make-posn 0 535) 920 5 "white") ;;Raya horizontal
    ((draw-solid-rectangle window2) (make-posn 457 0) 5 540 "white") ;;Raya medio campo
    ((draw-solid-ellipse window2) (make-posn 452 262) 15 15 "white") ;;Circulo central
    ((flip-ellipse window2) (make-posn 385 195) 150 150 "white")     ;;Circulo grande central
    ((flip-rectangle window2) (make-posn -90 185) 180 170 "white")   ;;Marco izquierdo
    ((flip-rectangle window2) (make-posn 830 185) 180 170 "white")   ;;Marco derecho
    ))

(define (posX jugador)
  (caar jugador))

(define (posY jugador)
  (cadar jugador))

(define (destinoX jugador)
  (caadr jugador))

(define (destinoY jugador)
  (car (cdadr jugador)))

(define (terminoMovimiento jugador)
  (cons (list (destinoX jugador) (destinoY jugador)) (cdr jugador)))

(define (hipotenusa jugador)
  (sqrt (+ (expt (- (destinoX jugador) (posX jugador)) 2)
          (expt (- (destinoY jugador) (posY jugador)) 2)))
  )

(define (movimientoX jugadores step diagonal angulo)
  (+ (posX (car jugadores)) (* step (cos (degrees->radians angulo))))
  )

(define (movimientoY jugadores step diagonal angulo)
  (+ (posY (car jugadores)) (* step (sin (degrees->radians angulo))))
  )

(define (getNextPlayer jugadores)
  (cond ((null? (cdr jugadores))
         (car jugadores))
        (else (cadr jugadores))
        ))

(define (pendiente jugador)
  (/ (- (destinoY jugador) (posY jugador)) (- (destinoX jugador) (posX jugador))))

(define (obtenerAnguloAux jugador)
  (list (- (destinoY jugador) (posY jugador)) (- (destinoX jugador) (posX jugador))))

(define (obtenerAngulo jugador)
  (cond
    ((and (< (car (obtenerAnguloAux jugador)) 0) (< (cadr (obtenerAnguloAux jugador)) 0))
     (- (radians->degrees (atan (pendiente jugador))) 180)
     )
    ((and (> (car (obtenerAnguloAux jugador)) 0) (< (cadr (obtenerAnguloAux jugador)) 0))
     (+ (radians->degrees (atan (pendiente jugador))) 180)
     )
    (else (radians->degrees (atan (pendiente jugador))))
    )
  )

(define (dibujarJugadores jugadores number)  
  (begin
    (cond ((null? jugadores) #t)
          (else (begin
                  ((draw-solid-ellipse window2) (make-posn (posX (car jugadores)) (posY (car jugadores))) 30 30 "purple")
                  ((draw-string window2) (make-posn (+ (caaar jugadores) 13) (+ (cadar (car jugadores)) 15)) (number->string number) "white")
                  (dibujarJugadores (cdr jugadores) (+ 1 number))
                  )))
    ))

(define (moverJugadores jugadores step nuevosJugadores diagonal angulo)
  (cond ((null? jugadores) nuevosJugadores)
        ((< diagonal step)
         (begin
           (moverJugadores (cdr jugadores) 0
                           (cons (terminoMovimiento (car jugadores)) nuevosJugadores)
                           (hipotenusa (getNextPlayer jugadores))
                           (obtenerAngulo (getNextPlayer jugadores))))
         )
        (else
         (begin
           (display step)
           (dibujarCancha)
           ((draw-solid-ellipse window2) (make-posn (movimientoX jugadores step diagonal angulo) (movimientoY jugadores step diagonal angulo))
                                         30 30 "purple")
           (dibujarJugadores (append nuevosJugadores (cdr jugadores)) 1)
           
           (copy-viewport window2 window)
           ((clear-viewport window2))
           
           ;;(display step)
           ;;(display "    X: ")
           ;;(display (movimientoX jugadores step diagonal angulo))
           ;;(newline)
           
           (moverJugadores jugadores (+ step 10) nuevosJugadores diagonal angulo)
           ))
        ))

(define (start)
  (begin
    (dibujarCancha)
    (escribirDatos '(1 0) 12)
    
    (moverJugadores players 0 '() (hipotenusa (car players)) (obtenerAngulo (car players)) )
    (escribirDatos '(1 1) 17)

    (copy-viewport window2 window)
    ((clear-viewport window2))
    ;;(dibujarJugadores '(((300 250) (800 250)) ((300 300) (450 300)) ((300 350) (350 350))) 1)
    ))

(start)