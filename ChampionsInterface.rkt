;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ChampionsInterface) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)

(open-graphics)

(define window (open-viewport "ConcaChampionsCE" 920 720))
(define window2 (open-pixmap "ConcaChampionsCE" 920 720))
;;(define players '(((340 150) (455 260) 20 1) ((300 100) (700 400) 16 2) ((700 400) (300 300) 10 3)  ))
;;(posicionI), fuerza-distancia, angulo
(define firstBall '((450 260) 0 45))

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
    ((flip-rectangle window2) (make-posn -90 180) 180 180 "white")   ;;Marco izquierdo
    ((flip-rectangle window2) (make-posn 830 180) 180 180 "white")   ;;Marco derecho
    ))

(define (drawWindow)
  (begin
    (copy-viewport window2 window)
    ((clear-viewport window2))
    )
  )

(define (obtenerEquipo jugador)
  (caddr (cdddr jugador)))

(define (posX jugador)
  (car (obtenerXY jugador)))

(define (posY jugador)
  (cadr (obtenerXY jugador)))

(define (destinoX jugador)
  (car (obtenerXYf jugador)))

(define (destinoY jugador)
  (cadr (obtenerXYf jugador)))

(define (terminoMovimiento jugador)
  (cons (list (destinoX jugador) (destinoY jugador)) (cdr jugador)))

(define (hipotenusa jugador)
  (cond ((null? jugador) 0)
        (else (sqrt (+ (expt (- (destinoX jugador) (posX jugador)) 2)
          (expt (- (destinoY jugador) (posY jugador)) 2))))
        ))

(define (movimientoX jugadores step diagonal angulo)
  (cond ((null? jugadores) 0)
        (else (+ (posX (car jugadores)) (* step (cos (degrees->radians angulo))))))
  )

(define (movimientoY jugadores step diagonal angulo)
  (cond ((null? jugadores) 0)
        (else (+ (posY (car jugadores)) (* step (sin (degrees->radians angulo))))))
  )

(define (getNextPlayer jugadores)
  (cond ((null? jugadores) '())
        (else (car jugadores))
        ))

(define (pendiente jugador)
  (cond ((zero? (- (destinoX jugador) (posX jugador)))
         (/ (- (destinoY jugador) (posY jugador)) 0.5))
        (else (/ (- (destinoY jugador) (posY jugador)) (- (destinoX jugador) (posX jugador))))
        )
  )

(define (obtenerAnguloAux jugador)
  (list (- (destinoY jugador) (posY jugador)) (- (destinoX jugador) (posX jugador))))

(define (obtenerAngulo jugador)
  (cond
    ((null? jugador) 0)
    ((and (< (car (obtenerAnguloAux jugador)) 0) (< (cadr (obtenerAnguloAux jugador)) 0))
     (- (radians->degrees (atan (pendiente jugador))) 180)
     )
    ((and (> (car (obtenerAnguloAux jugador)) 0) (< (cadr (obtenerAnguloAux jugador)) 0))
     (+ (radians->degrees (atan (pendiente jugador))) 180)
     )
    (else (radians->degrees (atan (pendiente jugador))))
    )
  )

;(define (fuerzaJugador jugador)
;  (caddr jugador)
;  )

(define (dibujarJugadores jugadores number)  
  (cond ((null? jugadores) #t)
        (else (begin
                ((draw-solid-ellipse window2) (make-posn (posX (car jugadores)) (posY (car jugadores))) 30 30 (obtenerEquipo (car jugadores)))
                ((draw-string window2) (make-posn (+ (caaar jugadores) 13) (+ (cadar (car jugadores)) 15)) (number->string (obtenerNum (car jugadores))) "white")
                (dibujarJugadores (cdr jugadores) (+ 1 number))
                ))
        )
  )

(define (interseccionAux jugadores bola)
  (cond ((null? jugadores) bola)
        ((and (< (posX (car jugadores)) (+ (posX bola) 20)) (< (posY (car jugadores)) (+ (posY bola) 20))
              (> (+ (posX (car jugadores)) 30) (posX bola)) (> (+ (posY (car jugadores)) 30) (posY bola)))  
                                                        ;;Hay va el angulo del jugador
         (list (car bola) (+ 10 (obtenerFuerza (car jugadores))) (anguloTiro (car jugadores)))
         )
        (else (interseccionAux (cdr jugadores) bola)) 
        )
  )

;; intersects(double x, double y, double w, double h) 
;;x < maxX && y < maxY && x + w > minX && y + h > minY
(define (interseccion jugador xm ym bola jugadores)
  (cond ((and (< xm (+ (posX bola) 20)) (< ym (+ (posY bola) 20)) (> (+ xm 30) (posX bola)) (> (+ ym 30) (posY bola)))
                                                       ;;Hay va el angulo del jugador 
         (list (car bola) (+ 10 (obtenerFuerza jugador)) (anguloTiro jugador))
         )
        (else (interseccionAux jugadores bola))
        ))

(define (fuerzaBola bola)
  (cond ((not (list? bola)) bola)
        (else (cadr bola))))

(define (anguloBola bola)
  (cond ((number? bola) bola)
        (else (caddr bola)))
  )

;;(cambiarFuerzaBola (interseccion (car jugadores) (movimientoX jugadores step diagonal angulo)
;;                 (movimientoY jugadores step diagonal angulo) bola (append nuevosJugadores (cdr jugadores)))
;;                                              )

(define (verificaLimites bola)
  (cond ((< (posY bola) 1)
         (list (list (posX bola) 1) (fuerzaBola bola) (+ (anguloBola bola) 240)))
        ((> (posY bola) 519)
         (list (list (posX bola) 519) (fuerzaBola bola) (- (anguloBola bola) 240)))
        ((and (< (posX bola) 1) (or (< (posY bola) 165) (> (posY bola) 360)))
         (list (list 1 (posY bola)) (fuerzaBola bola) (+ (anguloBola bola) 240)))
        ((and (> (posX bola) 889) (or (< (posY bola) 165) (> (posY bola) 360)))
         (list (list 889 (posY bola)) (fuerzaBola bola) (+ (anguloBola bola) 240)))
        (else bola)
    ))

(define (cambiarFuerzaBola bola)
    (cond ((> (fuerzaBola bola) 0)
                                              ;;Duracion del tiro
           (verificaLimites (list
                             (list (+ (posX bola) (* (fuerzaBola bola) (cos (degrees->radians (anguloBola bola)))))
                                   (+ (posY bola) (* (fuerzaBola bola) (sin (degrees->radians (anguloBola bola))))))
                             (- (fuerzaBola bola) 0.38) (anguloBola bola))))
          (else (list (car bola) (fuerzaBola bola) (anguloBola bola)))
          )
    
  )

(define (dibujarBola bola)
  (cond ((or (< (fuerzaBola bola) 0) (zero? (fuerzaBola bola)))
         (begin
           ((draw-solid-ellipse window2) (make-posn (posX bola) (posY bola)) 20 20 "white")
           ((draw-ellipse window2) (make-posn (posX bola) (posY bola)) 20 20 "black")
           ((draw-line window2) (make-posn (+ (posX bola) 10) (posY bola))
                                (make-posn (+ (posX bola) 10) (+ (posY bola) 20)) "black")
           ((draw-line window2) (make-posn (posX bola) (+ (posY bola) 10))
                                (make-posn (+ (posX bola) 20) (+ (posY bola) 10)) "black")
           )
         )
        (else
         (begin
           ((draw-solid-ellipse window2) (make-posn (posX bola) (posY bola)) 20 20 "white")
           ((draw-ellipse window2) (make-posn (posX bola) (posY bola)) 20 20 "black")
           ((draw-ellipse window2) (make-posn (+ (posX bola) (/ (- 20 (fuerzaBola bola)) 2))
                                              (+ (posY bola) (/ (- 20 (fuerzaBola bola)) 2)))
                                   (fuerzaBola bola) (fuerzaBola bola) "black")
           ))
        )
  )

;;(define (obtenerAptitudTiro jugador)
;;  (cond ((equal? (car (cdddr jugador)) 1) 45)
;;        ((equal? (car (cdddr jugador)) 2) -45)
;;        ((equal? (car (cdddr jugador)) 3) (obtenerAngulo ))
;;        )
;;  )
;; (define (mover_a_bola  jugador listaF ball)

(define (remplazarDestino jugadores nuevos bola)
  (cond ((null? jugadores) '())
        ((equal? (obtenerEquipo (car jugadores)) "purple")
         (append (cons (mover_a_bola_S (car jugadores) nuevos bola) '()) (cdr jugadores)))
        (else
         (append (cons (mover_a_bola (car jugadores) nuevos bola) '()) (cdr jugadores)))
        )
  )

(define (obtenerResto jugadores)
  (cond ((null? jugadores) '())
        (else (cdr jugadores))
        ))

(define (estela-aux jugadores step nuevosJugadores diagonal angulo bola)
  (estela jugadores 0
          nuevosJugadores
          (hipotenusa (getNextPlayer jugadores))
          (obtenerAngulo (getNextPlayer jugadores))
          (cambiarFuerzaBola (interseccion
                              (getNextPlayer jugadores)
                              (movimientoX jugadores step diagonal angulo)
                              (movimientoY jugadores step diagonal angulo)
                              bola (append nuevosJugadores (obtenerResto jugadores))))))

(define (estela jugadores step nuevosJugadores diagonal angulo bola)
  (begin
    ;;(sleep 1)
    (cond ((null? jugadores)
           (cond
             ((not (or (< (fuerzaBola bola) 0) (zero? (fuerzaBola bola))))
               (begin
                 (dibujarCancha)
                 (dibujarJugadores  nuevosJugadores 1)
                 (dibujarBola (interseccionAux nuevosJugadores bola))
                 (drawWindow)
                 (estela '() 0 nuevosJugadores 0 0
                                 (cambiarFuerzaBola (fuerzaBola (interseccionAux nuevosJugadores bola))))))
             (else (list nuevosJugadores bola))))
        ((< diagonal step)
         (estela-aux
          (remplazarDestino (cdr jugadores) nuevosJugadores bola) 0
          (cons (terminoMovimiento (car jugadores)) nuevosJugadores)
          diagonal angulo bola))        
        (else
         (begin
           (dibujarCancha)
           ((draw-solid-ellipse window2) (make-posn (movimientoX jugadores step diagonal angulo)
                                                    (movimientoY jugadores step diagonal angulo))
                                         30 30 (obtenerEquipo (car jugadores)))
           (dibujarJugadores (append nuevosJugadores (cdr jugadores)) 1)
           (dibujarBola (interseccion (car jugadores)
                                      (movimientoX jugadores step diagonal angulo)
                                      (movimientoY jugadores step diagonal angulo)
                                      bola (append nuevosJugadores (cdr jugadores))))
           (drawWindow)
           (estela jugadores (+ step 18) nuevosJugadores diagonal angulo
                           (cambiarFuerzaBola (interseccion (car jugadores)
                                                            (movimientoX jugadores step diagonal angulo)
                                                            (movimientoY jugadores step diagonal angulo) bola
                                                            (append nuevosJugadores (cdr jugadores))))))))))

(define (startMatchAux players generaciones)
  (startMatch
   (seleccion-reproduccion-aux (mover players))
   generaciones
   (cadr players))
  )

(define (startMatch players generaciones bola)
  (begin
    (dibujarCancha)
    (escribirDatos '(1 0) 12)
    (display "Generacion: ")
    (display generaciones)
    (newline)
    
    (cond ((< generaciones 20)
           (startMatchAux
            (estela players 0 '() (hipotenusa (car players)) (obtenerAngulo (car players)) bola)
            (+ generaciones 1)))
          )
    
    
    (copy-viewport window2 window)
    ((clear-viewport window2))
    ))

(define (makeOneList equipos)
  (append (car equipos) (cadr equipos))
  )

(define tt (mover-aux (inicializacion1 '(4 4 2)) (inicializacion2 '(4 3 3)) firstBall))

(define (CCCE2019 form1 form2 iteraciones)
  (cond ((or (null? form1) (null? form2)) '())
        (else
         (begin
           
           
           (startMatch (makeOneList tt) 1 firstBall)
           ))
  ))



(CCCE2019 '(4 4 2) '(4 3 3) 20)