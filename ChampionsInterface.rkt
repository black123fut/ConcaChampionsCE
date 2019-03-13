;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ChampionsInterface) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)


(open-graphics)

(define window (open-viewport "ConcaChampionsCE" 920 720))
(define window2 (open-pixmap "ConcaChampionsCE" 920 720))

;;(posicionI), fuerza-distancia, angulo
(define firstBall '((450 260) 0 45))


;;Escribe en la pantalla el marcador y las generaciones.
;;E: El marcador, y las generaciones.
;;S: El marcador y las generaciones en la pantalla.
(define (escribirDatos marcador generacion)
  (begin
    ;;(overlay/xy texto 430 600 linea)
    ((draw-string window2) (make-posn 430 600) (number->string (car marcador)) "white")
    ((draw-string window2) (make-posn 490 600) (number->string (cadr marcador)) "white")
    ((draw-string window2) (make-posn 800 600) "Geracion: " "white")
    ((draw-string window2) (make-posn 820 615) (number->string generacion) "white")
    ))

;;Dibuja la cancha.
;;E: No tiene.
;;S: La cancha en pantalla.
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

;;Borra la pantalla 2 y pega la pantalla 2 en la principal.
;;E: No tiene.
;;S: Borra la pantalla y la pega en la principal.
(define (drawWindow)
  (begin
    (copy-viewport window2 window)
    ((clear-viewport window2))
    )
  )

;;Obtiene a que equipo pertenece el jugador.
;;E: El jugador.
;;S: El equipo del jugador.
(define (obtenerEquipo jugador)
  (caddr (cdddr jugador)))

;;Obtiene la posicion inicial en X del jugador.
;;E: El jugador.
;;S: La posicion inicial en X.
(define (posX jugador)
  (car (obtenerXY jugador)))

;;Obtiene la posicion inicial en Y del jugador.
;;E: El jugador.
;;S: La posicion inicial en Y.
(define (posY jugador)
  (cadr (obtenerXY jugador)))

;;Obtiene la posicion final en X del jugador.
;;E: El jugador.
;;S: La posicion final en X.
(define (destinoX jugador)
  (car (obtenerXYf jugador)))

;;Obtiene la posicion final en Y del jugador.
;;E: El jugador.
;;S: La posicion final en Y.
(define (destinoY jugador)
  (cadr (obtenerXYf jugador)))

;;Cambia la posicion final a la inicial en un jugador.
;;E: El jugador.
;;S: El jugador con la posicion final como inicial.
(define (terminoMovimiento jugador)
  (cons (list (destinoX jugador) (destinoY jugador)) (cdr jugador)))

;;La magnitud total que debe recorrer el jugador.
;;E: El jugador.
;;S: El resultado de la operacion para sacar la hipotenusa.
(define (hipotenusa jugador)
  (cond ((null? jugador) 0)
        (else (sqrt (+ (expt (- (destinoX jugador) (posX jugador)) 2)
          (expt (- (destinoY jugador) (posY jugador)) 2))))
        ))

;;Da la posicion en X en la que se tiene que colocar el jugador respecto al contador(step).
;;E: La lista de jugadores, el contador, la magnitud que tiene que recorrer, el angulo en el que se mueve el jugador.
;;S: La posicion en X que se tiene que colocar el jugador.
(define (movimientoX jugadores step diagonal angulo)
  (cond ((null? jugadores) 0)
        (else (+ (posX (car jugadores)) (* step (cos (degrees->radians angulo))))))
  )

;;Da la posicion en Y en la que se tiene que colocar el jugador respecto al contador(step).
;;E: La lista de jugadores, el contador, la magnitud que tiene que recorrer, el angulo en el que se mueve el jugador.
;;S: La posicion en Y  que se tiene que colocar el jugador.
(define (movimientoY jugadores step diagonal angulo)
  (cond ((null? jugadores) 0)
        (else (+ (posY (car jugadores)) (* step (sin (degrees->radians angulo))))))
  )

;;Obtiene el siguiente jugador de la lista.
;;E: La lista de jugadores.
;;S: El siguiente jugador en la lista.
(define (getNextPlayer jugadores)
  (cond ((null? jugadores) '())
        (else (car jugadores))
        ))

;;Obtiene la magntitud del vector o la pentiente.
;;E: Un jugador.
;;S: El resultado de esta operacion.
(define (pendiente jugador)
  (cond ((zero? (- (destinoX jugador) (posX jugador)))
         (/ (- (destinoY jugador) (posY jugador)) 0.5))
        (else (/ (- (destinoY jugador) (posY jugador)) (- (destinoX jugador) (posX jugador))))
        )
  )

;;Da una lista con el denominador y el numerador de la diferencia entre el punto inicial y el final.
;;E: Un jugador.
;;S: La lista con la diferencia entre el punto final y el inicial.
(define (obtenerAnguloAux jugador)
  (list (- (destinoY jugador) (posY jugador)) (- (destinoX jugador) (posX jugador))))

;;Obtiene el angulo entre el punto inicial y el punto final.
;;E: Un jugador.
;;S: El angulo que debe seguir el jugador.
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

;;Dibuja los jugadores.
;;E: La lista de jugadores.
;;S: Los jugadores en la pantalla.
(define (dibujarJugadores jugadores number)  
  (cond ((null? jugadores) #t)
        (else (begin
                ((draw-solid-ellipse window2) (make-posn (posX (car jugadores)) (posY (car jugadores))) 30 30 (obtenerEquipo (car jugadores)))
                ((draw-string window2) (make-posn (+ (caaar jugadores) 13) (+ (cadar (car jugadores)) 15)) (number->string (obtenerNum (car jugadores))) "white")
                (dibujarJugadores (cdr jugadores) (+ 1 number))
                ))))

;;Verifica que el resto de jugadores tengan alguna colision con la bola.
;;E: La lista de jugadores, la lista de la bola.
;;S:En caso de intersectar pasa la fuerza del jugador a la bola.
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

;;Verifica si el jugador en movimiento intersecto con el balon.
;;E: El jugador, su posicion en X y Y, la lista de la bola, y la lista de jugadores.
;;S: En caso de intersectar pasa la fuerza del jugador a la bola.
(define (interseccion jugador xm ym bola jugadores)
  (cond ((and (< xm (+ (posX bola) 20)) (< ym (+ (posY bola) 20)) (> (+ xm 30) (posX bola)) (> (+ ym 30) (posY bola)))
                                                       ;;Hay va el angulo del jugador 
         (list (car bola) (+ 10 (obtenerFuerza jugador)) (anguloTiro jugador))
         )
        (else (interseccionAux jugadores bola))
        ))

;;Obtiene la fuerza que esta en la lista de la bola.
;;E: La lista de la bola.
;;S: La fuerza que tiene la bola.
(define (fuerzaBola bola)
  (cond ((not (list? bola)) 0)
        (else (cadr bola))))

;;Obtiene el angulo que tiene la lista de la bola.
;;E: La lista de la bola.
;;S: El angulo de la bola.
(define (anguloBola bola)
  (cond ((number? bola) bola)
        (else (caddr bola)))
  )
                                          
;;Verifica si la toco un borde del campo para rebotar.
;;E: La lista de la bola.
;;S: La bola con un angulo sumado 240 para dar la esencia de haber rebotado.
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

;;Verifica si la bola traspaso el marco del equipo rival.
;;E: La lista de la bola, el marcador, la lista de jugadores, y las generaciones.
;;S: Llama a la funcion para alinear a los jugadores con un nuevo marcador.
(define (verificarGol bola marcador jugadores generaciones)
  (cond ((and (> (posX bola) 900) (or (> (posY bola) 165) (< (posY bola) 360)))
         (startMatchGol jugadores generaciones (list (+ (car marcador) 1) (cadr marcador))))
        ((and (< (posX bola) -5) (or (> (posY bola) 165) (< (posY bola) 360)))
         (startMatchGol jugadores generaciones (list (car marcador) (+ (cadr marcador) 1))))
        (else marcador)
        )
  )

;;Disminuye gradualmente la fuerza que posee la bola.
;;E: La lista de la bola.
;;S: La bola en una nueva posicion si esta tenia fuerza.
(define (cambiarFuerzaBola bola)
    (cond ((> (fuerzaBola bola) 0)
                                              ;;Duracion del tiro
           (verificaLimites (list
                             (list (+ (posX bola) (* (fuerzaBola bola) (cos (degrees->radians (anguloBola bola)))))
                                   (+ (posY bola) (* (fuerzaBola bola) (sin (degrees->radians (anguloBola bola))))))
                             (- (fuerzaBola bola) 0.3) (anguloBola bola))))
          ((number? bola) (begin
                            (display "coso: ")
                            (display bola) (newline)
                            (cambiarFuerzaBola (list (list 450 260) -1 0))
                            ))
          (else (list (car bola) (fuerzaBola bola) (anguloBola bola)))
          ))

;;Dibuja la bola.
;;E: La lista de la bola.
;;S: La bola dibuja en la pantalla.
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

;;Vuelve a dar una posicion final al jugador para que siga la bola con mas presicion.
;;E: La lista de jugadores, los nuevos, y la bola.
;;S: La lista de jugadores con el primer jugadores con una posicion final nueva.
(define (remplazarDestino jugadores nuevos bola)
  (cond ((null? jugadores) '())
        ((equal? (obtenerEquipo (car jugadores)) "purple")
         (append (cons (mover_a_bola_S (car jugadores) nuevos bola) '()) (cdr jugadores)))
        (else
         (append (cons (mover_a_bola (car jugadores) nuevos bola) '()) (cdr jugadores)))
        ))

;;Obtiene el resto de una lista.
;;E: Una lista.
;;S: Una lista vacio o el resto de elementos en la lista.
(define (obtenerResto jugadores)
  (cond ((null? jugadores) '())
        (else (cdr jugadores))
        ))

;;Simplifica la llamada recursiva de estela, a parecerla mas pequena.
;;E: Los jugadores, contador de la hipotenusa, la lista con los jugadores que ya se movieron,
;; magnitud que debe recorrer el jugador, angulo, bola, marcador, generaciones.
;;S: Llama a estela para seguir la recursion.
(define (estela-aux jugadores step nuevosJugadores diagonal angulo bola marcador generaciones)
  (estela jugadores 0
          nuevosJugadores
          (hipotenusa (getNextPlayer jugadores))
          (obtenerAngulo (getNextPlayer jugadores))
          (cambiarFuerzaBola (interseccion
                              (getNextPlayer jugadores)
                              (movimientoX jugadores step diagonal angulo)
                              (movimientoY jugadores step diagonal angulo)
                              bola (append nuevosJugadores (obtenerResto jugadores))))
          marcador generaciones))

;;Mueve a los jugadores de su posicion inicial a la final de manera que paresca que dibuja una estela.
;;E: Los jugadores, contador para la hipotenusa, lista donde se guardan los jugadores que ya se movieron,
;; magnitud que debe recorrer el jugador, angulo que sigue el jugador, bola, marcador, generaciones.
;;S: Una lista con los jugadores con la posicion final como incial y la bola en la ultima posicion que se situo.
(define (estela jugadores step nuevosJugadores diagonal angulo bola marcador generaciones)
  (begin
    ;;(sleep 1)
    (dibujarCancha)
    (escribirDatos marcador generaciones)
    (cond ((null? jugadores)
           (cond
             ((not (< (fuerzaBola bola) 0))
               (begin
                 (dibujarJugadores  nuevosJugadores 1)
                 (dibujarBola (interseccionAux nuevosJugadores bola))
                 (drawWindow)
                 (estela '() 0 nuevosJugadores 0 0
                                 (cambiarFuerzaBola (fuerzaBola (interseccionAux nuevosJugadores bola)))
                                 (verificarGol bola marcador (append nuevosJugadores jugadores) generaciones) generaciones)))
             (else (list nuevosJugadores bola))))
        ((< diagonal step)
         (estela-aux
          (remplazarDestino (cdr jugadores) nuevosJugadores bola) 0
          (cons (terminoMovimiento (car jugadores)) nuevosJugadores)
          diagonal angulo bola (verificarGol bola marcador (append nuevosJugadores jugadores) generaciones) generaciones))        
        (else
         (begin
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
                                                            (append nuevosJugadores (cdr jugadores))))
                           (verificarGol bola marcador (append jugadores nuevosJugadores) generaciones) generaciones))))))

;;Simplifica la llamada recursiva de startMatch.
;;E: Los jugadores, las generaciones, y el marcador.
;;S: Llama a startMatch con la siguiente generacion de jugadores.
(define (startMatchAux players generaciones marcador)
  (startMatch
   (seleccion-reproduccion-aux (mover players))
   generaciones (cadr players) marcador)
  )

;;En caso de que se marque un gol vuelve a alinear a los jugadores.
;;E: Los jugadores, las generaciones, y el marcador.
;;S: Los jugadores alineados.
(define (startMatchGol players generaciones marcador)
  (startMatch (alinear (dividirEquipos players))
              generaciones '((450 260) 0 0)
              marcador))

;;Se encarga de manejar cada generacion y el marcador del juego. 
;;E: Los jugadores, la cantidad de generaciones, la bola, y el marcador inicial.
;;S: El resultado del partido, o la siguiente generacion.
(define (startMatch players generaciones bola marcador)
  (begin
    (dibujarCancha)
    (escribirDatos marcador generaciones)
    (display "Generacion: ")
    (display generaciones)
    (newline)
    
    (cond ((or (> (car marcador) 2) (> (cadr marcador) 2))
           (escribirDatos marcador generaciones))
          ((< generaciones 20)
           (startMatchAux
            (estela players 0 '() (hipotenusa (car players)) (obtenerAngulo (car players)) bola
                    marcador generaciones)
            (+ generaciones 1) marcador))
          (else (escribirDatos marcador generaciones))
          
          )
    
    
    (copy-viewport window2 window)
    ((clear-viewport window2))
    ))

;;Hace una lista con dos sublistas en una sola lista.
;;E: Una lista con dos sublistas.
;;S: Una sola lista.
(define (makeOneList equipos)
  (append (car equipos) (cadr equipos))
  )

(define firstPlayers (mover-aux (inicializacion1 '(4 4 2)) (inicializacion2 '(4 3 3)) firstBall))

;;Inicializa el partido.
;;E: Formacion de los equipos y la cantidad de generaciones.
;;S: Un partido de futbol.
(define (CCCE2019 form1 form2 iteraciones)
  (cond ((or (null? form1) (null? form2)) '())
        (else
         (begin
           (startMatch (makeOneList firstPlayers) 1 firstBall '(0 0))
           ))
  ))



(CCCE2019 '(4 4 2) '(4 3 3) 20)