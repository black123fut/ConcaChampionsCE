;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Movimiento_Logica) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (eliminar elem lista)
  (cond ((null? lista) '())
        ((equal? elem (car lista))
         (eliminar elem (cdr lista)))
        (else
         (cons (car lista) (eliminar elem (cdr lista))))))


(define (mover_jugadores jugadores listaF)
  (cond ((null? jugadores) listaF)
        (else
         (mover_jugadores (cdr jugadores) (append listaF (list (mover_a_bola (car jugadores) (bola) listaF)))))))
;;creacion de los jugadores
(define (jugador posXY posXYf estadisticas num-tipo numero)
  (cond ((not(list? posXY)) #f)
        
        ((not (list? posXYf)) #f)
        
        ((not (list? estadisticas)) #f)
        ((not (number? num-tipo)) #f)
        ((not (number? numero)) #f)
        (else
         (list posXY posXYf estadisticas num-tipo numero))
  ))


;;Movimiento de jugadores
;(define (mover_jugador jugador bola)
 ; (cond ((null? jugador) #F)
  ;      (else
   ;      (mover_a_bola  jugador bola))))

;Funcion que comprueba el tipo de jugador el cual se desea mover
(define(comprobar_tipo jugador posXYf)
  (cond((equal? (obtenerNumTipo jugador) 1) (moverPortero jugador posXYf))
       ((equal? (obtenerNumTipo jugador) 2) (moverDefensa jugador posXYf))
       ((equal? (obtenerNumTipo jugador) 3) (moverMedio jugador posXYf))
       ((equal? (obtenerNumTipo jugador) 4) ((moverDelantero jugador posXYf)))))

 ;Funcion que identifica si el portero se puede mover a dicha ubicacion, si no es el caso, genera una nueva ubicacion
(define (moverPortero jugador posXYf)
  (cond ((not (equal? (car posXYf) 5)) (moverPortero jugador (cons 5(cdr posXYf))))
        ((< (cadr posXYf) 180) (moverPortero jugador (list 5 180)))
        ((> (cadr posXYf) 330) (moverPortero jugador (list 5 330)))
        (else
         (append (list (car jugador)) (list posXYf) (cddr jugador)))))

;Funcion que identifica si el defensa se puede mover a dicha ubicacion, si no es el caso, genera una nueva ubicacion
(define (moverDefensa jugador posXYf)
  
  (cond 
    ((> (car posXYf) 230) (moverDefensa jugador (list 230 (cadr posXYf))))
        (else
         {append (list (car jugador)) (list posXYf) (cddr jugador)})))
 ;Funcion que identifica si el Medio se puede mover a dicha ubicacion, si no es el caso, genera una nueva ubicacion
(define (moverMedio jugador posXYf)
  (cond ((< (car posXYf) 230) (moverMedio jugador (list 230 (cadr posXYf))))
        ((> (car posXYf) 690) (moverMedio jugador (list 690 (cadr posXYf))))
        (else
         {append (list (car jugador)) (list posXYf) (cddr jugador)})))
 ;Funcion que identifica si el delantero se puede mover a dicha ubicacion, si no es el caso, genera una nueva ubicacion
(define (moverDelantero jugador posXYf)
  (cond ((< (car posXYf) 690) (moverMedio jugador (list 690 (cadr posXYf))) )
        ((> (car posXYf) 900) (moverMedio jugador (list 900 (cadr posXYf))))
        (else
         {append (list (car jugador)) (list posXYf) (cddr jugador)})))

 ;Funcion que verifica si un jugador puede moverse directamente a la bola 

(define (mover_a_bola  jugador bola listaF)

 (cond ((and (<= (abs (- (car(obtenerXY jugador)) (caar bola))) (* (obtenerAgilidad jugador) 15)) (<= (abs (- (cadr(obtenerXY jugador)) (cadar bola))) (* (obtenerAgilidad jugador) 15)) (comprobar_otro_jugador (car bola) listaF)) (comprobar_tipo jugador  (car bola)))
        
         ((equal? (obtenerNumTipo jugador) 1)
          (moverPortero jugador (list 5 (+ (random 150) 180) )))
         ((equal? (obtenerNumTipo jugador) 2)
          (moverDefensa jugador (list (+ (random 505) 5) (+ (random 225) 5) )))
         
         ((equal? (obtenerNumTipo jugador) 3)
          (moverMedio jugador (list (+ (random 505) 5) (+ (random 460) 230) )))
         ((equal? (obtenerNumTipo jugador) 4)
          (moverDelantero jugador (list (+ (random 505) 5) (+ (random 210) 690) )))
         
         ))
         
(define (comprobar_otro_jugador posXYf listaF)
  (cond((null? listaF) #t)
       ((equal? (obtenerXYf (car listaF)) posXYf) #f)
       (else
        (comprobar_otro_jugador posXYf (cdr listaF)))))
;Funcion que comprueba que si un jugador ya se encuentra en esa posicion
;(define (comprobar_pos jugador jugadores)
 ; (cond((null? jugadores) #t)
  ;     (equal? (obtener
   ;     ))).
;;Pruebas de listas de jugadores


(define (listajugadores  jugador1 jugador2 jugador3 jugador4 jugador5 jugador6 jugador7 jugador8 jugador9 jugador10 jugador11)
  (list jugador1 jugador2 jugador3 jugador4 jugador5 jugador6 jugador7 jugador8 jugador9 jugador10 jugador11)
 )
(define (bola)
   '((200 30) 1 1))
(define (jugadores)
  (listajugadores (jugador '(100 30) '(200 6) '(8 5 3 4 1) 1 1)
                  (jugador '(100 130) '(100 60) '(8 5 3 2 2) 2 2) (jugador '(150 60) '(100 60) '(8 5 3 2 5) 2 3) (jugador '(100 180) '(100 60) '(8 5 3 2 8) 2 4) (jugador '(100 60) '(100 60) '(8 5 3 2 10) 2 5)
                  (jugador '(100 130) '(100 60) '(8 5 3 2 3) 3 6) (jugador '(100 60) '(199 11) '(8 5 3 2 6) 3 7) (jugador '(100 130) '(100 60) '(8 5 3 2 9) 3 8) (jugador '(100 60) '(100 60) '(8 5 3 2 11) 3 9)
                  (jugador '(100 130) '(100 60) '(8 5 3 2 4) 4 10) (jugador '(100 180) '(100 60) '(8 5 3 2 7) 4 11))
 )



;;FIn de las pruebas


;;Funciones para obtener datos de un jugador en especifico
;;Obtiene las posiciones iniciales X y Y
(define (obtenerXY jugador)
  (cond((null? jugador) '())
       (else
        (car jugador))))

;;Obtiene las posiciones finales X y Y
(define (obtenerXYf jugador)
  (cond((null? jugador) '())
       (else
        (cadr jugador))))
;Obtiene las estadisticas
(define (obtenerEst jugador)
  (cond ((null? jugador)
         '())
        (else
         (obtenerEst_aux jugador 0))))
(define (obtenerEst_aux jugador num)
  (cond((equal? num 2) (car jugador))
       (else
        (obtenerEst_aux (cdr jugador) (+ num 1)))))
;;Obtiene la Aptitud
(define (obtenerAptitud jugador)
  (cond((null? jugador) '())
       (else
        (car (obtenerEst jugador))
  )))

;;Obtiene la fuerza
(define (obtenerFuerza jugador)
  (cond((null? jugador) '())
       (else
        (obtenerFuerza_aux (obtenerEst jugador) 0)
  )))
(define (obtenerFuerza_aux estadistica num)
  (cond((equal? num 1) (car estadistica))
       (else
        (obtenerFuerza_aux (cdr estadistica) (+ num 1)))))

;;Obtiene la punteria
(define (obtenerPunteria jugador)
  (cond((null? jugador) '())
       (else
        (obtenerPunteria_aux (obtenerEst jugador) 0)
  )))
(define (obtenerPunteria_aux estadistica num)
  (cond((equal? num 2) (car estadistica))
       (else
        (obtenerPunteria_aux (cdr estadistica) (+ num 1)))))

;;Obtiene la velocidad
(define (obtenerVelocidad jugador)
  (cond((null? jugador) '())
       (else
        (obtenerVelocidad_aux (obtenerEst jugador) 0)
  )))
(define (obtenerVelocidad_aux estadistica num)
  (cond((equal? num 3) (car estadistica))
       (else
        (obtenerVelocidad_aux (cdr estadistica) (+ num 1)))))

;;Obtiene la agilidad
(define (obtenerAgilidad jugador)
  (cond((null? jugador) '())
       (else
        (obtenerAgilidad_aux (obtenerEst jugador) 0)
  )))
(define (obtenerAgilidad_aux estadistica num)
  (cond((equal? num 4) (car estadistica))
       (else
        (obtenerAgilidad_aux (cdr estadistica) (+ num 1)))))

;Obtiene numero de tipo
(define (obtenerNumTipo jugador)
  (cond((null? jugador) '())
       (else
        (obtenerNumTipo_aux jugador 0)
  )))

(define (obtenerNumTipo_aux jugador num)
  (cond((equal? num 3) (car jugador))
       (else
        (obtenerNumTipo_aux (cdr jugador) (+ num 1)))))
;;Obtiene el numero
(define (obtenerNum jugador)
  (cond((null? jugador) '())
       (else
        (obtenerNum_aux jugador 0)
  )))

(define (obtenerNum_aux jugador num)
  (cond((equal? num 4) (car jugador))
       (else
        (obtenerNum_aux (cdr jugador) (+ num 1)))))

(print "Jugador: ")

(car(jugadores))
(print "Posiciones iniciales: ")
(obtenerXY (car(jugadores)))
(print "Posiciones Finales: ")
(obtenerXYf (car(jugadores)))
(print "Estadisticas")
(obtenerEst (car(jugadores)))
(print "Aptitud: ")
(obtenerAptitud(car(jugadores)))
(print "Fuerza: ")
(obtenerFuerza(car(jugadores)))
(print "Puneria/habilidad: ")
(obtenerPunteria(car(jugadores)))
(print "Velocidad: ")
(obtenerVelocidad (car(jugadores)))
(print "Agilidad:")
(obtenerAgilidad (car(jugadores)))
(print "Numero de Tipo:")
(obtenerNumTipo (car(jugadores)))
(print "Numero:")
(obtenerNum (car(jugadores)))
(print "mover que tipo jugador:")
;(mover_jugador (car(jugadores)) (bola))

(print "------JUGADORES----")
(display (jugadores))
(display (newline))
(print "------Jugadores_Mover-------")
(display (mover_jugadores (jugadores) '()))
;;(jugador 10 10 10 10 )
;;(jugador 1 10 10 10 )
;;(jugador '(10 60) 10 10 10 )
