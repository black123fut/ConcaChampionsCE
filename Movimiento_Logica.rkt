;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Movimiento_Logica) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Agregar al codigo
;Argumentos: Listas Equipo1 y Equipo2
;Salida: Lista con ambos equipos y sus posiciones finales modificadas
;Funcion principal para mover los 2 equipos, llama a su debida funcion de movimiento
(define (mover jugadores jugadores_S ball)
  (cond((or (null? jugadores) (null? jugadores)) '())
       (else
        (list (mover_jugadores jugadores '() ball) (mover_jugadores_S jugadores_S '() ball)))))

;Argumentos: La lista de jugadores y una lista vacia
;Salida: Lista de jugadores con nueva posicion
;Funcion que toma toda la lista de jugadores y crea una nueva lista con sus posiciones finales modificadas
(define (mover_jugadores_S jugadores listaF ball)
  (cond ((null? jugadores) listaF)
        (else
         (mover_jugadores_S (cdr jugadores) (append listaF (list (mover_a_bola_S (car jugadores) listaF  ball))) ball))))


;Argumentos: Jugador y Posicion Final
;Salida: Llama a la funcion correspondiente
;Compruba que tipo de jugador es, y que tipo de movimiento deberia realizar
(define(comprobar_tipo_S jugador posXYf)
  (cond((equal? (obtenerNumTipo jugador) 1) (moverPortero_S jugador posXYf))
       ((equal? (obtenerNumTipo jugador) 2) (moverDefensa_S jugador posXYf))
       ((equal? (obtenerNumTipo jugador) 3) (moverMedio_S jugador posXYf))
       ((equal? (obtenerNumTipo jugador) 4) (moverDelantero_S jugador posXYf))))

;Argumentos: Jugador y Posicion Final
;Salida: Jugador con una nueva posicion final
;Portero-Defensa-Delantero-Medios(Equipo 2)

;Funcion que identifica si el portero se puede mover a dicha ubicacion, si no es el caso, genera una nueva ubicacion
(define (moverPortero_S jugador posXYf)
  (cond ((not (equal? (car posXYf) 895)) (moverPortero_S jugador (cons 895 (cdr posXYf))))
        ((< (cadr posXYf) 180) (moverPortero_S jugador (list 895 180)))
        ((> (cadr posXYf) 330) (moverPortero_S jugador (list 895 330)))
        (else
         (append (list (car jugador)) (list posXYf) (cddr jugador)))))


;Funcion que identifica si el defensa se puede mover a dicha ubicacion, si no es el caso, genera una nueva ubicacion
(define (moverDelantero_S jugador posXYf)
  
  (cond 
    ((> (car posXYf) 230) (moverDelantero_S jugador (list 230 (cadr posXYf))))
        (else
         {append (list (car jugador)) (list posXYf) (cddr jugador)})))
 ;Funcion que identifica si el Medio se puede mover a dicha ubicacion, si no es el caso, genera una nueva ubicacion
(define (moverMedio_S jugador posXYf)
  (cond ((< (car posXYf) 230) (moverMedio_S jugador (list 230 (cadr posXYf))))
        ((> (car posXYf) 690) (moverMedio_S jugador (list 690 (cadr posXYf))))
        (else
         {append (list (car jugador)) (list posXYf) (cddr jugador)})))
 ;Funcion que identifica si el delantero se puede mover a dicha ubicacion, si no es el caso, genera una nueva ubicacion
(define (moverDefensa_S jugador posXYf)
  (cond ((< (car posXYf) 690) (moverDefensa_S jugador (list 690 (cadr posXYf))) )
        ((> (car posXYf) 900) (moverDefensa_S jugador (list 900 (cadr posXYf))))
        (else
         {append (list (car jugador)) (list posXYf) (cddr jugador)})))


;Argumentos: Jugador, bola y lista Final
;Salida: Tipo de movimiento a realizar o comprobar que tipo de jugador
;;Comprueba si un jugador del equipo 2 se puede mover hacia el balon
(define (mover_a_bola_S  jugador listaF ball)

 (cond ((and (<= (abs (- (car(obtenerXY jugador)) (caar ball))) (* (obtenerAgilidad jugador) 15)) (<= (abs (- (cadr(obtenerXY jugador)) (cadar ball))) (* (obtenerAgilidad jugador) 15)) (comprobar_otro_jugador (car ball) listaF)) (comprobar_tipo_S jugador  (car ball)))
        
         ((equal? (obtenerNumTipo jugador) 1)
          (moverPortero_S jugador (list 895 (+ (random 150) 180) )))
         ((equal? (obtenerNumTipo jugador) 4)
          (moverDelantero_S jugador (list  (+ (random 225) 5) (+ (random 505) 5) )))
         
         ((equal? (obtenerNumTipo jugador) 3)
          (moverMedio_S jugador (list (+ (random 460) 230) (+ (random 505) 5) )))
         ((equal? (obtenerNumTipo jugador) 2)
          (moverDefensa_S jugador (list (+ (random 210) 690) (+ (random 505) 5) )))
         
         ))

;Argumentos: La lista de jugadores y una lista vacia
;Salida: Lista de jugadores con nueva posicion
;Funcion que reliaza el movimento del equipo del lado izquierda
(define (mover_jugadores jugadores listaF ball)
  (cond ((null? jugadores) listaF)
        (else
         (mover_jugadores (cdr jugadores) (append listaF (list (mover_a_bola (car jugadores) listaF ball))) ball))))

;Argumentos: Jugador y Posicion Final
;Salida: Llama a la funcion correspondiente
;Funcion que comprueba el tipo de jugador el cual se desea mover
(define(comprobar_tipo jugador posXYf )
  
  (cond((equal? (obtenerNumTipo jugador) 1) (moverPortero jugador posXYf))
       ((equal? (obtenerNumTipo jugador) 2) (moverDefensa jugador posXYf))
       ((equal? (obtenerNumTipo jugador) 3) (moverMedio jugador posXYf))
       ((equal? (obtenerNumTipo jugador) 4) (moverDelantero jugador posXYf))))
       
 
;Argumentos: Jugador y Posicion Final
;Salida: Jugador con una nueva posicion final
;Portero-Defensa-Delantero-Medios(Equipo 1)

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



;Argumentos: Jugador, bola y lista Final
;Salida: Tipo de movimiento a realizar o comprobar que tipo de jugador
;Funcion que verifica si un jugador puede moverse directamente a la bola 

(define (mover_a_bola  jugador listaF ball)
  

 (cond ((and (<= (abs (- (car(obtenerXY jugador)) (caar ball))) (* (obtenerAgilidad jugador) 15)) (<= (abs (- (cadr(obtenerXY jugador)) (cadar ball))) (* (obtenerAgilidad jugador) 15)) (comprobar_otro_jugador (car ball) listaF)) (comprobar_tipo jugador  (car ball)))
        
         ((equal? (obtenerNumTipo jugador) 1)
          (moverPortero jugador (list 5 (+ (random 150) 180) )))
         ((equal? (obtenerNumTipo jugador) 2)
          (moverDefensa jugador (list  (+ (random 225) 5) (+ (random 505) 5) )))
         
         ((equal? (obtenerNumTipo jugador) 3)
          (moverMedio jugador (list (+ (random 460) 230) (+ (random 505) 5) )))
         ((equal? (obtenerNumTipo jugador) 4)
          (moverDelantero jugador (list (+ (random 210) 690) (+ (random 505) 5) )))
         
         ))

;Argumentos: Posicion deseada y lista nueva de jugadores
;Salida: #f si existe un jugador en esa posicion, #t si no hay ningun jugador en esa posicion
;Comprueba el si existe otro jugador en la posicion ya selecionada         
(define (comprobar_otro_jugador posXYf listaF)

  
  (cond((null? listaF) #t)
       ((equal? (obtenerXYf (car listaF)) posXYf) #f)
       (else
        (begin
          
         (display posXYf)
        (comprobar_otro_jugador posXYf (cdr listaF))))))



;Argumento: Jugador
;Salida:Caracteristica especifica del jugador

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
;Obtiene el angulo para tirar segun su punteria
(define (angulo jugador)
  (cond ((null? jugador)
         '())
        (else
         (* (* (obtenerPunteria jugador) (/ 9 10)) 10))))

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



;-------------------------------------------------------------FINALIZA EL CODIGO

;;Pruebas de listas de jugadores


(define (listajugadores  jugador1 jugador2 jugador3 jugador4 jugador5 jugador6 jugador7 jugador8 jugador9 jugador10 jugador11)
  (list jugador1 jugador2 jugador3 jugador4 jugador5 jugador6 jugador7 jugador8 jugador9 jugador10 jugador11)
 )

(define (jugadores)
  (listajugadores (jugador '(100 30) '(200 6) '(8 5 10 4 1) 1 1)
                  (jugador '(100 130) '(100 60) '(8 5 3 2 2) 2 2) (jugador '(150 60) '(100 60) '(8 5 3 2 5) 2 3) (jugador '(100 180) '(100 60) '(8 5 3 2 8) 2 4) (jugador '(100 60) '(100 60) '(8 5 3 2 10) 2 5)
                  (jugador '(100 130) '(100 60) '(8 5 3 2 3) 3 6) (jugador '(100 60) '(199 11) '(8 5 3 2 6) 3 7) (jugador '(100 130) '(100 60) '(8 5 3 2 9) 3 8) (jugador '(100 60) '(100 60) '(8 5 3 2 11) 3 9)
                  (jugador '(100 130) '(100 60) '(8 5 3 2 4) 4 10) (jugador '(100 180) '(100 60) '(8 5 3 2 7) 4 11))
 )

(define (jugadores2)
  (listajugadores (jugador '(100 30) '(200 6) '(8 5 3 4 1) 1 12)
                  (jugador '(100 130) '(100 60) '(8 5 3 2 2) 2 13) (jugador '(150 60) '(100 60) '(8 5 3 2 5) 2 14) (jugador '(100 180) '(100 60) '(8 5 3 2 8) 2 15) (jugador '(100 60) '(100 60) '(8 5 3 2 10) 2 16)
                  (jugador '(100 130) '(100 60) '(8 5 3 2 3) 2 17) (jugador '(100 60) '(199 11) '(8 5 3 2 6) 3 18) (jugador '(100 130) '(100 60) '(8 5 3 2 9) 3 19) (jugador '(100 60) '(100 60) '(8 5 3 2 11) 3 20)
                  (jugador '(100 130) '(100 60) '(8 5 3 2 4) 3 21) (jugador '(100 180) '(100 60) '(8 5 3 2 7) 4 22))
 )





(mover (jugadores) (jugadores2)  '((457 150) 1 1))





;;(jugador 10 10 10 10 )
;;(jugador 1 10 10 10 )
;;(jugador '(10 60) 10 10 10 )
