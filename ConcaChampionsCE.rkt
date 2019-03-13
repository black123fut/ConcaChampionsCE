;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ConcaChampionsCE) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require graphics/graphics)
(require 2htdp/image)
(require racket/math)

;;Agregar al codigo
;Argumentos: Listas Equipo1 y Equipo2
;Salida: Lista con ambos equipos y sus posiciones finales modificadas
;Funcion principal para mover los 2 equipos, llama a su debida funcion de movimiento
(define (mover listaGrande)
  (mover-aux (cadr (dividirEquipos (car listaGrande))) (car (dividirEquipos (car listaGrande))) (cadr listaGrande))
  )

;Argumentos: Listas Equipo1 y Equipo2
;Salida: Lista con ambos equipos y sus posiciones finales modificadas
;Funcion principal para mover los 2 equipos, llama a su debida funcion de movimiento
(define (mover-aux jugadores jugadores_S ball)
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
 (cond ((and (<= (abs (- (car(obtenerXY jugador)) (caar ball))) (+ (* (obtenerAgilidad jugador) 15) 150))
             (<= (abs (- (cadr(obtenerXY jugador)) (cadar ball))) (+ (* (obtenerAgilidad jugador) 15) 150))
             (comprobar_otro_jugador (car ball) listaF))
        (comprobar_tipo_S jugador  (car ball)))
        
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
 (cond ((and (<= (abs (- (car(obtenerXY jugador)) (caar ball))) (+ (* (obtenerAgilidad jugador) 15) 150))
             (<= (abs (- (cadr(obtenerXY jugador)) (cadar ball))) (+ (* (obtenerAgilidad jugador) 15) 150))
             (comprobar_otro_jugador (car ball) listaF))
        (comprobar_tipo jugador  (car ball)))
        
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
       
        (comprobar_otro_jugador posXYf (cdr listaF)))))

;Argumento: Jugador
;Salida:Caracteristica especifica del jugador

;;Funciones para obtener datos de un jugador en especifico
;;Obtiene las posiciones iniciales X y Y
(define (obtenerXY jugador)
  (cond((or (null? jugador) (number? jugador)) '(50 50))
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

;;Divide la lista de jugadores en dos
(define (dividirEquiposAux jugadores num nuevos)
  (cond ((> num 10) (list nuevos jugadores))
        (else (dividirEquiposAux (cdr jugadores) (+ num 1) (cons (car jugadores) nuevos)) ))
  )

(define (dividirEquipos jugadores)
  (begin
    (display jugadores) (newline)
    (dividirEquiposAux jugadores 0 '() )
    ))

;Obtiene el angulo para tirar segun su punteria
(define (anguloTiro-aux jugador marco)
  
    (cond ((null? jugador)
         '())
        (else
          (+ (* (/ (- (obtenerPunteria jugador) 10) -0.2222) (expt -1 (random 2))) marco)  )
        )
    )

(define (anguloTiro jugador)
  (cond ((equal? (obtenerEquipo jugador) "purple")
         (anguloTiro-aux jugador (obtenerAngulo (list (obtenerXY jugador) '(0 250)))))
        (else
         (anguloTiro-aux jugador (obtenerAngulo (list (obtenerXY jugador) '(920 250))))
         ))
  )

;-------------------------------------------------------------------------------------------;;
;-------------------------------------------------------------------------------------------;;
;-------------------------------------------------------------------------------------------;;
;-------------------------------------------------------------------------------------------;;

;;Funcion de Aptitud--------------------------------------------------------------------------------------------------------
(define (aptitud-eq equipo)
  (cond((null? equipo) equipo)
       (else( cons (aptitud (car equipo)) (aptitud-eq (cdr equipo))))))

(define (aptitud jugador)
  (cond((null? jugador) jugador)
       (else (list(car jugador) (cadr jugador) (aptitud-aux (caddr jugador)) (car(cdddr jugador)) (cadr(cdddr jugador))))))

(define (aptitud-aux habilidades)
  (cond((null? habilidades) habilidades)
       (else(cons (suma (cdr habilidades)) (cdr habilidades)))))

(define(suma lista)
  (cond((null? lista) 0)
       (else(+ (car lista) (suma (cdr lista))))))
;;--------------------------------------------------------------------------------------------------------------------------

(define (obtenerFormacion jugadores  num1 num2 num3)
  (cond((null? jugadores) (list num1 num2 num3))
       ((equal? (obtenerNumTipo (car jugadores)) 2) (obtenerFormacion (cdr jugadores) (+ num1 1) num2 num3))
       ((equal? (obtenerNumTipo (car jugadores)) 3) (obtenerFormacion (cdr jugadores) num1 (+ num2 1) num3))
       ((equal? (obtenerNumTipo (car jugadores)) 4) (obtenerFormacion(cdr jugadores)  num1 num2 (+ num3 1)))
       (else
        (obtenerFormacion (cdr jugadores) num1 num2 num3))))

;;Funcion de Seleccion------------------------------------------------------------------------------------------------------
(define (seleccion-reproduccion-aux equipos)
  (append (seleccion-reproduccion (obtenerFormacion (car equipos) 0 0 0) (car equipos))
          (seleccion-reproduccion (obtenerFormacion (cadr equipos) 0 0 0) (cadr equipos)))
  )

(define (seleccion-reproduccion formacion equipo)
  (cond((null? equipo) equipo)
       (else
        (asignar-caracts
         (mutacion
          (reproduccion
           (crear-hijos (+ 1 (car formacion) (cadr formacion) (caddr formacion)) (mejores(retornar-caracts equipo)))))
         equipo))))

;;3
(define (crear-hijos num padres)
  (cond((even? num) (append (crearhijos-aux (quotient num 2) (car padres)) (crearhijos-aux (quotient num 2) (cadr padres))))
       (else(append (crearhijos-aux (+ 1 (quotient num 2)) (car padres)) (crearhijos-aux (quotient num 2) (cadr padres))))))

;;3.1
(define(crearhijos-aux num padre)
  (cond((= num 1) (list padre))
       (else (append (list padre) (crearhijos-aux (- num 1) padre)))))

;;1
(define (retornar-caracts equipo)
  (cond((null? equipo) equipo)
       (else(cons (caddr(car equipo)) (retornar-caracts (cdr equipo))))))
;;2
(define (mejores lista)
  (cond((null? lista) lista)
       (else
        (list (mayor (car lista) (cdr lista))
              (mayor (car(eliminar (mayor (car lista) (cdr lista)) lista))
                     (eliminar (mayor (car lista) (cdr lista)) lista))))))
;;2.1
(define (mayor actual lista)
  (cond((null? lista) actual)
       ((> (car actual) (caar lista)) (mayor actual (cdr lista)))
       ((< (car actual) (caar lista)) (mayor (car lista) (cdr lista)))
       (else(mayor actual (cdr lista)))))
;;2.2
(define(eliminar num lista)
  (cond((null? lista) (list))
       ((equal? (car num) (caar lista)) (cdr lista))
       (else (cons (car lista) (eliminar num (cdr lista))))))

;Funciones extra para la seleccion----------------------------
(define(asignar-caracts carac equipo)
  (cond((null? equipo) '())
       (else (cons (asignar-aux (car carac) (car equipo)) (asignar-caracts (cdr carac) (cdr equipo))))))

(define(asignar-aux hab jugador)
  (cond((or (null? hab) (null? jugador)) jugador)
       (else (list(car jugador) (cadr jugador) hab (car(cdddr jugador)) (cadr(cdddr jugador)) (obtenerEquipo jugador)))))
;;-------------------------------------------------------------------------------------------------------------------------------------------------



;;Funcion de reproduccion--------------------------------------------------------------------------------------------------------------------------
(define(reproduccion lista)
  (cond((null? lista) lista)
       ((null? (cdr lista)) lista)
       (else(append (combinar (car lista) (ultimo (car lista) lista)) (reproduccion (sin-ultimo(cdr lista)))))))

(define(combinar habilidades1 habilidades2)
  (cond((or (null? habilidades1) (null? habilidades2)) (list habilidades1 habilidades2))
       (else(cons (combinar-aux (+ 1 (random 5)) habilidades1 habilidades2) (list (combinar-aux (+ 1 (random 5)) habilidades1 habilidades2))))))

(define(combinar-aux num hab1 hab2)
  (cond((or (null? hab1) (= num 0)) hab2)
       (else(cons (car hab1) (combinar-aux (- num 1) (cdr hab1) (cdr hab2))))))

(define(ultimo actual lista)
  (cond((null? lista) actual)
       (else (ultimo (car lista) (cdr lista)))))

(define(sin-ultimo lista)
  (cond((null? (cdr lista)) '())
       (else(cons (car lista) (sin-ultimo (cdr lista))))))
;;-------------------------------------------------------------------------------------------------------------------------------------------------



;;Funcion de mutacion------------------------------------------------------------------------------------------------------------------------------
(define(mutacion habilidades)
  (cond((null? habilidades) habilidades)
       (else(cons (mutacion-aux (car habilidades)) (mutacion (cdr habilidades))))))

(define (mutacion-aux hab)
  (cond((< (random 100) 30) (mutar (+ 1 (random 4)) hab))
       (else hab)))

(define(mutar corte hab)
  (cond((zero? corte) (cons (random 11) (cdr hab)))
       (else (cons (car hab) (mutar (- corte 1) (cdr hab))))))

;;Funcion para regresar los jugadores a las alineaciones-------------------------------------------------------------------------------------------

(define(alinear equipos)
  (cond((null? equipos) equipos)
       (else (append (alinear2 (cadr equipos)) (alinear1 (car equipos))))))

(define(alinear1 equipo)
  (cond((null? equipo) equipo)
       (else (cons (alinear-jugador1 (car equipo)) (alinear1 (cdr equipo))))))

(define(alinear2 equipo)
  (cond((null? equipo) equipo)
       (else (cons (alinear-jugador2 (car equipo)) (alinear2 (cdr equipo))))))

(define(alinear-jugador1 jugador)
  (cond((null? jugador) jugador)
       ((equal? 1 (car(cdddr jugador)))
        (list (list 5 (+ 180 (random 180))) (list 0 0) (caddr jugador) 1 (cadr(cdddr jugador)) "red"))
       ((equal? 2 (car(cdddr jugador)))
        (list (list (+ 90 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr jugador) 2 (cadr(cdddr jugador)) "red"))
       ((equal? 3 (car(cdddr jugador)))
        (list (list (+ 260 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr jugador) 3 (cadr(cdddr jugador)) "red"))
       (else
        (list (list (+ 400 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr jugador) 4 (cadr(cdddr jugador)) "red"))))

(define(alinear-jugador2 jugador)
  (cond((null? jugador) jugador)
       ((equal? 1 (car(cdddr jugador)))
        (list (list 895 (+ 180 (random 180))) (list 0 0) (caddr jugador) 1 (cadr(cdddr jugador)) "purple"))
       ((equal? 2 (car(cdddr jugador)))
        (list (list (+ 750 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr jugador) 2 (cadr(cdddr jugador)) "purple"))
       ((equal? 3 (car(cdddr jugador)))
        (list (list (+ 600 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr jugador) 3 (cadr(cdddr jugador)) "purple"))
       (else
        (list (list (+ 465 (random 50)) (+ 5 (random 510))) (list 0 0) (caddr jugador) 4 (cadr(cdddr jugador)) "purple"))))

;;Inicializacion de la primera generacion para ambos equipos---------------------------------------------------------------------------------------

(define (jugador-1er tipo num)
  (cond((equal? 1 tipo)
        (list (list 5 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "red"))
       ((equal? 2 tipo)
        (list (list (+ 90 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "red"))
       ((equal? 3 tipo)
        (list (list (+ 260 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "red"))
       (else
        (list (list (+ 400 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "red"))
       ))

(define (jugador-2do tipo num)
  (cond ((equal? 1 tipo)
         (list (list 895 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "purple"))
        ((equal? 2 tipo)
         (list (list (+ 750 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "purple"))
        ((equal? 3 tipo)
         (list (list (+ 600 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "purple"))
        (else
         (list (list (+ 465 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "purple"))
        ))

(define (inic-aux1 num formacion)
  (cond((zero? num) '())
       ((equal? 1 num)
        (cons (jugador-1er 1 1) (inic-aux1 (- num 1) formacion)))
       ((and (> num 1) (< num (+ 2 (car formacion))))
        (cons (jugador-1er 2 num) (inic-aux1 (- num 1) formacion)))
       ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
        (cons (jugador-1er 3 num) (inic-aux1 (- num 1) formacion)))
       (else
        (cons (jugador-1er 4 num) (inic-aux1 (- num 1) formacion)))
       ))

(define (inic-aux2 num formacion)
  (cond ((zero? num) '())
        ((equal? 1 num)
         (cons (jugador-2do 1 1) (inic-aux2 (- num 1) formacion)))
        ((and (> num 1) (< num (+ 2 (car formacion))))
         (cons (jugador-2do 2 num) (inic-aux2 (- num 1) formacion)))
        ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
         (cons (jugador-2do 3 num) (inic-aux2 (- num 1) formacion)))
        (else
         (cons (jugador-2do 4 num) (inic-aux2 (- num 1) formacion)))
        ))

(define (inicializacion1 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux1 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))

(define (inicializacion2 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux2 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))
        ))

;;------------------------------------------------------------------------------------------;;Ec
;;------------------------------------------------------------------------------------------;;Ef
;;------------------------------------------------------------------------------------------;;Ei
;;------------------------------------------------------------------------------------------;;Ev

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