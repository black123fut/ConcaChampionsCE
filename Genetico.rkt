;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Genetico2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
       (else (list(car jugador) (cadr jugador) hab (car(cdddr jugador)) (cadr(cdddr jugador)) (caddr(cdddr jugador))))))
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
        (list (list (+ 400 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "red"))))

(define (jugador-2do tipo num)
  (cond ((equal? 1 tipo)
         (list (list 895 (+ 180 (random 180))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 1 num "purple"))
        ((equal? 2 tipo)
         (list (list (+ 750 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 2 num "purple"))
        ((equal? 3 tipo)
         (list (list (+ 600 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 3 num "purple"))
        (else
         (list (list (+ 465 (random 50)) (+ 5 (random 510))) (list 0 0) (list 0 (random 11) (random 11) (random 11) (random 11)) 4 num "purple"))))

(define (inic-aux1 num formacion)
  (cond((zero? num) '())
       ((equal? 1 num)
        (cons (jugador-1er 1 1) (inic-aux1 (- num 1) formacion)))
       ((and (> num 1) (< num (+ 2 (car formacion))))
        (cons (jugador-1er 2 num) (inic-aux1 (- num 1) formacion)))
       ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
        (cons (jugador-1er 3 num) (inic-aux1 (- num 1) formacion)))
       (else
        (cons (jugador-1er 4 num) (inic-aux1 (- num 1) formacion)))))

(define (inic-aux2 num formacion)
  (cond ((zero? num) '())
        ((equal? 1 num)
         (cons (jugador-2do 1 1) (inic-aux2 (- num 1) formacion)))
        ((and (> num 1) (< num (+ 2 (car formacion))))
         (cons (jugador-2do 2 num) (inic-aux2 (- num 1) formacion)))
        ((and (> num (+ 1 (car formacion))) (< num (+ 2 (car formacion) (cadr formacion))))
         (cons (jugador-2do 3 num) (inic-aux2 (- num 1) formacion)))
        (else
         (cons (jugador-2do 4 num) (inic-aux2 (- num 1) formacion)))))

(define (inicializacion1 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux1 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))))

(define (inicializacion2 formacion)
  (cond ((not(list? formacion)) '())
        (else
         (inic-aux2 (+ 1 (car formacion) (cadr formacion) (caddr formacion)) formacion))))



;Obtener formacion del equipo-----------------------------------------------------------------------------------------------
(define (obtenerNumTipo jugador)
  (cond((null? jugador) '())
       (else
        (obtenerNumTipo_aux jugador 0)
  )))

(define (obtenerNumTipo_aux jugador num)
  (cond((equal? num 3) (car jugador))
       (else
        (obtenerNumTipo_aux (cdr jugador) (+ num 1)))))
(define (obtenerFormacion jugadores  num1 num2 num3)
  (cond((null? jugadores) (list num1 num2 num3))
       ((equal? (obtenerNumTipo (car jugadores)) 2) (obtenerFormacion (cdr jugadores) (+ num1 1) num2 num3))
       ((equal? (obtenerNumTipo (car jugadores)) 3) (obtenerFormacion (cdr jugadores) num1 (+ num2 1) num3))
       ((equal? (obtenerNumTipo (car jugadores)) 4) (obtenerFormacion(cdr jugadores)  num1 num2 (+ num3 1)))
       (else
        (obtenerFormacion (cdr jugadores) num1 num2 num3))))
;;--------------------------------------------------------------------------------------------------------------------------
