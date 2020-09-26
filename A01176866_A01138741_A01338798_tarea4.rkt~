#lang racket
; Tarea 4
; INTEGRANTES
; Isabel Cruz, A01138741
; Hector Diaz, A01176866
; Ruben Salazar, A01338798


;EJERCICIO 1a


;EJERCICIO 1b
(define (agrega-fila matriz numFilas)
  (if (>= (length matriz) numFilas)
      matriz
      (agrega-fila (append matriz (list (list 0))) numFilas)))


(define (agrega-columna lista numColumnas)
  (if (>= (length lista) numColumnas)
      lista
      (agrega-columna (append lista (list 0)) numColumnas)))

(define (cambiar-valor lista indice indiceActual valor)
  (cond
    ((= indice indiceActual) (append (list valor) (rest lista)))
    (else
     (append (list (first lista)) (cambiar-valor (rest lista) indice (+ indiceActual 1) valor)))))


(define (agrega-valor-helper valor posicion matriz renglonActual)
  (cond
    ((= renglonActual (first posicion)) (cons (cambiar-valor (first matriz) (second posicion) 1 valor) (rest matriz)))
    (else
     (cons (first matriz) (agrega-valor-helper valor posicion (rest matriz) (+ renglonActual 1))))))

(define (agrega-columnas matriz numColumnas)
  (cond
    ((empty? matriz) matriz)
    (else
     (cons (agrega-columna (first matriz) numColumnas) (agrega-columnas (rest matriz) numColumnas)))))

(define (agrega-valor valor posicion matriz)
  (agrega-valor-helper valor posicion (agrega-columnas (agrega-fila matriz (first posicion)) (second posicion)) 1))


;EJERCICIO 2a



;EJERCICIO 2b

(define ABB
  '(8 (5 (2 () ()) (7 () ()))
      (9 () (15 (11 () ()) ()) )
      )
  )

(define ABB2
  '(a (b (c () ()) ()) (d (e () ()) ()))
  )

(define (cuenta-nivel nivel arbol)
  (cuenta-aux 0 nivel arbol)
  )

(define (cuenta-aux index nivel arbol)
  (cond
    [(null? arbol) 0]
    [(= index nivel)
     (if (null? (car arbol)) 0 1)]
    [else (+ (cuenta-aux (+ index 1) nivel (cadr arbol))
             (cuenta-aux (+ index 1) nivel (caddr arbol))
             )
          ]
    )
  )


;EJERCICIO 3a
(define (nodos-destino grafo nodo)
  (flatten (map (lambda (adyacencia) (if (equal? (first adyacencia) nodo) (apply append (map (lambda (edge) (list (first edge))) (rest adyacencia))) '())) grafo)))
  



;EJERCICIO 3b
(define g
  '((A (B 2) (D 10))
    (B (C 9) (E 5))
    (C (A 12) (D 6))
    (D (E 7))
    (E (C 3))
    ))

(define (elimina-nodo grafo nodo)
  (apply append
         (map
          (lambda (seccion)
            (if (eq? (car seccion) nodo)
                '()
                (list 
                 (append (list(car seccion))
                         (apply append
                                (map
                                 (lambda (adyacente)
                                   (if (eq? (car adyacente) nodo)
                                       '()
                                       (list adyacente) )
                                   ) (cdr seccion)))) )
                ))
          grafo)))





