#lang racket

(require csv-reading)

(define (csvfile->list filename)
  (call-with-input-file filename
                        csv->list))

(define empresas (csvfile->list "dados.csv"))
       
(define (split_strings lista)
   [cond [(empty? lista) empty]
   [else (cons(string-split (first(first lista))";" #:repeat? #t) (split_strings (rest lista)))]])

(struct acoes (nome data open high low close adj volume) #:transparent)

(define formated (split_strings empresas))

(define (construir lista)
  (acoes (first lista) (second lista) (string->number (third lista)) (string->number (fourth lista)) (string->number (fifth lista)) (string->number (sixth lista))
         (string->number (seventh lista)) (string->number (eighth lista))))

(define (separa_acoes_nome lista nome)
  [cond [(empty? lista) empty]
        [(equal? (first(first lista)) nome) (cons(construir (first lista)) (separa_acoes_nome (rest lista) nome))]
        [else (separa_acoes_nome (rest lista) nome)]])

(define Google (separa_acoes_nome formated "Google"))

(define Microsoft (separa_acoes_nome formated "Microsoft"))

(define Petrobras (separa_acoes_nome formated "Petrobras"))

(define (columnCloseToCorr  lista)
  [cond [(empty? lista) 0]
        [else (+  (acoes-close (first lista)) (columnCloseToCorr (rest lista)))]])
        
(define (MultColumnXYCorr lista1 lista2)
  [cond [(empty? lista2) 0]
        [else (+ (* (acoes-close (first lista1)) (acoes-close (first lista2))) (MultColumnXYCorr (rest lista1) (rest lista2)))]])

(define (calcNCorr lista)
  [cond [(empty? lista) 0]
        [else (add1 (calcNCorr (rest lista)))]])

(define (x nome) (columnCloseToCorr nome))
(define (y nome) (columnCloseToCorr nome))
(define (xy nome1 nome2) (MultColumnXYCorr nome1 nome2))
(define (valorN nome) (calcNCorr nome))

(define (Correlacao nome1 nome2)
  (/ (- (xy nome1 nome2) (/ (* (x nome1) (y nome2)) (valorN nome1))) (sqrt (* (- (xy nome1 nome1) (/ (* (x nome1) (x nome1)) (valorN nome1))) (- (xy nome2 nome2) (/ (* (y nome2) (y nome2)) (valorN nome1))))))) 
   
  

