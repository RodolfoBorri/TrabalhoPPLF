#lang racket

(require csv-reading)
(require srfi/19)

;;Função para poder ser realizada a leitura de arquivos .csv
(define (csvfile->list filename)
  (call-with-input-file filename
                        csv->list))

;;Arquivo.csv -> lista
;;Chamada da função de leitura
(define empresas (csvfile->list "dados.csv"))

;;Lista -> lista
;;Recebe uma lista contendo uma única string e a divide em várias strings menores
(define (split_strings lista)
   [cond [(empty? lista) empty]
   [else (cons(string-split (first(first lista))";" #:repeat? #t) (split_strings (rest lista)))]])

;;Estrutura das acoes
(struct acoes (nome data open high low close adj volume) #:transparent)

;;Chamada da função que divide uma string em várias
(define formated (split_strings empresas))

;;Lista -> Lista
;;Transforma o tipo do close de string para número
(define (construir lista)
  (acoes (first lista) (second lista) (string->number (third lista)) (string->number (fourth lista)) (string->number (fifth lista)) (string->number (sixth lista))
         (string->number (seventh lista)) (string->number (eighth lista))))

;;Lista, Nome de empresa -> Lista
;;Recebe o nome de uma empresa e filtra esse nome de 'Lista', o transformando em outra lista contendo apenas os elementos que tem esse nome
(define (separa_acoes_nome lista nome)
  [cond [(empty? lista) empty]
        [(equal? (first(first lista)) nome) (cons(construir (first lista)) (separa_acoes_nome (rest lista) nome))]
        [else (separa_acoes_nome (rest lista) nome)]])

(define Google (separa_acoes_nome formated "Google"))

(define Microsoft (separa_acoes_nome formated "Microsoft"))

(define Petrobras (separa_acoes_nome formated "Petrobras"))

;;Lista -> Numero
;;Recebe uma lista de fechamentos(closes) e realiza o somatorio do primeiro até o ultimo elemento dessa lista
(define (somatorioColumnClose  lista)
  [cond [(empty? lista) 0]
        [else (+  (acoes-close (first lista)) (somatorioColumnClose (rest lista)))]])

;;Lista, Lista -> Numero
;;Recebe duas listas de fechamentos(closes) e realiza a multiplicação e em seguida o somatorio entre o primeiro até o ultimo elemento dessas listas
(define (MultColumnXYCorr lista1 lista2)
  [cond [(empty? lista2) 0]
        [else (+ (* (acoes-close (first lista1)) (acoes-close (first lista2))) (MultColumnXYCorr (rest lista1) (rest lista2)))]])

;;Lista -> Numero
;;Recebe uma lista e faz o calculo de quantos elementos aquela lista possui
(define (calcNCorr lista)
  [cond [(empty? lista) 0]
        [else (add1 (calcNCorr (rest lista)))]])


;; { Chamadas das funções somatorio, mult e calcN
(define (x nome) (somatorioColumnClose nome))
(define (y nome) (somatorioColumnClose nome))
(define (xy nome1 nome2) (MultColumnXYCorr nome1 nome2))
(define (valorN nome) (calcNCorr nome))
;; }

;;Lista, lista -> Numero
;;Recebe duas listas com respectivos closes e retorna o valor de correlação entre os mesmos
(define (Correlacao nome1 nome2)
  (/ (- (xy nome1 nome2) (/ (* (x nome1) (y nome2)) (valorN nome1))) (sqrt (* (- (xy nome1 nome1) (/ (* (x nome1) (x nome1)) (valorN nome1))) (- (xy nome2 nome2) (/ (* (y nome2) (y nome2)) (valorN nome1))))))) 

;;Nome de empresa, dias -> Numero
;;Faz o calculo da media movel em um periodo n de dias 
(define (media-movel-simples nome qnt_dias)
  [cond [(> qnt_dias (length nome)) empty]
        [(empty? nome) empty]
        [else (cons(/ (auxMMS nome qnt_dias) qnt_dias) (media-movel-simples (rest nome) qnt_dias))]])

;;Lista, Numero -> Numero
;;Realiza a soma entre os valores de close dentro do intervalo num
(define (auxMMS lista num)
  [cond [(zero? num) 0]
        [else (+ (acoes-close (first lista)) (auxMMS (rest lista) (sub1 num)))]])

;;Data, Lista -> Data
;;Recebe uma lista (nome de empresa) e uma data, a função retorna a proxima data válida a partir da data recebida
(define (proxDataValida data lista)
  [cond [(empty? lista) "Nao existe proxima data valida para a data inserida"]
        [(> (date-month (string->date data "~d/~m/~Y")) (date-month(string->date (acoes-data (first lista)) "~d/~m/~Y"))) (proxDataValida data (rest lista))]
        [(= (date-month (string->date data "~d/~m/~Y")) (date-month(string->date (acoes-data (first lista)) "~d/~m/~Y")))
        [cond
          [(> (date-day (string->date data "~d/~m/~Y")) (date-day (string->date (acoes-data (first lista)) "~d/~m/~Y"))) (proxDataValida data (rest lista))]
          [(< (date-day (string->date data "~d/~m/~Y")) (date-day (string->date (acoes-data (first lista)) "~d/~m/~Y"))) (acoes-data (first lista))]
          [else (proxDataValida data (rest lista))]]]])

         

;(string->date "11/05/2018" "~d/~m/~Y") 08/10/2018