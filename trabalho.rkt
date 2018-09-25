#lang racket

(require csv-reading)
(require srfi/19)
(require rackunit)
(require rackunit/text-ui)
(require plot)

;;Função para poder ser realizada a leitura de arquivos .csv
(define (csvfile->list filename)
  (call-with-input-file filename
                        csv->list))

;;Arquivo.csv -> lista
;;Chamada da função de leitura
(define empresas (csvfile->list "dados.csv"))


;;Estrutura das acoes
(struct acoes (nome data close) #:transparent)

;;Lista -> Lista
;;Transforma o tipo do close de string para número
(define (construir lista)
  (acoes (first lista) (revert(second lista)) (string->number (sixth lista))))

;;String -> String
;;Recebe uma data e inverte ano com dia
(define (revert date)
  (string-join (reverse (string-split date "-")) "-"))

;;Lista, Nome de empresa -> Lista
;;Recebe o nome de uma empresa e filtra esse nome de 'Lista', o transformando em outra lista contendo apenas os elementos que contém esse nome
(define (separa_acoes_nome lista nome)
  [cond [(empty? lista) empty]
        [(equal? (first(first lista)) nome) (cons(construir (first lista)) (separa_acoes_nome (rest lista) nome))]
        [else (separa_acoes_nome (rest lista) nome)]])

(define Google (separa_acoes_nome empresas "Google"))

(define Microsoft (separa_acoes_nome empresas "Microsoft"))

(define Petrobras (separa_acoes_nome empresas "Petrobras"))

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

;;Lista->Lista
;;Recebe uma lista de empresa, e a ordena conforme a data
(define (sorting lista)
  (recursive_revert (sort (recursive_revert lista) #:key acoes-data string<?)))

;;Lista->Lista
;;Recebe uma lista e inverte o ano com o dia das datas dadas
(define (recursive_revert lista)
  [cond [(empty? lista) empty]
        [else (cons (acoes (acoes-nome(first lista)) (revert(acoes-data(first lista))) (acoes-close (first lista))) (recursive_revert (rest lista)))]])

;;Data, Lista -> Data
;;Recebe uma lista (nome de empresa) e uma data, a função retorna a proxima data válida a partir da data recebida
(define (proxDataValida data lista)
  [cond [(not (equal? 2018 (date-year (string->date data "~d-~m-~Y")))) (displayln "\nData invalida")]
        [else
         [cond [(empty? lista) "Nao existe proxima data valida para a data inserida"]
               [(> (date-month (string->date data "~d-~m-~Y")) (date-month(string->date (acoes-data (first lista)) "~d-~m-~Y"))) (proxDataValida data (rest lista))]
               [(= (date-month (string->date data "~d-~m-~Y")) (date-month(string->date (acoes-data (first lista)) "~d-~m-~Y")))
                [cond
                  [(> (date-day (string->date data "~d-~m-~Y")) (date-day (string->date (acoes-data (first lista)) "~d-~m-~Y"))) (proxDataValida data (rest lista))]
                  [(< (date-day (string->date data "~d-~m-~Y")) (date-day (string->date (acoes-data (first lista)) "~d-~m-~Y"))) (acoes-data (first lista))]
                  [else (proxDataValida data (rest lista))]]]]]])

;;Data, lista -> Data
;;Recebe uma data e uma lista e retorna a data válida antecendente a recebida
(define (prevDataValida data lista)
  [cond [(not (equal? 2018 (date-year (string->date data "~d-~m-~Y")))) (displayln "\nData invalida")]
        [else
         [cond [(empty? lista) "Nao existe data anterior valida para a data inserida"]
               [(> (date-month (string->date data "~d-~m-~Y")) (date-month(string->date (acoes-data (first (rest lista))) "~d-~m-~Y"))) (prevDataValida data (rest lista))]
               [(= (date-month (string->date data "~d-~m-~Y")) (date-month(string->date (acoes-data (first (rest lista))) "~d-~m-~Y")))
                [cond
                  [(> (date-day (string->date data "~d-~m-~Y")) (date-day (string->date (acoes-data (first (rest lista))) "~d-~m-~Y"))) (prevDataValida data (rest lista))]
                  [(<= (date-day (string->date data "~d-~m-~Y")) (date-day (string->date (acoes-data (first (rest lista))) "~d-~m-~Y"))) (acoes-data (first lista))]
                  [else (prevDataValida data (rest lista))]]]]]])

;;Lista, dias -> Lista
;;Recebe uma lista e um periodo de dias, faz o calculo do MMS para ser usado na 1 iteração do MME, e em seguida chama a funcao de calculo do MME
(define (exponencial lista dias)
  (define x (media-movel-simples lista dias))
  (cons (first x) (media-movel-exponencial (run_list lista dias) dias (first x))))

;;Lista, dias -> Lista
;;Recebe uma lista e um periodo de dias e avança esse periodo de dias na lista passada por parametro
(define (run_list lista dias)
  [cond [(= dias 0) lista]
        [else (run_list (rest lista) (sub1 dias))]])
  
;;Lista, dias, 1o valor do MMS ->Lista
;;Recebe uma lista, um periodo de dias e o primeiro valor do MMS para aquele periodo, e realiza o calculo do MME para a quantidade de dias passada por parametro no periodo
(define (media-movel-exponencial lista qnt_dias y)
   (define multiplier (/ 2 (+ qnt_dias 1)))
            [cond [(> qnt_dias (length lista)) empty]
                  [(empty? lista) empty]
                  [else
                      (define media-movel  (+ (* (- (acoes-close(first lista)) y) multiplier) y))
                      (cons media-movel (media-movel-exponencial (rest lista) qnt_dias media-movel))]])

;;Lista, período, período
;;Recebe uma lista e dois períodos, faz o calculo do MME para os mesmos e envia para a funcao de calculo do MACD
(define (moving-average lista periodo1 periodo2)
  (define per1 (exponencial lista periodo1))
  (define per2 (exponencial lista periodo2))
  (MACD lista per1 per2))

;;Lista, periodo, período
;;Recebe um nome de empresa e os intervalos de periodos para a realização da conta do MACD
(define (MACD lista per1 per2)
  [cond [(empty? per1) empty]
        [(empty? per2) empty]
        [else (cons (- (first per1) (first per2)) (MACD lista (rest per1) (rest per2)))]])


;;Lista, periodo -> Numero
;;Recebe uma lista e calcula a soma da subtração de dois dias de fechamento, dado que essa subtração seja positiva
(define (verAumento lista num)
  [cond [(zero? num) 0]
        [(empty? (rest lista)) 0]

        [else
         [cond [(> (* (- (acoes-close (first lista)) (acoes-close (first (rest lista)))) -1) 0)
                (+ (* (- (acoes-close (first lista)) (acoes-close (first (rest lista)))) -1) (verAumento (rest lista) (sub1 num)))]
         
               [else (verAumento (rest lista) (sub1 num))]]]])


;;Lista, periodo -> Numero
;;Recebe uma lista e calcula a soma da subtração de dois dias de fechamento, dado que essa subtração seja negativa
(define (verDiminui lista num)
  [cond [(zero? num) 0]
        [(empty? (rest lista)) 0]

        [else
         [cond [(< (* (- (acoes-close (first lista)) (acoes-close (first (rest lista)))) -1) 0)
                (+ (* (- (acoes-close (first lista)) (acoes-close (first (rest lista)))) -1) (verDiminui (rest lista) (sub1 num)))]
         
               [else (verDiminui (rest lista) (sub1 num))]]]])

;;Lista, periodo -> Numero
;;Recebe uma lista e calcula a quantidade de dias em que a subtração de dois dias de fechamento foram maior que 0
(define (qntAumento lista num)
  [cond [(zero? num) 0]
        [(empty? (rest lista)) 0]

        [else
         [cond [(> (* (- (acoes-close (first lista)) (acoes-close (first (rest lista)))) -1) 0)
                (add1 (qntAumento (rest lista) (sub1 num)))]
               [else (qntAumento (rest lista) (sub1 num))]]]])

;;Lista, periodo -> Numero
;;Recebe uma lista e calcula a quantidade de dias em que a subtração de dois dias de fechamento foram menor que 0
(define (qntDiminui lista num)
 [cond [(zero? num) 0]
        [(empty? (rest lista)) 0]

        [else
         [cond [(< (* (- (acoes-close (first lista)) (acoes-close (first (rest lista)))) -1) 0)
                (add1 (qntDiminui (rest lista) (sub1 num)))]
               [else (qntDiminui (rest lista) (sub1 num))]]]])




;;Lista, periodo
;;Recebe uma lista e um periodo, chama as funções de calculo de soma e quantidade e faz o calculo da força relativa usado na funcao de RSI
(define (ForcaRelativa lista periodo)
  (define valorAumento 0)
  (define valorDiminui 0)
  (define qntiAumento 0)
  (define qntiDiminui 0)

  
  (set! valorAumento (verAumento lista periodo))
  (set! valorDiminui (verDiminui lista periodo))
  (set! qntiAumento (qntAumento lista periodo))
  (set! qntiDiminui (qntDiminui lista periodo))
 
  [cond [(or (= qntiAumento 0) (= qntiDiminui 0)) 0]
        [else (/ (/ valorAumento qntiAumento) (/ (* valorDiminui -1) qntiDiminui))]])

;;Lista, periodo
;;Recebe uma lista e um periodo e efetua o calculo de RSI para um periodo de x dias
(define (RSI lista periodo)
  [cond [(empty? lista) empty]
        [else (cons (- 100 (/ 100 (+ 1 (ForcaRelativa lista periodo)))) (RSI (rest lista) periodo))]])
        
;;------------------------------------/
;;Execução de testes                 /
;;----------------------------------/

(define (executa-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))(void))

(define test-MME
  (test-suite "testes MME"
  (check-equal? (first(exponencial Google 14)) 1111.1821462857145)
  (check-equal? (first(exponencial Petrobras 10)) 11.171000000000001)
  (check-equal? (first(exponencial Microsoft 15)) 88.77999973333334)))

(define test-MMS
  (test-suite "testes MMS"
  (check-equal? (first (media-movel-simples Google 10)) 1100.145996)
  (check-equal? (first (media-movel-simples Microsoft 12)) 88.18249941666669)
  (check-equal? (first (media-movel-simples Petrobras 15)) 11.494666666666665)))

(define test-correl
  (test-suite "Testes Correlacao"
  (check-equal? (Correlacao Google Microsoft) 0.1603697511597682)
  (check-equal? (Correlacao Google Petrobras) -0.07983751056172986)
  (check-equal? (Correlacao Microsoft Petrobras) 0.6372067611546479)))

(define test-next-date
  (test-suite "Testes próxima data válida"
  (check-equal? (proxDataValida "20-04-2018" Google) "23-04-2018")
  (check-equal? (proxDataValida "10-03-2018" Google) "12-03-2018")
  (check-equal? (proxDataValida "03-02-2018" Google) "05-02-2018")))

(define test-prev-date
  (test-suite "Testes data anterior válida"
  (check-equal? (prevDataValida "30-05-2018" Google) "29-05-2018")
  (check-equal? (prevDataValida "05-01-2018" Google) "04-01-2018")
  (check-equal? (prevDataValida "26-02-2018" Google) "23-02-2018")))

(define test-MACD
  (test-suite "Testes MACD"
  (check-equal? (first(moving-average Google 12 26)) -18.221595237179372)
  (check-equal? (first(moving-average Petrobras 12 26)) -0.9373076923076944)
  (check-equal? (first(moving-average Microsoft 12 26)) -2.069808198717922)))

(define test-RSI
  (test-suite "Testes RSI"
  (check-equal? (first (RSI Google 10)) 86.43485803815199)
  (check-equal? (first (RSI Microsoft 16)) 72.09370788300682)
  (check-equal? (first (RSI Petrobras 12)) 65.72580645161293)))

  
(executa-testes test-MME)
(executa-testes test-MMS)
(executa-testes test-correl)
(executa-testes test-next-date)
(executa-testes test-prev-date)
(executa-testes test-MACD)
(executa-testes test-RSI)

;-------------------------------------/
;Fim execução testes                 /
;-----------------------------------/

(define date 0)
(define carteira 0)
(define qntGoogle 0)
(define qntMicro 0)
(define qntPetro 0)

;;Data, lista -> lista
;;Recebe uma data e uma lista, e percorre a lista até a data inserida
(define (run_to_date data lista)
 [cond [(empty? lista) "Nao existe proxima data valida para a data inserida"]
        [(> (date-month (string->date data "~d-~m-~Y")) (date-month(string->date (acoes-data (first lista)) "~d-~m-~Y"))) (run_to_date data (rest lista))]
        [(= (date-month (string->date data "~d-~m-~Y")) (date-month(string->date (acoes-data (first lista)) "~d-~m-~Y")))
        [cond
          [(> (date-day (string->date data "~d-~m-~Y")) (date-day (string->date (acoes-data (first lista)) "~d-~m-~Y"))) (run_to_date data (rest lista))]
          [(= (date-day (string->date data "~d-~m-~Y")) (date-day (string->date (acoes-data (first lista)) "~d-~m-~Y"))) lista]
          [else (run_to_date data (rest lista))]]]])

;;Data->Função
;;Recebe uma data para inicio da função de compra e venda
(define (data_inicio date)
  (displayln "Digite uma data para começar no formato ~d-~m~y (com aspas duplas): ")
  (set! date (read-line))
  [cond [(not (equal? 2018 (date-year (string->date date "~d-~m-~Y")))) (displayln "\nData invalida")]
        
  [else (compra_e_venda date (run_to_date date Google) (run_to_date date Microsoft) (run_to_date date Petrobras) qntGoogle qntMicro qntPetro carteira)]])

;;Data->Função
;;Recebe uma data para prosseguir a execução da funcao de compra e venda
(define (dataSubsequente date qntiGoogle qntiMicro qntiPetro carteira)
  (compra_e_venda date (run_to_date date Google) (run_to_date date Microsoft) (run_to_date date Petrobras) qntiGoogle qntiMicro qntiPetro carteira))



;;Data, lista, lista, lista -> void
;;Recebe uma data e 3 sublistas, cada uma correspondente aos dados de uma empresa específica, a função simula a compra e venda de ações dia por dia
(define (compra_e_venda dia lisGoogle lisMicro lisPetro qntGoogle qntMicro qntPetro carteira)

(define valor 0)
(define opcao 0)  
(define choose 0)
(define nome_empresa "g")

  (displayln "\t\tDia")
  (printf "----------  ~a ----------\n\n" dia)
  (printf "Valor Carteira: ~a\n" carteira)
  (displayln "Valor ações: ")
                 (displayln (string-append "\t\tGoogle: " (number->string(acoes-close (first lisGoogle)))))
                 (displayln (string-append "\t\tMicrosoft: " (number->string(acoes-close (first lisMicro)))))
                 (displayln (string-append "\t\tPetrobras: " (number->string(acoes-close (first lisPetro))) "\n"))
  (displayln "0) Finalizar simulação.")
  (displayln "1) Comprar acao")
  (displayln "2) Vender acao")
  (displayln "3) Nenhuma acao")
  (set! opcao (string->number (read-line)))

  [cond
    [(= opcao 0) (display "\nFinalizado.\n")

                 [cond
                   [(>= carteira 0) (display "\nLucro: ")]
                   [(< carteira 0) (display "\nPerda: ")]]
                 
                  (displayln (string-append (number->string carteira) "\n"))

                  (displayln "Ações restantes: ")
                  (displayln (string-append "\t\tGoogle\t\tMicrosoft\t\tPetrobras"))
                  (displayln (string-append "              \t" (number->string qntGoogle) "\t\t" (number->string qntMicro) "\t\t\t" (number->string qntPetro)))]
    
    [(= opcao 1) (define (menu-compras choose)
                 (displayln "-----Compra de ações-----\n")
                 (displayln "0) Finalizar compras")
                 (displayln "1) Comprar ação")        
                 (set! choose (string->number (read-line)))

                 [cond
                   [(= choose 0) (displayln "Retornando ao menu..") (compra_e_venda dia lisGoogle lisMicro lisPetro qntGoogle qntMicro qntPetro carteira)]
                   [(= choose 1) (displayln "Digite o nome da empresa: ")
                                 (set! nome_empresa (read-line))
                                 [cond [(not (or (equal? nome_empresa "Google") (equal? nome_empresa "Microsoft") (equal? nome_empresa "Petrobras"))) (displayln "\n**Nome empresa invalido**\n")]
                                       [else
                                        [cond [(string-locale=? "Google" nome_empresa) (displayln "Digite a quantidade de ações: ")
                                                                                       (set! valor (string->number(read-line)))
                                                                                       (set! qntGoogle (+ qntGoogle valor))
                                                                                       (set! carteira (- carteira (* valor (acoes-close (first lisGoogle)))))]
                                              
                                              [(string-locale=? "Microsoft" nome_empresa) (displayln "Digite a quantidade de ações: ")
                                                                                          (set! valor (string->number(read-line)))
                                                                                          (set! qntMicro (+ qntMicro valor))
                                                                                          (set! carteira (- carteira (* valor (acoes-close (first lisMicro)))))]
                                              
                                              [(string-locale=? "Petrobras" nome_empresa) (displayln "Digite a quantidade de ações: ")
                                                                                          (set! valor (string->number(read-line)))
                                                                                          (set! qntPetro (+ qntPetro valor))
                                                                                          (set! carteira (- carteira (* valor (acoes-close (first lisPetro)))))]]]]

                                 
                                 
                                 (displayln (string-append "\t\tGoogle\t\tMicrosoft\t\tPetrobras"))
                                 (displayln (string-append "Minhas ações: \t" (number->string qntGoogle) "\t\t" (number->string qntMicro) "\t\t\t" (number->string qntPetro)))
                                                                                                    (menu-compras choose)]])(menu-compras choose)]
    

    [(= opcao 2) (define (menu_venda choose)

                   (displayln (string-append "\t\tGoogle\t\tMicrosoft\t\tPetrobras"))
                   (displayln (string-append "Minhas ações: \t" (number->string qntGoogle) "\t\t" (number->string qntMicro) "\t\t\t" (number->string qntPetro)))
                 
                   (displayln "0. Sair")
                   (displayln "1. Vender todas as ações")
                   (displayln "2. Vender ações Google")
                   (displayln "3. Vender ações Microsoft")
                   (displayln "4. Vender ações Petrobras")
                   (set! choose (string->number (read-line)))

                   [cond
                     [(= choose 0) (displayln "Retornando ao menu principal") (compra_e_venda dia lisGoogle lisMicro lisPetro qntGoogle qntMicro qntPetro carteira)]

                     [(= choose 1) (set! carteira (+ carteira (* qntGoogle (acoes-close (first lisGoogle)))
                                                              (* qntMicro (acoes-close (first lisMicro)))
                                                              (* qntPetro (acoes-close (first lisPetro)))))
                                   (set! qntGoogle 0)
                                   (set! qntMicro 0)
                                   (set! qntPetro 0)
                                   
                                   (menu_venda choose)]

                     [(= choose 2) (printf "Você possui ~a ações da Google\n" qntGoogle)
                                   (displayln "Digite a quantidade de ações que deseja vender: ")
                                   (set! valor (string->number (read-line)))

                                   (set! qntGoogle (- qntGoogle valor))
                                   (set! carteira (+ carteira (* valor (acoes-close (first lisGoogle)))))
                                   
                                   (printf "~a ações da Google vendidas\n" valor)
                                   (menu_venda choose)]
                     
                     [(= choose 3) (printf "Você possui ~a ações da Microsoft\n" qntMicro)
                                   (displayln "Digite a quantidade de ações que deseja vender: ")
                                   (set! valor (string->number (read-line)))

                                   (set! qntMicro (- qntMicro valor))
                                   (set! carteira (+ carteira (* valor (acoes-close (first lisMicro)))))
                                   
                                   (printf "~a ações da Microsoft vendidas\n" valor)
                                   (menu_venda choose)]
                     
                     [(= choose 4) (printf "Você possui ~a ações da Petrobras\n" qntPetro)
                                   (displayln "Digite a quantidade de ações que deseja vender: ")
                                   (set! valor (string->number (read-line)))

                                   (set! qntPetro (- qntPetro valor))
                                   (set! carteira (+ carteira (* valor (acoes-close (first lisPetro)))))
                                   
                                   (printf "~a ações da Petrobras vendidas\n" valor)
                                   (menu_venda choose)]]) (menu_venda choose)]

    
    [(= opcao 3) (dataSubsequente (proxDataValida dia Google) qntGoogle qntMicro qntPetro carteira)]])



;----------FUNÇÕES CONSTRUÇÃO GRAFICOS-----------


;;Lista -> lista
;;Recebe uma lista e retorna uma lista contendo apenas os valores de close de uma determinada empresa
(define (close lista)
  (cond [(empty? lista) empty]
        [else (cons (acoes-close (first lista)) (close (rest lista)))]))

;;Numero->lista
;;Recebe um numero e constroi uma lista decrescente daquele numero até 0
(define (constroi valor)
  (cond [(= valor 0) empty]
        [else (cons valor (constroi (sub1 valor)))]))

;;Lista, lista, string, string -> grafico
;;Recebe uma lista de numeros de periodo e uma lista contendo o que desejamos plotar, em seguida gera o grafico com base nos parametros dados
(define (plota-grafico plot1 plot2 nome_emp nome)
  (display (plot (lines (map vector plot2 plot1))
        #:title (string-append "Valor " nome ": " nome_emp) #:width 1300 #:y-label (string-append "Valor " nome) #:x-label "Periodo")))

;---------------------FIM------------------------
                    
(define opcao 0)

;;Num -> void
;;Menu principal, aqui ocorrerá toda a execução das funções implementadas
(define (main-menu opcao)
  (define nome_emp "teste")
  (define nome_emp2 "teste")
  (define periodo 10)
  (define periodo2 10)
  (define date "teste")

  (sorting Google)
  (sorting Microsoft)
  (sorting Petrobras)
  
  (displayln "\n---------- Menu principal ----------\n")
  (displayln "0. Finalizar")
  (displayln "1. Valor ações empresa")
  (displayln "2. Calcular MMS")
  (displayln "3. Calcular MME")
  (displayln "4. Calcular RSI")
  (displayln "5. Calcular MACD")
  (displayln "6. Calcular Correlacao")
  (displayln "7. Verificar proxima data valida")
  (displayln "8. Verificar data anterior valida")
  (displayln "9. Simular compra e venda de ações")

  (set! opcao (string->number (read-line)))

  [cond [(= opcao 0) (displayln "Saindo...")]
        
        [(= opcao 1) (displayln "\n ---- Valor Ações ----\n")
                     (displayln "Digite o nome da empresa: ")
                     (set! nome_emp (read-line))
                     [cond [(not (or (equal? nome_emp "Google") (equal? nome_emp "Microsoft") (equal? nome_emp "Petrobras"))) (displayln "\nNome empresa invalido") (main-menu opcao)]
                     [else 
                           (define plot1 (close (separa_acoes_nome empresas nome_emp)))
                           (define plot2 (reverse (constroi (length plot1))))
                           (plota-grafico plot1 plot2 nome_emp "Ações") (main-menu opcao)]]]
                               

        [(= opcao 2) (displayln "\n ---- Calculo de MMS ----\n")
                     (displayln "Digite o nome da empresa: ")
                     (set! nome_emp (read-line))
                     [cond [(not (or (equal? nome_emp "Google") (equal? nome_emp "Microsoft") (equal? nome_emp "Petrobras"))) (displayln "\nNome empresa invalido") (main-menu opcao)]
                     [else (displayln "Digite a quantidade de períodos: ")
                           (set! periodo (read-line))
                           (define plot1 (media-movel-simples (separa_acoes_nome empresas  nome_emp) (string->number periodo)))
                           (define plot2 (reverse (constroi (length plot1))))
                           (plota-grafico plot1 plot2 nome_emp "MMS") (main-menu opcao)]]]
                          

        [(= opcao 3) (displayln "\n ---- Calculo de MME ----\n")
                     (displayln "Digite o nome da empresa: ")
                     (set! nome_emp (read-line))
                     [cond [(not (or (equal? nome_emp "Google") (equal? nome_emp "Microsoft") (equal? nome_emp "Petrobras"))) (displayln "\nNome empresa invalido") (main-menu opcao)]
                     [else (displayln "Digite a quantidade de períodos: ")
                           (set! periodo (read-line))
                           (define plot1 (exponencial (separa_acoes_nome empresas nome_emp) (string->number periodo)))
                           (define plot2 (reverse (constroi (length plot1))))
                           (plota-grafico plot1 plot2 nome_emp "MME") (main-menu opcao)]]]

       [(= opcao 4) (displayln "\n ---- Calculo de RSI ----\n")
                     (displayln "Digite o nome da empresa: ")
                     (set! nome_emp (read-line))
                     [cond [(not (or (equal? nome_emp "Google") (equal? nome_emp "Microsoft") (equal? nome_emp "Petrobras"))) (displayln "\nNome empresa invalido") (main-menu opcao)]
                     [else (displayln "Digite a quantidade de períodos: ")
                           (set! periodo (read-line))
                           (define plot1 (RSI (separa_acoes_nome empresas nome_emp) (string->number periodo)))
                           (define plot2 (reverse (constroi (length plot1))))
                           (plota-grafico plot1 plot2 nome_emp "RSI") (main-menu opcao)]]]


        [(= opcao 5) (displayln "\n ---- Calculo de MACD ----\n")
                     (displayln "Digite o nome da empresa: ")
                     (set! nome_emp (read-line))
                     [cond [(not (or (equal? nome_emp "Google") (equal? nome_emp "Microsoft") (equal? nome_emp "Petrobras"))) (displayln "\nNome empresa invalido") (main-menu opcao)]
                     [else (displayln "Digite a primeira quantidade de períodos: ")
                           (set! periodo (read-line))
                           (displayln "Digite a segunda quantidade de períodos: ")
                           (set! periodo2 (read-line))

                           (define plot1 (moving-average (separa_acoes_nome empresas nome_emp) (string->number periodo) (string->number periodo2)))
                           (define plot2 (reverse (constroi (length plot1))))
                           (plota-grafico plot1 plot2 nome_emp "MACD") (main-menu opcao)]]]

        [(= opcao 6) (displayln "\n ---- Calculo de correlação ----\n")
                     (displayln "Digite o nome da primeira empresa: ")
                     (set! nome_emp (read-line))
                     [cond [(not (or (equal? nome_emp "Google") (equal? nome_emp "Microsoft") (equal? nome_emp "Petrobras"))) (displayln "\nNome empresa invalido") (main-menu opcao)]
                     [else (displayln "Digite o nome da segunda empresa: ")
                     (set! nome_emp2 (read-line))
                     (displayln (string-append "\n---- O nível de correlação entre " nome_emp " e " nome_emp2 " é: ---- \n" "\t          "
                                               (number->string(Correlacao (separa_acoes_nome empresas nome_emp) (separa_acoes_nome empresas nome_emp2))))) (main-menu opcao)]]]

        [(= opcao 7) (displayln "\n ---- Verificação Próxima Data Válida ----\n")
                     (displayln "Digite o nome da empresa: ")
                     (set! nome_emp (read-line))
                     [cond [(not (or (equal? nome_emp "Google") (equal? nome_emp "Microsoft") (equal? nome_emp "Petrobras"))) (displayln "\nNome empresa invalido") (main-menu opcao)]
                     [else (displayln (string-append "Digite uma data no formato " "~d-~m-~y (com aspas duplas): " ))
                           (set! date (read-line))
                           (displayln (string-append "\nA próxima data válida a partir de " date " é "
                                                     (proxDataValida date (separa_acoes_nome empresas nome_emp)) "\n"))(main-menu opcao)]]]
                           

        [(= opcao 8) (displayln "\n ---- Verificação Data Anterior Válida ----\n")
                     (displayln "Digite o nome da empresa: ")
                     (set! nome_emp (read-line))
                     [cond [(not (or (equal? nome_emp "Google") (equal? nome_emp "Microsoft") (equal? nome_emp "Petrobras"))) (displayln "\nNome empresa invalido") (main-menu opcao)]
                     [else (displayln (string-append "Digite uma data no formato " "~d-~m-~y (com aspas duplas): " ))
                           (set! date (read-line))
                           (displayln (string-append "\nA data anterior válida a partir de " date " é "
                                                     (prevDataValida date (separa_acoes_nome empresas nome_emp)) "\n"))(main-menu opcao)]]]

        [(= opcao 9) (displayln "\n ---- Compra e Venda de ações ----\n")
                     (data_inicio date)
                     (main-menu opcao)]])


(main-menu opcao)
