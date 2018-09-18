#lang racket/gui
(require racket/gui/base)

;This is our main window
(define main-window (new frame%
                       [label "Simulador de ações"]
                       [width 400]
                       [height 300]
                       [style '(fullscreen-button)]
                       [alignment '(left top)]))


;This establishes a horizontal pane. It will take the text output for the permutations and a button.
;It is declared after name-input because it is ordered below it in the main-window. 
(define panel (new horizontal-pane%
                   [parent main-window]
                   [vert-margin 10]
                   [horiz-margin 10]
                   [alignment '(left top)]
                   [stretchable-width #t]
                   [stretchable-height #t]))

(define panel2 (new horizontal-pane%
                   [parent main-window]
                   [vert-margin 10]
                   [horiz-margin 10]
                   [alignment '(left bottom)]
                   [stretchable-width #f]
                   [stretchable-height #f]))



(define combo-field (new combo-field%
                         (label "Funções")
                         (parent panel)                      
                         (choices (list "Preço ações" "Calcular MMS" "Calcular MME" "Calcular Correlação" "Calcular RSI" "Calcular MACD" "Calcular Próxima Data" "Calcular Data antecedente" "Simular compra e venda"))
                         (init-value "Selecione")
                         [min-width 150]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [stretchable-width #t]
                         [stretchable-height #f]
                         [callback (lambda(cfield event) [cond [(equal? (equal? (send combo-field get-value) "Preço ações")  #t) (send name-input enable #f)]
                                                               [else (send name-input enable #t)]])]))

(define get-opcao (send combo-field get-value))
                         

;This is the textfield where you will enter your name. We declare the textfield before the following
;gui-elements because it is the first element in the window to be placed on the top
(define name-input (new text-field%
                        [parent main-window]
                        [label "Períodos: "]
                        [min-width 50]
                        [min-height 30]
                        [vert-margin 20]
                        [horiz-margin 20]
                        [enabled #t]
                        [stretchable-width #f]
                        [stretchable-height #f]))

(define get-periodo (send name-input get-value))

(define field-empresas (new combo-field%
                         (label "Empresas")
                         (parent panel2)                      
                         (choices (list "Google" "Microsoft" "Petrobras"))
                         (init-value "Selecione")
                         [min-width 150]
                         [vert-margin 10]
                         [horiz-margin 10]
                         [stretchable-width #t]
                         [stretchable-height #f]))

(define get-nome (send field-empresas get-value))

;Here comes the button for starting the permutation calculations
(define start-calculation (new button%
                               [parent panel]
                               [label "Start"]
                               [vert-margin 10]
                               [horiz-margin 10]
                               [stretchable-width #f]
                               [stretchable-height #f]
                               [callback (lambda (button event) (send combo-field get-value)
                                           [cond [(equal? (send combo-field get-value) "Preço ações") 
                                                  (define plot1 (close (separa_acoes_nome empresas (send field-empresas get-value))))
                                                  (define plot2 (reverse (constroi (length plot1))))

                                                  (plot (lines (map vector plot2 plot1)) #:title (string-append "Valor ações: " (send field-empresas get-value)) #:width 1300 #:y-label "Valor close" #:x-label "Dias")]])]))

                                                  ;[(string-locale=? get-combo "Calcular MMS") get-input ;;gerar grafic

  
;Make window visible
(send main-window show #t)