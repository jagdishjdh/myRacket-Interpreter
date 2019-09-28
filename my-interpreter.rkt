#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

(define stacks (make-vector 100))
(define stacksindex 0)

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                        (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                        (car stack)))


;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent) ;hastable gives the initial bindings
  (set! framenumber (+ 1 framenumber))
  (frame (- framenumber 1) hashtable parent))

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)))

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
         (match prog
           [(pgm deflist) (map (λ (x) (processdef x (top))) deflist)
                          (let ([main (return-value-of-main (top))])
                            ;(if (equal? main (void)) '(void)
                                main)]))

;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (hash-set! (frame-bindings fr) v/f (eval-exp exp))]))

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (hash-ref! (frame-bindings frame) 'main "main not found"))

;; function to map parameters/variables of function to their arguments/values
;;in function call
;; currently assuming that varlist and arglist have same no. of elements
(define (map-var-arg varlist arglist fr)
  (match (cons varlist arglist)
    ['(()) '()]
    [(cons (cons v vr) (cons a ar)) (hash-set! (frame-bindings fr) v (eval-exp a))
                                    (map-var-arg vr ar fr)]))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) (match (search exp (top))
                         [(emptyframe) (error "symbol not found")]
                         [(frame n b p) (hash-ref b exp)])]
        [(boolean? exp) exp]
        [(number? exp)  exp]
        [(list? exp)    exp]
        [(string? exp)  exp]
        [else (match exp
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(lam var _) (closure exp (top))]
                [(app exp1 explist) (let*([clsr (eval-exp exp1)]
                                          [new-fr (createframe (make-hash '())
                                                               (closure-frame clsr))])
                                      (match (closure-lambda clsr)
                                        [(lam varlist exp2) (map-var-arg varlist explist new-fr)
                                                            (push new-fr)
                                                            (let ([ans (eval-exp exp2)])
                                                              (pop) ans)]))]
                [(iff con exp1 exp2) (if (eval-exp con) (eval-exp exp1) (eval-exp exp2))]
                [(sett var exp2) (match (search var (top))
                                   [(emptyframe) (error "symbol not found")]
                                   [(frame n b p) (hash-set! b var (eval-exp exp2))])]
                [(lett deflist exp2) (let ([new-fr (createframe (make-hash '()) (top))])
                                       (map (λ (defn) (processdef defn new-fr)) deflist)
                                       (push new-fr)
                                       (let ([ans (eval-exp exp2)])
                                         (pop)  ans))]
                [(lets deflist exp2) (process-lets deflist exp2)]
                [(beginexp explist)  (process-beginexp explist)]
                [(defexp deflist exp2) (map (λ (defn) (processdef defn (top))) deflist)
                                       (eval-exp exp2)]
                ;...and so on, fill in these...
                [(debugexp) (begin
                              (vector-set! stacks stacksindex stack)
                              (set! stacksindex (+ 1 stacksindex)))])]))
                ;(begin
                ; (print-current-environment (top))
                             

;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  (match explist
    [(cons exp '()) (eval-exp exp)]
    [(cons exp rest) (eval-exp exp) (process-beginexp rest)]))

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (match deflist
    [(cons a '()) (eval-exp (lett deflist exp))]
    [(cons a rest) (eval-exp (lett (list a) (lets rest exp)))]))

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
  (match fr
    [(frame n b (emptyframe)) (displayln fr)
                              (displayln "@@@@@@@@@@@@@@@@@@@@@@@")]
    [(frame n b parent) (displayln fr)
                        (print-current-environment parent)]))

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (cleanup)
  (set!  stacks (make-vector 100))
  (set! stacksindex 0)
  (set! framenumber 0)
  (set! stack '())
  (push (createframe (make-hash '()) (emptyframe))))

(define (search sym fr)
  (match fr
    [(emptyframe) (emptyframe)]
    [(frame n b p) (if (hash-has-key? b sym)
                       fr
                       (search sym p))]))