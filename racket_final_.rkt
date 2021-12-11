#lang pl

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL> } ;; intersection should be on two objects(lists) of SOL. 
        |  { union <SOL> <SOL> }  ;;union should be on two objects(lists) of SOL.
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
    [Set  SET]
    [Smult Number SOL] ;; 
    [Inter SOL SOL] ;; 
    [Union SOL SOL] ;; 
    [Id    Symbol]
;;    [With  Symbol SOL SOL] -- not to be used, syntactic sugar for ...
    [Fun Symbol Symbol SOL]
    [CallS SOL SOL SOL]
    [CallD SOL SOL SOL])

;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 


;; ismember? function: Returns: True || False.
;; --------------------
;; parameters:
;; -----------
;; 1) Number
;; 2) Set (list) of Numbers.
;; The function takes number and a list and checks if the number is in the list.
;; thats done using this steps: 
;; a) if the list is empty then return false. 
;; b) else if the number entered to the function is equal to the current element in the list it will returns true.
;; c) if the current element isnt match with the given number it will requsivley check with the rest of the list.
;; untill null accurance.(end of list) 
  (: ismember? : Number SET  -> Boolean)
  (define (ismember? n l)
    (cond [(null? l) #f]
          [(= n (first l)) #t]
          [else (ismember? n (rest l))]))


  (test (not (ismember? 1 '(3 4 5))))
  (test (not (ismember? 1 '( 3 2 3 5 6))))
  (test (ismember? 1 '(3 4 5 1 3 4)))
  (test (ismember? 1 '(1)))
  (test (not (ismember? 22 '(3 4 5))))
  (test (not (ismember? 1 '( 5 4 5 6 4 5))))
  (test (ismember? 22 '(22 22 22 22 22)))
  (test (not (ismember? 1 '())))


;; remove-duplicates function: Returns: Set of numbers without duplications.
;; --------------------
;; parameters:
;; -----------
;; 1) Set
;; 
;; For given set the function go over the list and remove any duplication within the set.
;; thats done using this steps: 
;; a) if the list of numbers is null (empty list) or there is only one element in the list it will return the list.  
;; b) else if the current element is-member in the rest of the list it will remove it by reqursivley sending the rest of the
;;    list to remove-duplicates function 
;; c) if the current element isnt a member in the rest of the list it will reqursivley sends the rest to remove-duplicates but saves the
;; current element by using the cons operation to add all the element of the list when the reqursive call is going back.
  (: remove-duplicates : SET  -> SET)
  (define (remove-duplicates l)
    (cond [(or (null? l) (null? (rest l))) l]
          [(ismember? (first l) (rest l)) (remove-duplicates (rest l))] 
          [else (cons (first l) (remove-duplicates (rest l)))]))

;; checking for the current element in the array if he is member in the rest of the list.
;; if so, take only the rest of the elements (remove the duplication which is the current number) 

;; empty list 
(test (remove-duplicates '()) => '())
;; no duplications tests:
(test (remove-duplicates '(1)) => '(1))
(test (remove-duplicates '(3 4 5 6 7)) => '(3 4 5 6 7))
(test (remove-duplicates '(1 2 3 4)) => '(1 2 3 4))
(test (remove-duplicates  (cons 1 (list 2 3 4))) => '(1 2 3 4))
(test (remove-duplicates '(9 4 3 1 12 17 66 77)) => '(9 4 3 1 12 17 66 77))
;; with duplications tests: 
(test (remove-duplicates '(3 4 5 1 3 4))=> '(5 1 3 4))
(test (remove-duplicates '(12 1 4 1 4 4))=> '(12 1 4))
(test (remove-duplicates '(4 4 4 4 4 4))=> '(4))
(test (remove-duplicates '(1 2 1 2 1 2))=> '(1 2))
(test (remove-duplicates '(1 2 2 2 2 1))=> '(2 1))
;given tests:
(test (remove-duplicates '(3 4 5)) => '(3 4 5))
(test (remove-duplicates '( 3 2 3 5 6)) => '(2 3 5 6))
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1))


;; create-sorted-set function: Returns: Set of numbers sorted low2high (without duplications).
;; --------------------
;; parameters:
;; -----------
;; 1) Set
;; 
;; For given set the function first remove duplication using the remove-duplicates function (above) and after that sorts
;; all the elements in the set from low to high using pre-built 'sort' function. 
;; thats done using this steps: 
;; a) sending the set to remove-duplicates
;; b) sort the returned set from (a)
  (: create-sorted-set : SET -> SET)
  (define (create-sorted-set l)
    (sort (remove-duplicates l) <))


; already sorted sets tests:
(test (create-sorted-set '())=> '())
(test (create-sorted-set '(1))=> '(1))
(test (create-sorted-set '(1 2 3 4 5 6))=> '(1 2 3 4 5 6))
(test (create-sorted-set '(1 12 24 48))=> '(1 12 24 48))
(test (create-sorted-set '(-1 3 4 6))=> '(-1 3 4 6))
; already sorted sets tests (with duplicates):
(test (create-sorted-set '(1 1 1 1 1 1 1 1))=> '(1))
(test (create-sorted-set '(1 2 2 3 4 5 5 6 6))=> '(1 2 3 4 5 6))
(test (create-sorted-set '(1 1 12 12 24 24 48 48))=> '(1 12 24 48))
(test (create-sorted-set '(-1 -1 3 3 4 4 6))=> '(-1 3 4 6))
; not sorted sets tests:
(test (create-sorted-set '(1 -3 -5 12 3 4))=> '(-5 -3 1 3 4 12))
(test (create-sorted-set '(1 100 2 -100 -2 -3))=> '(-100 -3 -2  1 2 100))
(test (create-sorted-set '(-1 -3 1 -5 12 3 200 4))=> '(-5 -3 -1 1 3 4 12 200))
(test (create-sorted-set '(1 2 -1 -2 -15 -3))=> '(-15 -3 -2 -1 1 2))
; not sorted sets tests (with duplicates):
(test (create-sorted-set '(1 -3 -5 12 3 4 1 -3 -5 12 3 4))=> '(-5 -3 1 3 4 12))
(test (create-sorted-set '(1 100 2 -100 -2 -3 100 2 -100 -2 -3))=> '(-100 -3 -2  1 2 100))
(test (create-sorted-set '(-1 -3 1 -5 12 3 200 4 12 3 200 -1 -3))=> '(-5 -3 -1 1 3 4 12 200))
(test (create-sorted-set '(1 2 -1 -2 -15 -3 2 -1 -2 -15 -3 -1 -2))=> '(-15 -3 -2 -1 1 2))


;; set-union function: Returns: The union of two Sets of numbers sorted low2high (without duplications).
;; --------------------
;; parameters:
;; -----------
;; 1) Set - NUMBER
;; 2) Set - NUMBER
;; For given sets the function uses append and created-sorted-set functions to obtain the union of the two sets.
;; thats done using this steps: 
;; a) appends A & B to create single set
;; b) remove duplications and sorts the new list using created-sorted-set.
  (: set-union : SET SET -> SET)
  (define (set-union A B)
    (create-sorted-set (append A B)))

; tests:
(test (set-union '() '()) => '())
(test (set-union '() '(1 15 3)) => '(1 3 15))
(test (set-union '(1 2 3 4) '()) => '(1 2 3 4))
(test (set-union '(1 2 3 4) '(1 15 3)) => '(1 2 3 4 15))
(test (set-union '(12 12 12 12 12 12) '(12)) => '(12))
(test (set-union '(1) '(1 1 1 1 1 1 1)) => '(1))
(test (set-union '(-2 -200 -13 -4) '(2 200 13 4)) => '(-200 -13 -4 -2 2 4 13 200))



;; set-intersection function: Returns: The intersection of two Sets of numbers sorted low2high (without duplications).
;; --------------------
;; parameters:
;; -----------
;; 1) Set - NUMBER
;; 2) Set - NUMBER
;; For given sets the function uses filter function which Returns a list with the elements of the input list
;; for which mem-filter (inner function) returns a true value.

;; thats done by using this steps: 
;; a) apply create-sorted-set on set B to remove duplicates and by the way sorts B
;; b) using filter operation going through each element in B and by using ismember? function check if the current
;;    element is a member in A -> if so the element will be in the returned list. 
  (: set-intersection : SET SET -> SET)
  (define (set-intersection A B)
    (: mem-filter : Number -> Boolean)
    (define (mem-filter n)
      (ismember? n A))
    (filter mem-filter (create-sorted-set B)))



(test (set-intersection '() '()) => '())
(test (set-intersection '(1 2 3) '(4 5 6)) => '())
(test (set-intersection '(3 4 5) '(3 4 5)) => '(3 4 5))
(test (set-intersection '(3 4 5) '(3)) => '(3))
(test (set-intersection '(3 4 5) '(1)) => '())
(test (set-intersection '(3 4 5 7) '(1 2 3 4 5 6 7)) => '(3 4 5 7))
(test (set-intersection '(3 4 5 7) '()) => '())
(test (set-intersection '(1 2 23 3) '(1 2 3)) => '(1 2 3))
(test (set-intersection '(1 2 3 55 32 44 100) '(2 2 3 44 100 1000 222)) => '(2 3 44 100))
(test (set-intersection '(1 1 1 1 1 1 1 1 1 2 3 55 32 44 100) '(1 1 1 2 2 3 44 100 1000 222)) => '(1 2 3 44 100))






;; ---------------------------------------------------------
;; Parser
  ;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable.


;; 
  (: parse-sexpr : Sexpr -> SOL)
  ;; to convert s-expressions into SOLs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates 
      [(symbol: name) (Id name)]

      
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (CallS (Fun name name (parse-sexpr body)) (parse-sexpr named) (parse-sexpr named))] ;;; there is no With constructor replace with existing constructors
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]

      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name1) (symbol: name2)) body)
          (if (eq? name1 name2)
              (error 'parse-sexpr "`fun' has a duplicate param name in ~s" sexpr) ;; cannot use the same param name twice  <-- changed.
              (Fun name1 name2 (parse-sexpr body)))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
      [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]  ;; changed 
      [(list 'call-dynamic fun arg1 arg2)(CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]  ;; changed add line
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))




  (: parse : String -> SOL)
  ;; parses a string containing a SOL expression to a SOL AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))



(test (parse "{with {hello world} c {}}") =error> "parse-sexpr: bad `with' syntax in (with (hello world) c ()")
(test (parse "{x}")  =error> "parse-sexpr: bad syntax in (x)")
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{fun {x y} {intersect {1 2 3} {4 2 3}}}") => (Fun 'x 'y (Inter (Set '(1 2 3)) (Set '(2 3 4)))))
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}} {call-static {fun {x y} {union x S}} {scalar-mult 3 S} {4 5 7 6 9 8 8 8}}}") =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}}} {call-static {fun {x y} {union x x}} {scalar-mult 3 S} {4 5 7 6 9 8 8 8}}}") =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'x))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
              {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters







;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl }, env)  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure



    eval({scalar-mult K E},env) =   (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,env) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)



    eval({intersect E1 E2},env) = (sort (create-set (set-intersection (eval(E1,env) , eval(E2,env))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise


    eval({union E1 E2},env) = (sort (create-set (eval(E1,env) , eval(E2,env))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise



    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))


    eval({fun {x1 x2} E},env)  = <{fun {x1 x2} E}, env>

    eval({call-static E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env), envf) ;; add to envarioment.
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise


    eval({call-dynamic E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env), env) ;; same envarioment.
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise

|#


;; Types for environments, values, and a lookup function

  (define-type ENV
    [EmptyEnv]
    [Extend Symbol VAL ENV])

  (define-type VAL
    [SetV SET]
    [FunV Symbol Symbol SOL ENV])

  (: lookup : Symbol ENV -> VAL)
  (define (lookup name env)
    (cases env
      [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
      [(Extend id val rest-env)
       (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

  (: SetV->set : VAL -> SET)
    (define (SetV->set v)
      (cases v
        [(SetV S) S]
        [else (error 'SetV->set "expects a set, got: ~s" v)]))


;;The difference between the notes of the lectures was that in this case I was needed to put everything inside the SETV builder.
;;using the MAP process which accepts as a first parameter the function we got mult-op and as a second parameter map
;;gets "SetV->set s" that we actually convert S from SETV to SET so we can work with it.
;;Difficulty: understands the usege of SetV->set s in the map
  (: smult-set : Number VAL -> VAL)
  (define (smult-set n s)
    (: mult-op : Number -> Number)
    (define (mult-op k)
      (* k n))
    (SetV (map mult-op (SetV->set s))))


 (: set-op :(SET SET -> SET) VAL VAL -> VAL )
  ;; gets a binary SET operator, and uses it within a SetV
  ;; wrapper
  (define (set-op op val1 val2)
     (SetV (op (SetV->set val1) (SetV->set val2))))




;;---------  the eval procedure ------------------------------


;;I use the previous assigments from the lectures to understand what is the right way to approach the evaluation proccedure.
;;When I getting Set S) I was needed to return the (SetV S) because the EVAL function returns VAL.

;;When I getting Smult n set I needed to run the smult-set and send the SET again to another evaluation proccedure with the same environment Im using for the decleration.
;;When I getting Inter l r OR Union l r a set-op was required on the same function and also send L and R to another evaluation the same environment.
;;When I getting CallS and in CASE on FVAL running evaluation on the BODY and in the environment only that extension was needed.
;;When I getting CallD same as previous state. 
;;            Only made an extension to our environment and not to the environment of fval.


;;Difficulty: poting on the difference between CallS and CallD

;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL ENV -> VAL)
  ;; evaluates SOL expressions by reducing them to set values
  (define (eval expr env)
    (cases expr
      [(Set S) (SetV S)]
      [(Smult n set) (smult-set n (eval set env))]
      [(Inter l r) (set-op set-intersection (eval l env) (eval r env))]
      [(Union l r) (set-op set-union (eval l env) (eval r env))]
      [(Id name) (lookup name env)]
      [(Fun bound-id1 bound-id2 bound-body)
       (FunV bound-id1 bound-id2 bound-body env)]
      [(CallS fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
           (eval bound-body (Extend bound-id1 (eval arg-expr1 env)
                                   (Extend bound-id2 (eval arg-expr2 env) f-env)))]
           [else (error 'eval "`call-static' expects a function, got: ~s"
                              fval)]))]
      [(CallD fun-expr arg-expr1 arg-expr2)
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body (Extend bound-id1 (eval arg-expr1 env)
                                   (Extend bound-id2 (eval arg-expr2 env) env)))]
           [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                              fval)]))]))


;;I used Extend as requested.
;;Each of the environments expands the environment that comes before hand There are 3 Extends.
;;Each one gets 3 organs as we learned (name value and Env).
;;The interior is designed for 'cons which gets two parameters('first 'second) and its environment is an empty environment.
;;The medium is intended for 'first wich also gets two parameters ('params1 'params2) and its environment is (Extend 'cons....
;;And the first is destined for 'second wich also gets two parameters ('params1 'params2) and its environment is (Extend 'first....
;;Each Extend expands the previous one.

;;difficulties: The exercise took a long time because roughly on the CONS we did other things and then changed according to the failures of the tests
  (: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
    (Extend 'second (FunV 'params1 'params2 (CallS (Id 'params1) (Fun 'fir 'sec (Id 'sec)) (Set '())) (EmptyEnv))
          (Extend 'first (FunV 'params1 'params2 (CallS (Id 'params1) (Fun 'fir 'sec (Id 'fir)) (Set '())) (EmptyEnv))
                  (Extend 'cons (FunV 'first 'second (Fun 'args1 'args2 (CallS (Id 'args1) (Id 'first) (Id 'second))) (EmptyEnv))
                          (EmptyEnv))))) 



;;According to the statement of the function gets a String and returns VAL or SET.
;;Because of the CASES the function must cover all cases.
;;In the case of (SetV S) the function returns S exactly as defined.
;;In "ELSE" we return the result because the result is actually VAL. (Because EVAL returns VAL)
;;        Difficulty: ELSE statement was tricky to figure out.
  (: run : String -> (U SET VAL))
  ;; evaluate a SOL program contained in a string
  (define (run str)
    (let ([result (eval (parse str)(createGlobalEnv))])
       (cases result
         [(SetV S) S]
         [else result])))


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{1 2 3  }") => '(1 2 3 ))
(test (run "{4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")

(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
(test (run "{}") => '())
(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{union {} {}}") => '())
(test (run "{intersect {} {}}") => '())
(test (run "x") =error> "lookup")
(test (run "y") =error> "lookup")

(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
(test (eval (CallD (Set '(110)) (Set '()) (Set '())) (EmptyEnv)) =error> "`call-dynamic' expects a function, got: #(struct:SetV (110))")
(test (SetV->set (FunV 'x 'y (Id 'x) (EmptyEnv))) =error> "SetV->set: expects a set")
(test (eval (CallD (Set '(1454)) (Set '()) (Set '())) (EmptyEnv)) =error> "`call-dynamic' expects a function")
(test (eval (CallD (Set '(46649)) (Set '(1445)) (Set '())) (EmptyEnv)) =error> "`call-dynamic' expects a function")




#|

Open questions:

1. 
Ans:

Types in SOL language:

  [Set  SET] - set
  [Smult Number SOL] - mult every element of the set with the number
  [Inter SOL SOL] - intersection
  [Union SOL SOL] - union
  [Id    Symbol]- identifier symbol
  [Fun   Symbol Symbol SOL] - function
  [CallS SOL SOL SOL] - static call
  [CallD SOL SOL SOL] - dynamic call

2.
Ans:
For overcome the number of parameters I exchanged the 'with statement with Call-Static and Fun
 and I used static calling to keep the envarioment error free from exchanging the symbols.
I will explain, 
 I choose to use "CallS" for several reasons when parsing with expressions :
    1) Because of the CALL STATIC definition.
       as I want to connect a name to experation, and also I needed that the connection will be to the value
       in that moment.
       On static model the function ran in the environment in which it was defined
       and all the variables recieve their values on definition and not on run time.
    2) The tests given to me the use of CALLS was quite clear so the guidence of the tests let me decide to use Stativlly.
    



3.
Ans:
 Tail Reqursion:
 
 * set-sorted-set
 * set-union
 * set-intersectiuon

 Just Reqursion:
 * ismember?
 * remove-duplicates

regular reqursion is a function that keep calling itself until a return statement is applied and then the function re-calling itself back.
which leads to use of growing memory in dynamic form. which can lead to stack overflow as the stack keep on filled with each reqursive call.

Tail reqursion on the other hand make use of another function which will do the reqursive calls.
that make the the function to return the value of the reqursion as soon as we hit the return statement - less memory consumption. 

4.
Ans:
In the GlobalEnv, I only used CALLS in all of them(In SECOND, FIRST and CONS).
But it is actually possible to combine and use CALLD as well but only within CONS.
First and second else use only CALLS.
This is because I wanted the evel of the body to be familiar with x and y thay inside what we recived from 'cons', and
also to prenvent the user from changing their behavior.
and CONS can use also CALLS or CALLD it depends on whether I want to give the user the option to change the CONS setting or not.

5.
Ans: 
In CONS it does not matter if we write CALLS or CALLD it will work one way or the other.
Regarding others it will also not cause any change because in our environment we have already set FIRST and SECOND with CALLS
so they already have preset values that will not change so there will be no difference.
|#



