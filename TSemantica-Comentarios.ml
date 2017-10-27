(*  
	UNIVERSIDADE FEDERAL DO RIO GRANDE DO SUL
	INSTITUTO DE INFORMÁTICA
	INF05516 - SEMÂNTICA FORMAL N - TURMA U
	PROF. DR. ÁLVARO MOREIRA

	TRABALHO FINAL 2017/10 - CÓDIGO COMENTADO

	217434 - Guillermo Falcão Amaya
	220485 - Leonardo Bissani
	219424 - Matheus Anzzulin
*)

(* ---------------------------------------------------------------------------------------- *)
(* Avaliador Big Step para Linguagem L1 *)
(* Regras de L1 *)

(*
	Uma variável é uma string, pois é um identificador
*)
type variavel = string

(*
	Os operadores da linguagem
*)
type operador = 
	| Soma
	| Subt
	| Mult
	| Div
	| Igual
	| MenorIgual
	| And
	| Or
	| Not

(*
	Os tipos da linguagem, TipoInt(Inteiros), 
	TipoBool(Booleanos), TipoT(Genérico) e 
	TipoFn(Funções(T->T))
*)
type tipo = 
	| TipoInt
	| TipoBool
	| TipoT
	| TipoFn of tipo * tipo

(*
	Expressões da linguagem conforme as regras
*)
type expressao =
	| Num of int
	| Bool of bool
	| Op of expressao * operador * expressao
	| If of expressao * expressao * expressao
	| Var of variavel
	| Ap of expressao * expressao
	| Fn of variavel * expressao
	| Let of variavel * expressao * expressao
	| LetRec of variavel * variavel * expressao * expressao
	| Raise

(*
	Valores da linguagem conforme as regras
*)
type valor =
	| ValorNumerico of int
	| ValorBooleano of bool
	| ValorClosure of variavel * expressao * ambiente
	| ValorClosureRec of variavel * variavel * expressao * ambiente
	| ValorRaise
and
(*
	Ambiente (uma lista com tuplas compostas por variavel e valor associado a ela)
*)
	ambiente = (variavel * valor) list

(* ---------------------------------------------------------------------------------------- *)
(* Exceções *)

exception ExcecaoNaoHaValor				(* Caso não seja o valor correto para a expressão/operação *)
exception ExcecaoNaoHaAvaliacao			(* Caso não haja como avaliar a expressão/operação *)
exception ExcecaoVariavelNaoDefinida	(* Caso a variável não esteja definida no ambiente *)
exception ExcecaoNaoEhDoTipo			(* Caso não seja do tipo correto para a inferência *)

(* ---------------------------------------------------------------------------------------- *)
(* Operadores *)

(* Soma dois valores, caso ambos sejam valores numéricos, então o seu retorno é a soma deles e também é um valor numérico *)
let soma (a: valor)(b: valor) : valor =
	match (a, b) with
	| (ValorNumerico(x), ValorNumerico(y)) -> ValorNumerico(x + y)
	| _ -> raise ExcecaoNaoHaValor

(* Subtração dois valores, caso ambos sejam valores numéricos, então o seu retorno é a diferença entre eles e também é um valor numérico *)
let subt (a: valor)(b: valor) : valor =
	match (a, b) with
	| (ValorNumerico(x), ValorNumerico(y)) -> ValorNumerico(x - y)
	| _ -> raise ExcecaoNaoHaValor

(* Multiplica dois valores, caso ambos sejam valores numéricos, então o seu retorno é o produto deles e também é um valor numérico *)
let mult (a: valor)(b: valor) : valor =
	match (a, b) with
	| (ValorNumerico(x), ValorNumerico(y)) -> ValorNumerico(x * y)
	| _ -> raise ExcecaoNaoHaValor

(* Divide dois valores, caso ambos sejam valores numéricos, então o seu retorno é a divisão deles e também é um valor numérico *)
let div (a: valor)(b: valor) : valor =
	match (a, b) with
	| (ValorNumerico(x), ValorNumerico(y)) -> ValorNumerico(x / y)
	| _ -> raise ExcecaoNaoHaValor

(* Compara os valores, se ambos forem valores numéricos, então o seu retorno é um valor booleano com o resultado da comparação *)
let igual (a: valor)(b: valor) : valor =
	match (a, b) with
	| (ValorNumerico(x), ValorNumerico(y)) -> ValorBooleano(x = y)
	| _ -> raise ExcecaoNaoHaValor

(* Compara os valores, se ambos forem valores numéricos, então o seu retorno é um valor booleano com o resultado da comparação *)
let menorigual (a: valor)(b: valor) : valor =
	match (a, b) with
	| (ValorNumerico(x), ValorNumerico(y)) -> ValorBooleano(x <= y)
	| _ -> raise ExcecaoNaoHaValor

(* Faz um and entre os valores, se ambos forem valores booleanos, então o seu retorno é um valor booleano com o resultado da operação *)
let opand (a: valor)(b: valor) : valor =
	match (a, b) with
	| (ValorBooleano(x), ValorBooleano(y)) -> ValorBooleano(x && y)
	| _ -> raise ExcecaoNaoHaValor

(* Faz um or entre os valores, se ambos forem valores booleanos, então o seu retorno é um valor booleano com o resultado da operação *)
let opor (a: valor)(b: valor) : valor =
	match (a, b) with
	| (ValorBooleano(x), ValorBooleano(y)) -> ValorBooleano(x || y)
	| _ -> raise ExcecaoNaoHaValor

let opnot (a: valor) : valor =
	match a with
	| (ValorBooleano(x)) -> ValorBooleano(not x)
	| _ -> raise ExcecaoNaoHaValor

(* ---------------------------------------------------------------------------------------- *)
(*--- Ambiente ---*)

(* 
	lookupAmbiente recebe um ambiente e uma variável e retorna um valor
	faz o pattern matching do ambiente
	se encontrar ambiente vazio, então raise exceção de variável não definida no ambiente
	se encontrar uma tupla (variavel, valor) concatenada com um tail (resto) então
		se a variável é igual a x (que é variável de entrada), então retorna o valor dela
		senão chama lookupAmbiente recursivamente com o resto do ambiente e a variável x
 *)
let rec lookupAmbiente (ambiente: ambiente) (x: variavel) : valor =
     match ambiente with 
     | [] -> raise ExcecaoVariavelNaoDefinida
     | (variavel, valor) :: tl ->
        if variavel = x then 
            valor
        else
            lookupAmbiente tl x

let rec lookupAmbiente2 (ambiente: ambiente) (x: variavel) : valor option =
     match ambiente with 
     | [] -> None
     | (variavel, valor) :: tl ->
        if variavel = x then 
            Some valor
        else
            lookupAmbiente2 tl x

(*
	adicionaAmbiente recebe um ambiente, uma variável x, e um valor v e retorna um ambiente
	e simplesmente concatena ao ambiente a variável x e o valor associado a ela
*)
let adicionaAmbiente (ambiente: ambiente) (x: variavel) (v: valor) : ambiente =
    (x, v) :: ambiente

(* ---------------------------------------------------------------------------------------- *)
(*--- Semântica Operacional Big Step ---*)

(* avalia é a função recursiva que recebe um ambiente e uma expressão e retorna um valor através da semântica big step *)
let rec avalia (ambiente: ambiente)(e: expressao) : valor =
    match e with
    (* Caso seja raise, então o valor é raise *)
    | Raise -> ValorRaise
    
    (* Caso a expressão seja uma exp. numérica Num, então o valor é ValorNumerico *)
    | Num e -> ValorNumerico e
    (* Caso a expressão seja uma exp. booleana Bool, então o valor é ValorBooleano *)
    | Bool e -> ValorBooleano e
    
    (* Caso a expressão seja uma variável, procura no ambiente e retorna o valor da variável *)
    | Var e -> lookupAmbiente ambiente e
    
    (* 
    	Caso seja a expressão if, então avalia o primeiro elemento
		Se o primeiro elemento for raise, então o valor é raise
		Se o primeiro elemento for true, então avalia o segundo elemento
		Se o primeiro elemento for false, então avalia o terceiro elemento
		Qualquer coisa que não for isso, retorna a exceção	
    *)
    | If(e1, e2, e3) ->
        (match avalia ambiente e1 with
        | ValorRaise -> ValorRaise
        | ValorBooleano true -> avalia ambiente e2
        | ValorBooleano false -> avalia ambiente e3
        | _ -> raise ExcecaoNaoHaAvaliacao
        )
    
    (*
		Caso a expressão avaliada seja uma operação
		Primeiro avalia o primeiro elemento
		Se ele for raise, então é valor raise
		Se ele for qualquer coisa diferente disto, então avalia-se o segundo elemento
		Se o segundo elemento for raise, então o valor é raise
		Se for qualquer outra coisa, então faz pattern matching com o elemento op (operação)
		Se op for soma, então soma os valores e retorna o resultado
		O mesmo vale para todas as outras operações

		Lembrando que é garantido que os elementos para operações numéricas serão números,
		para operações com booleanos serão booleanos e assim por diante, como codado nas
		linhas (90 a 140)
    *)
    | Op(e1, op, e2) -> (
        let v1 = avalia ambiente e1 in
        match v1 with
        | ValorRaise -> ValorRaise
        | _ ->
            let v2 = avalia ambiente e2 in
            match v2 with
            | ValorRaise -> ValorRaise
            | _ -> (match op with
                    | Soma -> soma v1 v2
                    | Subt -> subt v1 v2
                    | Mult -> mult v1 v2
                    | Div -> div v1 v2
                    | Igual -> igual v1 v2
                    | And -> opand v1 v2
                    | Or -> opor v1 v2
                    | Not -> opnot v1
                    | MenorIgual -> menorigual v1 v2))

    (* 
    	Se for uma função, então retorna um ValorClosure
		com a variável, a expressão e o ambiente
     *)
    | Fn(x, e1) -> (
        ValorClosure(x, e1, ambiente))

    (*
		Se for uma aplicação de uma expressão em outra
		Então avalia o primeiro elemento e
		Se for ValorRaise, então retorna ValorRaise
		Se for qualquer outra coisa, avalia o segundo elemento
		Através de pattern matching, se o primeiro elemento for argumento x,
		mas o segundo for ValorRaise, então o retorno é ValorRaise
		Se o primeiro for uma closure e o segundo um valor v, então
		substitui a variável por valor no corpo da função
		Se for closure recursivo, a única diferença é que tem o f que é
		a definição da própria closure recursiva para chamadas recursivas
    *)
    | Ap(e1, e2) -> (
        let e1' = avalia ambiente e1 in
            match e1' with
            | ValorRaise -> ValorRaise
            | _ ->
                let e2' = avalia ambiente e2 in
                    match (e1', e2') with
                    | (x, ValorRaise) -> ValorRaise
                    | (ValorClosure(x, e, env), v) ->
                        let env' : ambiente = (adicionaAmbiente env x v) in
                            avalia env' e
                    | (ValorClosureRec(f, x, e, env), v) ->
                        let env' : ambiente = (adicionaAmbiente (adicionaAmbiente env x v)  f (ValorClosureRec(f, x, e, env))) in
                            avalia env' e
                    | _ -> raise ExcecaoNaoHaAvaliacao)

    (* 
		Se for Let, então avalia e1 para um valor e substitui e2 por esse valor
    *)
    | Let(x, e1, e2) -> (
        let env' : ambiente = (adicionaAmbiente ambiente x (avalia ambiente e1)) 
            in avalia env' e2)

    (* 
    	Se for LetRec, então funciona da mesma forma que o Let, mas mantém o f preso no e1
     *)
    | LetRec(f, x, e1, e2) -> (
        let closerec = ValorClosureRec(f, x, e1, ambiente) in
           avalia (adicionaAmbiente ambiente f closerec) e2)

(*--- Final do Big Step para Linguagem L1  ---*)
(* ---------------------------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------------------------- *)
(*--- Inferência de Tipos (typeInfer) ---*)

(* typeInfer recebe um ambiente e uma expressão e retorna o tipo da expressão *)
let rec typeInfer (ambiente: ambiente) (e: expressao) : tipo =
	(* Faz pattern matching da expressão *)
    match e with
    (* Se for Num é inteiro, logo é TipoInt *)
    | Num e -> TipoInt

	(* Se for Bool é booleano, logo é TipoBool *)
    | Bool e -> TipoBool
    
    (* 
    	Se for Op é uma operação, logo é avalia ambos os termos t1 e t2
    	e através de pattern matching compara, se ambos forem tipos inteiros
    	então o retorno da soma também é um tipo inteiro

    	O mesmo equivale para todas as operações, cada qual com seu tipo de entrada e retorno
    	Caso não encaixe, então é mal tipado
     *)
	| Op(t1, Soma, t2) -> (
		let typet1 = typeInfer ambiente t1 in
		let typet2 = typeInfer ambiente t2 in
		match (typet1, typet2) with
		| (TipoInt, TipoInt) -> TipoInt
		| _ -> raise ExcecaoNaoEhDoTipo
	)
	
	| Op(t1, Subt, t2) -> (
		let typet1 = typeInfer ambiente t1 in
		let typet2 = typeInfer ambiente t2 in
		match (typet1, typet2) with
		| (TipoInt, TipoInt) -> TipoInt
		| _ -> raise ExcecaoNaoEhDoTipo
	)
	
	| Op(t1, Mult, t2) -> (
		let typet1 = typeInfer ambiente t1 in
		let typet2 = typeInfer ambiente t2 in
		match (typet1, typet2) with
		| (TipoInt, TipoInt) -> TipoInt
		| _ -> raise ExcecaoNaoEhDoTipo
	)
	
	| Op(t1, Div, t2) -> (
		let typet1 = typeInfer ambiente t1 in
		let typet2 = typeInfer ambiente t2 in
		match (typet1, typet2) with
		| (TipoInt, TipoInt) -> TipoInt
		| _ -> raise ExcecaoNaoEhDoTipo
	)
	
	| Op(t1, Igual, t2) -> (
		let typet1 = typeInfer ambiente t1 in
		let typet2 = typeInfer ambiente t2 in
		match (typet1, typet2) with
		| (TipoInt, TipoInt) -> TipoBool
		| _ -> raise ExcecaoNaoEhDoTipo
	)
	
	| Op(t1, MenorIgual, t2) -> (
		let typet1 = typeInfer ambiente t1 in
		let typet2 = typeInfer ambiente t2 in
		match (typet1, typet2) with
		| (TipoInt, TipoInt) -> TipoBool
		| _ -> raise ExcecaoNaoEhDoTipo
	)
	
	| Op(t1, And, t2) -> (
		let typet1 = typeInfer ambiente t1 in
		let typet2 = typeInfer ambiente t2 in
		match (typet1, typet2) with
		| (TipoBool, TipoBool) -> TipoBool
		| _ -> raise ExcecaoNaoEhDoTipo
	)
	
	| Op(t1, Or, t2) -> (
		let typet1 = typeInfer ambiente t1 in
		let typet2 = typeInfer ambiente t2 in
		match (typet1, typet2) with
		| (TipoBool, TipoBool) -> TipoBool
		| _ -> raise ExcecaoNaoEhDoTipo
	)
	
	(* 
		Se for uma expressão If, avalia os três termos da expressão e através de pattern matching
		compara se o primeiro termo é booleano. Se ele for booleano, então através de um if
		compara se os termos e2 e e3 da expressão são do mesmo tipo, como pede a regra do sistema
		de tipos. Se forem, então retorna o tipo de e2 (podia ser e3, tanto faz), caso contrário
		retorna um raise
	 *)
	| If(e1, e2, e3) -> (
		let exp1 = typeInfer ambiente e1 in
		let exp2 = typeInfer ambiente e2 in
		let exp3 = typeInfer ambiente e3 in
		match (exp1) with
		| (TipoBool) -> (if (exp2 = exp3) then (exp2) else (raise ExcecaoNaoEhDoTipo))
		| _ -> raise ExcecaoNaoEhDoTipo
	)
	
	| Var(v) -> ( 
	    let valor = lookupAmbiente2 ambiente v in
	    match valor with
	    | Some (ValorNumerico _) -> TipoInt
	    | Some (ValorBooleano _) -> TipoBool
	    | Some ValorClosure(var, expr, ambiente') -> (
	        let tipoVar = typeInfer ambiente' (Var var) in
	        let tipoExpr = typeInfer ambiente' expr in
	        TipoFn(tipoVar, tipoExpr)
	    )
	    | None -> TipoT
	)
	| _ -> raise ExcecaoNaoEhDoTipo
	
	(* 
		Se for uma expressão de aplicação, então avalia os termos da expressão 
		e através de pattern matching analisa se o primeiro termo é do tipo Fn.
		Se for, então se t1 for igual a t2, então ele retorna o tipo de t2, como
		pede a regra do sistema de tipos para aplicação. Caso contrário, retorna
		uma exceção.
	*)
	| Ap(e1, e2) -> (
		let exp1 = typeInfer ambiente e1 in
		let exp2 = typeInfer ambiente e2 in
		match exp1 with
		| TipoFn(t1, t2) -> (if (t1 = t2) then (t2) else (raise ExcecaoNaoEhDoTipo))
		| _ -> raise ExcecaoNaoEhDoTipo
	)

(*--- Final da Inferência de Tipos (typeInfer) ---*)
(* ---------------------------------------------------------------------------------------- *)


(* ---------------------------------------------------------------------------------------- *)
(*--- Testes para bigStep e typeInfer  ---*)

(* Cria valores para testes *)
let numero0 = ValorNumerico 0;;
let numero1 = ValorNumerico 1;;
let numero2 = ValorNumerico 2;;
let numero3 = ValorNumerico 3;;
let numero4 = ValorNumerico 4;;
let numero5 = ValorNumerico 5;;
let numero6 = ValorNumerico 6;;
let numero7 = ValorNumerico 7;;
let numero8 = ValorNumerico 8;;
let numero9 = ValorNumerico 9;;
let numero10 = ValorNumerico 10;;
let booleanoTrue = ValorBooleano true;;
let booleanoFalse = ValorBooleano false;;

(* Ambiente - lista vazia *)
let ambiente : ambiente = [];;

(* Cria um ambiente com variáveis para teste *)
let ambiente = adicionaAmbiente ambiente "var1" numero1;;
let ambiente = adicionaAmbiente ambiente "var2" numero2;;
let ambiente = adicionaAmbiente ambiente "var3" numero3;;
let ambiente = adicionaAmbiente ambiente "var4" numero4;;
let ambiente = adicionaAmbiente ambiente "var5" numero5;;
let ambiente = adicionaAmbiente ambiente "var8" numero8;;
let ambiente = adicionaAmbiente ambiente "varTrue" booleanoTrue;;
let ambiente = adicionaAmbiente ambiente "varFalse" booleanoFalse;;

(* Expressões (numéricas e booleanas) para testes *)
let num0 = Num 0;;
let num1 = Num 1;;
let num2 = Num 2;;
let bTrue = Bool true;;
let bFalse = Bool false;;

(* Expressões (operações) de soma, subt, mult, and, or, not para testes *)
let somaDoisValores = Op(Var "var1", Soma, Var "var2");;
let subtDoisValores = Op(Var "var8", Subt, Var "var3");;
let multDoisValores = Op(Var "var4", Mult, Var "var5");;
let divDoisValores = Op(Var "var8", Div, Var "var2");;

(* Expressões (operações) de Igual e MenorIgual *)
let igualTrue = Op(Var "var1", Igual, Var "var1");;
let igualFalse = Op(Var "var3", Igual, Var "var8");;
let menorIgualTrue = Op(Var "var3", MenorIgual, Var "var8");;
let menorIgualFalse = Op(Var "var5", MenorIgual, Var "var1");;

(* Expressões (operações) de And e Or *)
let andTrueTrue = Op(Var "varTrue", And, Var "varTrue");;
let andTrueFalse = Op(Var "varTrue", And, Var "varFalse");;
let orFalseFalse = Op(Var "varFalse", Or, Var "varFalse");;
let orTrueFalse = Op(Var "varTrue", And, Var "varFalse");;

(* Expressões com Operadores *)
let soma2 = Op(Var "var1", Soma, Var "var2");;	(* soma2 = 1 + 2 *)
let subt2 = Op(Var "var8", Subt, Var "var3");;	(* O mesmo raciocínio vale para todos abaixo *)
let mult2 = Op(Var "var1", Mult, Var "var2");;
let div2 = Op(Var "var8", Div, Var "var2");;
let igual2 = Op(Var "var5", Igual, Var "var3");;
let igual3 = Op(Var "var8", Igual, Var "var8");;
let mIgual2 = Op(Var "var7", MenorIgual, Var "var7");;
let mIgual3 = Op(Var "var2", MenorIgual, Var "var4");;
let and2 = Op(Var "varTrue", And, Var "varTrue");;
let and3 = Op(Var "varFalse", And, Var "varTrue");;
let or2 = Op(Var "varTrue", Or, Var "varFalse");;
let or3 = Op(Var "varFalse", Or, Var "varFalse");;

(* Expressões com If Then Else *)
let ifThenElseTrue = If(bTrue, soma2, subt2);;	(* Por ser true, ele avalia para a expressão soma2, que é 1 + 2 = 3 *)
let ifThenElseFalse = If(bFalse, soma2, subt2);;	(* Por ser falso, ele avalia para a expressão subt2, que é 8 - 3 = 5 *)
let ifThenElseTrue2 = If(or2, div2, subt2);;	(* or2 resulta em true, logo irá avaliar div2, que é 8/2, resultando em 4 *)
let ifThenElseFalse2 = If(and3, igual2, igual3) (* and3 resulta em false, logo irá avaliar igual3, que é 8 = 8, que é true *)

(* Teste de raises para variável inexistente no ambiente *)
(* Exception variavel não definida no ambiente *)
let somaDoisValores2 = Op(Var "var10", Soma, Var "var5");;
(* Exception variavel não é um valor *)
let somaDoisBooleanos = Op(Var "varTrue", Soma, Var "var5");;

(* Teste de função *)
let funcaoSoma2 = Fn("var1", soma2);;

(* Teste de Let *)
let let2 = Let("f", soma2, Var("var1"));;

(* Teste Final - cálculo de fatorial do número desejado *)
let numero = 6;;
let igualA1 = Op(Var "x", Igual, Num(1));;
let menos1 = Op(Var "x", Subt, Num(1));;
let funcao = If(igualA1, Num(1), Op(Var "x", Mult, Ap(Var "fat", menos1)));;
let fatorial = LetRec("fat", "x", funcao, Ap(Var "fat", Num numero))

(* Observações para uso correto do trabalho *)
(*	Obs.: Para avaliar com bigStep deve-se chamar: "avalia (nome_do_ambiente)(nome_da_expressão)" *)
(*	Obs.: Para fazer inferência de tipos usa-se typeInfer, deve-se chamar: "typeInfer (nome_do_ambiente)(nome_da_expressão)" *)