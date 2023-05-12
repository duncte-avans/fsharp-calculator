type Operator =
    | Add
    | Subtract
    | Multiply
    | Divide

type Expression =
    | Constant of double
    | Apply of Operator * Expression * Expression
    | Average of List<Expression>
   
let doMaths o n m =
    match o with
    | Add -> n + m
    | Subtract -> n - m
    | Multiply -> n * m
    | Divide -> n / m

let rec eval (e: Expression) =
    match e with
    | Constant(v) -> v
    | Apply(o, e1, e2) ->
        let e1Res = eval e1
        let e2Res = eval e2
        doMaths o e1Res e2Res
    | Average(l) ->
        match l with
        | [] -> 0
        | goodList ->
            goodList
            |> List.map eval
            |> List.average

let num5 = Constant(5)
let operator = Apply(Add, num5, num5)

let shouldBe25 = Apply(Multiply, num5, num5)

let averageOperator = Average([operator; shouldBe25])

// (5 + 5) + (5 * 5)
let bla = Apply(Add, operator, shouldBe25)

let evalRes = eval bla
printfn $"{evalRes}"
