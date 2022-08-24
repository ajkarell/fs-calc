open System

let addOp lhs rhs = lhs + rhs
let subOp lhs rhs = lhs - rhs
let mulOp lhs rhs = lhs * rhs
let divOp lhs rhs = lhs / rhs

type OpFun = int -> int -> int

type Token =
    | Op of OpFun
    | Integer of int

let parseToken (word: string): Token option =
    let number = 
        try Some(int word)
        with
        | :? FormatException -> None 
    if number.IsSome then Some(Integer(number.Value))
    else 
        match word with
        | "+" -> Some(Op(addOp))
        | "-" -> Some(Op(subOp))
        | "*" -> Some(Op(mulOp))
        | "/" -> Some(Op(divOp))
        | _ -> None

let isSome (option: Token option) = option.IsSome

let tokenize (input: string) =
    let words = (input.Split " ") |> Array.toList
    words |> List.choose parseToken

exception EvaluationException

let rec evaluate (expression: Token list) : int option =
    if expression.IsEmpty then None
    else if expression.Length <= 2 then
        match expression[0] with
        | Integer (i) -> Some(i)
        | _ -> None
    else
    try
        let tokens = expression |> List.take 3

        let lhs = match tokens[0] with | Integer(i) -> i | _ -> raise EvaluationException
        let op = match tokens[1] with | Op(fn) -> fn | _ -> raise EvaluationException
        let rhs = match tokens[2] with | Integer(i) -> i | _ -> raise EvaluationException

        let calculated = op lhs rhs
        let reducedExpression = Integer(calculated) :: (expression |> List.skip 3)
        evaluate reducedExpression
    with
    | EvaluationException -> None

let rec loop () =
    printf ">> "
    let input = Console.ReadLine()
    let expression = tokenize input
    let result = evaluate expression

    match result with
    | Some(i) -> i.ToString()
    | None -> "Virhe"
    |> printfn "%s"

    loop ()

loop()