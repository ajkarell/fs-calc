open System

let addOp lhs rhs = lhs + rhs
let subOp lhs rhs = lhs - rhs
let mulOp lhs rhs = lhs * rhs
let divOp lhs rhs = lhs / rhs

type OpFun = int -> int -> int

type Token =
    | Op of OpFun
    | Number of int

let parseNumber word =
    try
        word |> int |> Some
    with :? FormatException -> None 

let parseToken word =
    match word with
    | "+" -> addOp |> Op |> Ok
    | "-" -> subOp |> Op |> Ok
    | "*" -> mulOp |> Op |> Ok
    | "/" -> divOp |> Op |> Ok
    | _ -> 
        match parseNumber word with
        | Some i -> i |> Number |> Ok 
        | None -> Error "Failed to parse word"

let isSome (option: Token option) = option.IsSome

let chooseOk (result: Result<Token, string>) =
    match result with
    | Ok token -> Some token
    | Error _ -> None

let chooseError (result: Result<Token, string>) =
    match result with
    | Ok _ -> None
    | Error msg -> Some msg


let tokenize (input: string) =
    let words = input.Split " " |> Array.toList
    let results = words |> List.map parseToken
    let tokens = List.choose chooseOk results
    let errors = List.choose chooseError results
    if errors.Length > 0 then errors[0] |> Error
    else Ok tokens
    
let rec evaluate (expression: Token list) =
    if expression.IsEmpty then Error "Empty expression"
    else if expression.Length <= 1 then
        match expression[0] with
        | Number i -> Ok i
        | _ -> Error "Unexpected token"
    else
        let lhs = 
            match expression[0] with 
            | Number i -> Ok i 
            | _ -> Error "Unexpected token"

        let op = 
            match expression[1] with 
            | Op fn -> Ok fn 
            | _ -> Error "Unexpected token"

        let rhs = 
            match expression[2] with 
            | Number i  -> Ok i 
            | _ -> Error "Unexpected token"

        let calculated = 
            match lhs, op, rhs with
            | Error msg, _, _ -> Error msg
            | _, Error msg, _ -> Error msg
            | _, _, Error msg -> Error msg
            | Ok lhs, Ok op, Ok rhs -> op lhs rhs |> Ok

        match calculated with
        | Ok number -> 
            let reducedExpression = Number(number) :: (expression |> List.skip 3)
            evaluate reducedExpression
        | Error msg -> Error msg

let rec loop () =
    printf ">> "
    let input = Console.ReadLine()
    let result = 
        tokenize input
        |> Result.bind evaluate
        |> Result.map (fun x -> x.ToString())

    match result with
    | Ok result -> result
    | Error error -> error |> sprintf "Error: %s"
    |> printfn "%s"
    
    loop ()

loop()