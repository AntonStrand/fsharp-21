namespace FSharp21

module InputValidation =
  let tryToInt (s:string) = 
    match System.Int32.TryParse s with
    | true, v -> Some v
    | false, _ -> None

  let maybeWithinScope x =
    if x > 0 && x < 31
    then Some x
    else None

  let isValidInput x =
    tryToInt x |> Option.bind maybeWithinScope