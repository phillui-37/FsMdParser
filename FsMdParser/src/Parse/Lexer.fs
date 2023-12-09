namespace FsMdParser.Parse

/// the Lexer will execute the token parsing works by 2 pass
/// 1st: text -> Simple Token / Complicate Token need further handling
/// 2nd: Complicate Token -> Simple Token

// TODO text -> token
// Result <|> Result (Ok/Fail)
module Lexer =
    open FSharpPlus

    let ParseHeading row =
        if String.startsWith "#" row then
            Ok ""
        else
            Error ()

    let Parse rows =
        ()
    ()