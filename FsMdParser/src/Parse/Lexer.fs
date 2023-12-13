namespace FsMdParser.Parse

/// the Lexer will execute the token parsing works by 2 pass
/// 1st: text -> Simple Token / Complicate Token need further handling
/// 2nd: Complicate Token -> Simple Token

// TODO text -> token
// Result <|> Result (Ok/Fail)
module Lexer =
    open FSharpPlus
    open FsMdParser

    let rec private ParseHeading row =
        match row with
        | Prefix "# " s -> Ok (H1, s)
        | Prefix "## " s -> Ok (H2, s)
        | Prefix "### " s -> Ok (H3, s)
        | Prefix "#### " s -> Ok (H4, s)
        | Prefix "##### " s -> Ok (H5, s)
        | Prefix "###### " s -> Ok (H6, s)
        | _ -> ParseFailure |> Error
        |> Result.map (fun (level, s) ->
            Heading(level, (ParseLine s), None) |> MDHeading)
    and private ParseLine row =
        ParseHeading row
        <|> Ok(MDText row)
        |> Result.get
    
    let Parse rows = ()
    ()
