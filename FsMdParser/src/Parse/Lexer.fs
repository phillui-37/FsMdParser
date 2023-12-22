namespace FsMdParser.Parse

/// the Lexer will execute the token parsing works by 2 pass
/// 1st: text -> Simple Token / Complicate Token need further handling
/// 2nd: Complicate Token -> Simple Token

// TODO text -> token
// Result <|> Result (Ok/Fail)
module Lexer =
    open FsMdParser
    open FSharpx.Collections
    open System.Text.RegularExpressions

    // private
    let inline private (<|>) (ra: Result<'a, ParseError>) (rb: Result<'a, ParseError>) =
        match ra with
        | Ok a -> Ok a
        | Error _ -> rb

    let private get = function
        | Ok x -> x
        | Error e -> raise <| System.AccessViolationException "Should not reach this"

    let rec private ParseHeading row =
        match row with
        | Prefix "# " s -> Ok (H1, s)
        | Prefix "## " s -> Ok (H2, s)
        | Prefix "### " s -> Ok (H3, s)
        | Prefix "#### " s -> Ok (H4, s)
        | Prefix "##### " s -> Ok (H5, s)
        | Prefix "###### " s -> Ok (H6, s)
        | s when s.Replace("=", "").Length = 0 -> NeedPreviousLine None |> Error
        | s when s.Replace("-", "").Length = 0 -> NeedPreviousLine None |> Error
        | _ -> ParseFailure |> Error
        |> Result.map (fun (level, s) ->
            Heading(level, (ParseLine s), None) |> MDHeading)
    and private ParseLineBreak row =
        let (|Br|_|) (in': string) =
            let pattern = "\<br\s*\/?\>(.*)"
            let m = Regex.Match(in', pattern)
            if m.Success then
                Some [ for g in m.Groups -> g.Value ]
            else
                None
        match row with
        | Br (s :: _) -> Ok << MDRow <| DList.singleton(MDLineBreak).Conj(ParseLine s)
        | _ -> Error ParseFailure
    and private ParseEmphasis row =
        
    and private ParseLine row =
        ParseHeading row
        <|> ParseLineBreak row
        <|> Ok(MDText row)
        |> get

    // paragraph should be parsed and handled in second pass
    

    // Public
    let Parse rows = ()
