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
            let m = RegexPattern.LINE_BREAK.Match in' 
            if m.Success then
                Some [ for g in m.Groups -> g.Value ]
            else
                None
        match row with
        | Br (s :: _) -> Ok << MDRow <| DList.singleton(MDLineBreak).Conj(ParseLine s)
        | _ -> Error ParseFailure
    and private ParseEmphasis row =
        let rec TryParse src ret: MDToken list =
            match src with
            | [] -> []
            | value :: remain ->
                let (pattern, mapper) = value
                let newList =
                    match row with
                    | Surround pattern pattern s -> (MDEmphasis << mapper <| ParseLine s) :: ret
                    | _ -> ret
                TryParse remain newList
        let result = TryParse [("**", Bold); ("__", Bold); ("*", Italic); ("_", Italic)] []
        match result with
        | [] -> Error ParseFailure
        | r -> Ok << MDRow <| DList.ofSeq r
    and private ParseBlockQuote row =
        let rec getLayer src accLayer =
            match src with
            | Prefix " " _ -> accLayer 
            | Prefix ">" s -> getLayer s accLayer + 1
            | _ -> raise <| System.AccessViolationException "Should not reach this"
        let (|BQ|_|) (in': string) =
            let m = RegexPattern.BLOCK_QUOTE.Match in'
            if m.Success then
                Some [for g in m.Groups -> g.Value]
            else
                None
        match row with
        | BQ (s :: _) ->
                let layer = getLayer row 0
                Ok <| MDBlockQuotes {layer=layer;content=ParseLine s}
        | _ -> Error ParseFailure
    and private ParseList row = // 2nd pass merge list together
        let (|OL|_|) src =
            let m = RegexPattern.ORDERED_LIST.Match src
            if m.Success then
                let indent::_::content::_ = [for g in m.Groups -> g.Value]
                Some (indent.Length, content)
            else
                None
        let getSymbolType = function
                | "-" -> Dash
                | "*" -> Asterisk
                | "+" -> Plus
                | _   -> Dash
        let (|UOL|_|) src =
            let m = RegexPattern.UNORDERED_LIST.Match src
            if m.Success then
                let indent::symbol::content::_ = [for g in m.Groups -> g.Value]
                let symbolType = getSymbolType symbol
                Some (indent.Length, symbolType, content)
            else
                None
        let (|CUOL|_|) src =
            let m = RegexPattern.CHECKED_UNORDERED_LIST.Match src
            if m.Success then
                let indent::symbol::checkSymbol::content::_ = [for g in m.Groups -> g.Value]
                let symbolType = getSymbolType symbol
                let isChecked = checkSymbol = " "
                Some (indent.Length, symbolType, isChecked, content)
            else
                None
        match row with
        | OL (len, content) -> Ok <| Ordered (len, None, DList.singleton(ParseLine content))
        | CUOL (len, symbolType, isChecked, content) -> Ok <| Unordered (len, Some symbolType, CheckListItem <| DList.singleton((ParseLine content, isChecked)))
        | UOL (len, symbolType, content) -> Ok <| Unordered (len, Some symbolType, NormalListItem <| DList.singleton(ParseLine content))
        | _ -> Error ParseFailure
        |> Result.map MDList
    and private ParseCode row = // Code/CodeBlock
        let (|CB|_|) src =
            let m = RegexPattern.CODEBLOCK.Match src
            if m.Success then
                Some m.Groups[0].Value
            else
                None
        let (|FCB|_|) src = // 2nd pass merge start-end, if no end including all later text
            let m = RegexPattern.FENCED_CODEBLOCK.Match src
            if m.Success then
                Some m.Groups[0].Value
            else
                None
        let (|C|_|) src =
            let m = RegexPattern.CODE.Match src
            if m.Success then
                let prefix::code::suffix::_ = [ for g in m.Groups -> g.Value]
                Some (prefix, code, suffix)
            else
                None
        match row with
        | FCB s ->
                match s with
                | "" -> Ok <| MDCodeBlock {lang=None;content=""}
                | s -> Ok <| MDCodeBlock {lang=Some s;content=""}
        | CB s -> Ok <| MDCodeBlock {lang=None;content=s}
        | C (prefix, code, suffix) ->
            DList.singleton(ParseLine prefix).Conj(MDCode {content=code}).Conj(ParseLine suffix)
            |> MDRow
            |> Ok
        | _ -> Error ParseFailure
    and private ParseHRule row =
        let m = RegexPattern.HRULE.Match row
        if m.Success then
            Ok MDHRules
        else
            Error ParseFailure
    // and private ParseLink row =

    and private ParseFallback row = // TODO
        MDText row |> Ok
    and private ParseLine row =
        [|
            ParseHeading;
            ParseLineBreak;
            ParseEmphasis;
            ParseBlockQuote;
            ParseList;
            ParseCode;
            ParseHRule;

            ParseFallback;
        |]
        |> Seq.fold
            (fun acc fn -> acc <|> fn row)
            (Error ParseFailure)
        |> get

    // paragraph should be parsed and handled in second pass
    

    // Public
    let Parse rows = ()
