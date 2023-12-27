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

    let private regexAP (pattern: Regex) mapper src =
        let m = pattern.Match src
        if m.Success then
            mapper [for g in m.Groups -> g.Value] |> Some
        else
            None
    let rec private ParseHeading row =
        let (|H|_|) = regexAP RegexPattern.HEADING (fun ls ->
            let level::content::_ = ls
            let layer =
                match level with
                | "#" -> H1
                | "##" -> H2
                | "###" -> H3
                | "####" -> H4
                | "#####" -> H5
                | "######" -> H6
            (layer, content))
        let (|MH|_|) src =
            let m1 = RegexPattern.MULTI_HEADING_1.Match src
            let m2 = RegexPattern.MULTI_HEADING_2.Match src
            if m1.Success then
                Some H1
            else if m2.Success then
                Some H2
            else
                None
        match row with
        | H (level, content) -> Heading (level, ParseLine content, None) |> Ok
        | MH level -> MultiLineHeading (level, None) |> Ok
        | _ -> ParseFailure |> Error
        |> Result.map MDHeading
    and private ParseLineBreak row =
        let (|Br|_|) = regexAP RegexPattern.LINE_BREAK Funs.id
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
        let (|BQ|_|) = regexAP RegexPattern.BLOCK_QUOTE Funs.id
        match row with
        | BQ (s :: _) ->
                let layer = getLayer row 0
                Ok <| MDBlockQuotes {layer=layer;content=ParseLine s}
        | _ -> Error ParseFailure
    and private ParseList row = // 2nd pass merge list together
        let (|OL|_|) = regexAP RegexPattern.ORDERED_LIST (fun ls ->
            let indent::_::content::_ = ls
            (indent.Length, content))
        let getSymbolType = function
                | "-" -> Dash
                | "*" -> Asterisk
                | "+" -> Plus
                | _   -> Dash
        let (|UOL|_|) = regexAP RegexPattern.UNORDERED_LIST (fun ls ->
            let indent::symbol::content::_ = ls
            let symbolType = getSymbolType symbol
            (indent.Length, symbolType, content))
        let (|CUOL|_|) = regexAP RegexPattern.CHECKED_UNORDERED_LIST (fun ls ->
            let indent::symbol::checkSymbol::content::_ = ls
            let symbolType = getSymbolType symbol
            let isChecked = checkSymbol = " "
            (indent.Length, symbolType, isChecked, content))
        match row with
        | OL (len, content) -> Ok <| Ordered (len, None, DList.singleton(ParseLine content))
        | CUOL (len, symbolType, isChecked, content) -> Ok <| Unordered (len, Some symbolType, CheckListItem <| DList.singleton((ParseLine content, isChecked)))
        | UOL (len, symbolType, content) -> Ok <| Unordered (len, Some symbolType, NormalListItem <| DList.singleton(ParseLine content))
        | _ -> Error ParseFailure
        |> Result.map MDList
    and private ParseCode row = // Code/CodeBlock
        let (|CB|_|) = regexAP RegexPattern.CODEBLOCK (fun ls -> ls[0])
        // 2nd pass merge start-end, if no end including all later text
        let (|FCB|_|) = regexAP RegexPattern.FENCED_CODEBLOCK (fun ls -> ls[0]) 
        let (|C|_|) = regexAP RegexPattern.CODE (fun ls ->
            let prefix::code::suffix::_ = ls
            (prefix, code, suffix))
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
    and private ParseLink row =
        let (|Simple|_|) = regexAP RegexPattern.LINK (fun ls ->
            let text::url::_ = ls
            (text, url))
        match row with
        | Simple (text, url) ->
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
