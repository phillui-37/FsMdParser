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
            [for g in m.Groups -> g.Value].Tail |> mapper |> Some
        else
            None

    let private mdRowTokenAssemblerMeta prefixMapper suffixMapper rowWrapper prefix suffix token =
        DList.singleton (prefixMapper prefix)
        |> DList.conj token
        |> DList.conj (suffixMapper suffix)
        |> rowWrapper
        |> Ok


    let rec private mdRowTokenAssembler = mdRowTokenAssemblerMeta ParseLine ParseLine MDRow
    and private ParseHeading row =
        let (|H|_|) = (fun ls ->
            let level::content::_ = ls
            let layer =
                match level with
                | "#" -> H1
                | "##" -> H2
                | "###" -> H3
                | "####" -> H4
                | "#####" -> H5
                | "######" -> H6
            (layer, content)) |> regexAP RegexPattern.HEADING
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
        | Br (s :: _) -> Ok << MDRow <| DList.singleton(MDLineBreak).Cons(ParseLine s)
        | _ -> Error ParseFailure
    and private ParseEmphasisMeta linePraser emphasisWrapper rowWrapper row =
        let rec tryParse src ret =
            match src with
            | [] -> []
            | value :: remain ->
                let (pattern, mapper) = value
                let newList =
                    match row with
                    | Surround pattern pattern s -> (emphasisWrapper << mapper <| linePraser s) :: ret
                    | _ -> ret
                tryParse remain newList
        let result = tryParse [("**", Bold); ("__", Bold); ("*", Italic); ("_", Italic)] []
        match result with
        | [] -> Error ParseFailure
        | r -> Ok << rowWrapper <| DList.ofSeq r
    and private ParseEmphasis = ParseEmphasisMeta ParseLine MDEmphasis MDRow
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
        let (|OL|_|) = (fun (ls: string list) ->
            let indent::_::content::_ = ls
            (indent.Length, content)) |> regexAP RegexPattern.ORDERED_LIST
        let getSymbolType = function
                | "-" -> Dash
                | "*" -> Asterisk
                | "+" -> Plus
                | _   -> Dash
        let (|UOL|_|) = (fun ls ->
            let indent::symbol::content::_ = ls
            let symbolType = getSymbolType symbol
            (indent.Length, symbolType, content)) |> regexAP RegexPattern.UNORDERED_LIST
        let (|CUOL|_|) =  (fun ls ->
            let indent::symbol::checkSymbol::content::_ = ls
            let symbolType = getSymbolType symbol
            let isChecked = checkSymbol = " "
            (indent.Length, symbolType, isChecked, content)) |> regexAP RegexPattern.CHECKED_UNORDERED_LIST
        match row with
        | OL (len, content) -> Ok <| Ordered (len, None, DList.singleton(ParseLine content))
        | CUOL (len, symbolType, isChecked, content) -> Ok <| Unordered (len, Some symbolType, CheckListItem <| DList.singleton((ParseLine content, isChecked)))
        | UOL (len, symbolType, content) -> Ok <| Unordered (len, Some symbolType, NormalListItem <| DList.singleton(ParseLine content))
        | _ -> Error ParseFailure
        |> Result.map MDList
    and private ParseCode row = // Code/CodeBlock
        let (|CB|_|) = regexAP RegexPattern.CODEBLOCK (List.Pos 0)
        // 2nd pass merge start-end, if no end including all later text
        let (|FCB|_|) = regexAP RegexPattern.FENCED_CODEBLOCK (List.Pos 0) 
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
            MDCode {content=code}
            |> mdRowTokenAssembler prefix suffix
        | _ -> Error ParseFailure
    and private ParseHRule row =
        let m = RegexPattern.HRULE.Match row
        if m.Success then
            Ok MDHRules
        else
            Error ParseFailure
    and private ParseLinkMeta lineParser linkWrapper rowWrapper row  =
        let (|Image|_|) = regexAP RegexPattern.IMAGE_LINK (fun ls ->
            let prefix::alt::imageUrl::linkUrl::suffix::_ = ls
            (alt, imageUrl, linkUrl, prefix, suffix))
        let (|TextAlt|_|) = (fun ls ->
            let prefix::text::url::alt::suffix::_ = ls
            (text, url, alt, prefix, suffix)) |> regexAP RegexPattern.LINK_WITH_ALT
        let (|Text|_|) = regexAP RegexPattern.LINK (fun ls ->
            let prefix::text::url::suffix::_ = ls
            (text, url, prefix, suffix))
        let (|SimpleHttp|_|) = regexAP RegexPattern.HTTP_URL (fun ls ->
            let prefix::url::suffix::_ = ls
            let url' =
                match url with
                | (Prefix "<" _ & Suffix ">" s) -> s.Substring(1)
                | s -> s
            (url', prefix, suffix))
        let (|SimpleEmail|_|) = regexAP RegexPattern.EMAIL_URL (fun ls ->
            let prefix::email::suffix::_ = ls
            let email' =
                match email with
                | (Prefix "<" _ & Suffix ">" s) -> s.Substring(1)
                | s -> s
            (email', prefix, suffix))
        let (|RefLink|_|) = regexAP RegexPattern.REF_LINK (fun ls ->
            let prefix::text::tag::suffix::_ = ls
            (text, tag, prefix, suffix))
        let (|RefLinkResolve|_|) src =
            let m = RegexPattern.REF_LINK_RESOLVE.Match src
            if m.Success then
                let _::tag::url::maybeTitle::_ = [for g in m.Groups -> g.Value]
                match url with
                | Prefix "<" _ & Suffix ">" s -> s.Substring(1) |> Some
                | Prefix "<" _ -> None
                | Suffix ">" _ -> None
                | s -> Some s
                |> Option.map (fun url' ->
                    (tag, url',
                    match maybeTitle with
                    | "" -> None
                    | s -> Some s))
            else
                None
        let mdRowMapper prefix suffix token =
            DList.singleton(lineParser prefix)
            |> DList.conj (linkWrapper token)
            |> DList.conj (lineParser suffix)
            |> rowWrapper
            |> Ok
        match row with
        | Image (alt, imageUrl, linkUrl, prefix, suffix) ->
            ImageLink ({alt=alt;url=imageUrl}, linkUrl)
            |> mdRowMapper prefix suffix
        | TextAlt (text, url, alt, prefix, suffix) ->
            TextLink (text, url, Some alt)
            |> mdRowMapper prefix suffix
        | Text (text, url, prefix, suffix) ->
            TextLink (text, url, None)
            |> mdRowMapper prefix suffix
        | SimpleHttp (url, prefix, suffix) ->
            SimpleLink url
            |> mdRowMapper prefix suffix
        | SimpleEmail (email, prefix, suffix) ->
            SimpleLink $"mailto:{email}"
            |> mdRowMapper prefix suffix
        | RefLink (text, tag, prefix, suffix) ->
            RefLink (text, tag) |> mdRowMapper prefix suffix
        | RefLinkResolve (tag, url, maybeTitle) ->
            RefLinkResolve {tag=tag;url=url;alt=maybeTitle}
            |> linkWrapper
            |> Ok
        | _ -> Error ParseFailure
    and private ParseLink = ParseLinkMeta ParseLine MDLink MDRow
    and private ParseImage row =
        let (|I|_|) = regexAP RegexPattern.IMAGE (fun ls ->
            (ls[0], ls[1], ls[3]))
        match row with
        | I (tag, url, maybeTitle) ->
            let title =
                match maybeTitle with
                | "" -> None
                | s -> Some s
            MDImage ({alt=tag;url=url;}, title) |> Ok
        | _ -> Error ParseFailure
    and private ParseTable row =
        let sepM = RegexPattern.TABLE_SEP.Match row
        let tM = RegexPattern.TABLE.Match row

        let rec parseTableContent s =
            parseTableLink s
            <|> parseTableCode s
            <|> parseTableEmphasis s
            <|> Ok(MDTableText s)
            |> get
        and parseTableLink = ParseLinkMeta parseTableContent MDTableLink MDTableRow
        and parseTableCode s =
            let (|C|_|) = regexAP RegexPattern.CODE (fun ls ->
                let prefix::code::suffix::_ = ls
                (prefix, code, suffix))
            match s with
            | C (prefix, code, suffix) ->
                DList.singleton(parseTableContent prefix)
                |> DList.conj(MDTableCode {content=code})
                |> DList.conj(parseTableContent suffix)
                |> MDTableRow
                |> Ok
            | _ -> Error ParseFailure
        and parseTableEmphasis = ParseEmphasisMeta parseTableContent MDTableEmphasis MDTableRow
            
        if sepM.Success then
            let cols = seq {
                for g in sepM.Groups ->
                    match String.RemovePrefixSpace(g.Value.Replace(" ", "")) with
                    | Prefix ":" _ & Suffix ":|" _ -> Some Middle
                    | Prefix ":" _ -> Some Left
                    | Suffix ":|" _ -> Some Right
                    | _ -> None
            }
            cols |> DList.ofSeq |> MDTableSeq |> Ok
        else if tM.Success then
            let cols = [
                for g in tM.Groups ->
                    g.Value.Replace("|", "")
                    |> String.RemovePrefixSpace
                    |> String.RemoveSuffixSpace
            ]
            let content =
                cols.Tail
                |> Seq.map parseTableContent
                |> DList.ofSeq
            MDTable {columns=DList.empty;rows=content} |> Ok
        else
            Error ParseFailure
    and private ParseFootNote row =
        match regexAP RegexPattern.FOOTNOTE Funs.id row with
        | Some (s::_) -> MDFootNote {tag=s} |> Ok
        | _ -> Error ParseFailure
    and private ParseFootNoteRef row = // maybe multiline, 2nd pass check
        let (|FNR|_|) = regexAP RegexPattern.FOOTNOTE_REF (fun ls ->
            let tag::content::_ = ls
            (tag, content))
        match row with
        | FNR (tag, content) -> MDFootNoteRef {tag=tag;text=content} |> Ok
        | _ -> Error ParseFailure
    and private ParseDefinitionList row = // 2nd pass to merge together
        match row with
        | Prefix ": " s -> MDDefinitionList {terms=DList.singleton {definitions=ParseLine s |> DList.singleton}} |> Ok
        | _ -> Error ParseFailure
    and private ParseStrikethrough row =
        let (|ST|_|) = regexAP RegexPattern.STRIKETHROUGH (fun ls ->
            let prefix::s::suffix::_ = ls
            (prefix, s, suffix))
        match row with
        | ST (prefix, s, suffix) ->
            ParseLine s
            |> MDStrikethrough
            |> mdRowTokenAssembler prefix suffix
        | _ -> Error ParseFailure
    and private ParseHighlight row =
        let (|HL|_|) = regexAP RegexPattern.HIGHLIGHT (fun ls ->
            let prefix::s::suffix::_ = ls
            (prefix, s, suffix))
        match row with
        | HL (prefix, s, suffix) ->
            ParseLine s
            |> MDHighLight
            |> mdRowTokenAssembler prefix suffix
        | _ -> Error ParseFailure
    and private ParseSubscript row =
        let (|SUB|_|) = regexAP RegexPattern.SUBSCRIPT (fun ls ->
            let prefix::s::suffix::_ = ls
            (prefix, s, suffix))
        match row with
        | SUB (prefix, s, suffix) ->
            ParseLine s
            |> MDSubscript
            |> mdRowTokenAssembler prefix suffix
        | _ -> Error ParseFailure
    and private ParseSuperscript row =
        let (|SUPER|_|) = regexAP RegexPattern.SUPERSCRIPT (fun ls ->
            let prefix::s::suffix::_ = ls
            (prefix, s, suffix))
        match row with
        | SUPER (prefix, s, suffix) ->
            ParseLine s
            |> MDSuperscript
            |> mdRowTokenAssembler prefix suffix
        | _ -> Error ParseFailure
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
            ParseLink;
            ParseImage;
            ParseTable;
            ParseFootNoteRef;
            ParseFootNote;
            ParseDefinitionList;
            ParseStrikethrough;
            ParseSubscript;
            ParseSuperscript;

            ParseFallback;
        |]
        |> Seq.fold
            (fun acc fn -> acc <|> fn row)
            (Error ParseFailure)
        |> get

    // TODO 2nd pass: paragraph, block quotes, list, code block, table + table seq, footnote ref, def list
    [<RequireQualifiedAccess>]
    module private StopPredicate =
        let Paragraph = function // backward
            | MDText ""
            | MDHeading _
            | MDBlockQuotes _
            | MDList _
            | MDCodeBlock _
            | MDHRules
            | MDTable _
            | MDTableSeq _
            | MDParagraph _
            | MDFootNoteRef _
            | MDDefinitionList _ -> true
            | _ -> false
        let BlockQuote = ()
        let List = ()
        let CodeBlock = ()
        let Table = ()
        let FootNoteRef = ()
        let DefList = ()

    let private ParseSndPass (rows: DList<string * MDToken>) =
        // as 2nd pass is run in series, impure array can be used to enhance the performance
        // this will be run repeatedly until fixed point cond meet
        let rows' = Array.ofSeq rows

        // predicate

        // let mapper row idx =
        //     | ("", _) -> 
        // for idx in 0..rows'.Length do
        ()
            

    // Public
    let Parse rows = ()
