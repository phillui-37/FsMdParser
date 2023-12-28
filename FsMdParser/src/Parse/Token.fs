namespace FsMdParser.Parse
open FSharpx.Collections

type HeadingLevel =
    | H1
    | H2
    | H3
    | H4
    | H5
    | H6
type ListSymbolType =
    | Number             // ol->1
    | CapitalRomanNumber // ol->I
    | SmallRomanNumber   // ol->i
    | CapitalLetter      // ol->A
    | SmallLetter        // ol->a
    | Dash
    | Asterisk
    | Plus
type TableAlignment = | Left | Middle | Right

type ListIndent = int
type Text      = string
type Alt       = string
type URL       = string
type Tag       = string
type HeadingId = string // for anchor link

type Heading =
    | Heading          of HeadingLevel * MDToken * HeadingId option
    | MultiLineHeading of HeadingLevel * HeadingId option // for =/- line, need to include previous row for handling, 2nd pass will convert to Heading
and Emphasis =
    | Bold   of MDToken
    | Italic of MDToken
and BlockQuote = {layer: int; content: MDToken}
and List' =
    | Ordered   of ListIndent * Option<ListSymbolType> * MDToken DList     // multiline will merge as one, a-z/A-Z/0-9
    | Unordered of ListIndent * Option<ListSymbolType> * UnorderedListItem // must be same char
and UnorderedListItem =
    | NormalListItem of MDToken DList
    | CheckListItem  of DList<MDToken * bool> // checked or not
and Link =
    | SimpleLink     of URL                     // syntax <URL>, eg <https://google.com>, <test@gmail.com>
    | TextLink       of Text * URL * Alt option // anchor linking also included
    | ImageLink      of Image * URL             // eg badge, syntax [![image alt](image link)](url)
    | RefLink        of Text * Tag              // tag -> LinkRef, if not valid Ref fallback to normal text
    | RefLinkResolve of LinkRef
and LinkRef   = { tag: Tag; url: URL; alt: Alt option; }
and Code      = { content: string; }
and CodeBlock = { lang: string option; content: string; }
and Image     = { alt: string; url: string; }
and Table =
    { columns: {| header: MDTableToken;
                  alignment: TableAlignment option; |} DList;
      rows: MDTableToken DList }
and TableEmphasis =
    | TBold   of string
    | TItalic of string
and MDTableToken =
    | MDTableLink     of Link
    | MDTableCode     of Code
    | MDTableEmphasis of TableEmphasis
and FootNote    = { tag: string; }
and FootNoteRef = { tag: string; text: string}
and DefinitionList = { terms: DefinitionTerm DList; }
and DefinitionTerm = { definitions: MDToken DList; }
and MDToken =
    | MDRow            of MDToken DList
    | MDText           of string
    | MDHeading        of Heading
    | MDParagraph      of MDToken
    | MDLineBreak                    // <br />
    | MDEmphasis       of Emphasis
    | MDBlockQuotes    of BlockQuote // multiline block quote will merge as one
    | MDList           of List'
    | MDCode           of Code
    | MDCodeBlock      of CodeBlock
    | MDHRules                       // <hr />
    | MDLink           of Link       // http/https/mailto should be automatically detected and converted to Link
    | MDImage          of Image * string option
    | MDComment        of string     // syntax: [comment]: #|<>, ref: https://stackoverflow.com/questions/4823468/comments-in-markdown
    | MDTable          of Table
    | MDFootNote       of FootNote
    | MDDefinitionList of DefinitionList
    | MDStrikethrough  of string
    | MDHighLight      of string
    | MDSubscript      of string
    | MDSuperscript    of string