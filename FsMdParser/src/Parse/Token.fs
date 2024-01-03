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
and Emphasis<'a> =
    | Bold   of 'a
    | Italic of 'a
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
      rows: MDTableToken DList } // 1st pass will all in rows first
and MDTableToken =
    | MDTableText     of string
    | MDTableLink     of Link
    | MDTableCode     of Code
    | MDTableEmphasis of MDTableToken Emphasis
    | MDTableRow      of MDTableToken DList
and FootNote    = { tag: string; }
and FootNoteRef = { tag: string; text: string}
and DefinitionList = { terms: DefinitionTerm DList; }
and DefinitionTerm = { definitions: MDToken DList; }
and MDToken =
    | MDRow            of MDToken DList
    | MDText           of string
    | MDHeading        of Heading
    | MDParagraph      of MDToken DList
    | MDLineBreak                    // <br />
    | MDEmphasis       of MDToken Emphasis
    | MDBlockQuotes    of BlockQuote // multiline block quote will merge as one
    | MDList           of List'
    | MDCode           of Code
    | MDCodeBlock      of CodeBlock
    | MDHRules                       // <hr />
    | MDLink           of Link       // http/https/mailto should be automatically detected and converted to Link
    | MDImage          of Image * string option
    | MDTable          of Table
    | MDTableSeq       of DList<TableAlignment option>
    | MDFootNote       of FootNote
    | MDFootNoteRef    of FootNoteRef
    | MDDefinitionList of DefinitionList
    | MDStrikethrough  of MDToken
    | MDHighLight      of MDToken
    | MDSubscript      of MDToken
    | MDSuperscript    of MDToken