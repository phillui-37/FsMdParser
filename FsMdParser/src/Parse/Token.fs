namespace FsMdParser.Parse

type HeadingLevel =
    | H1
    | H2
    | H3
    | H4
    | H5
    | H6
type ListLayer = int
type ListSymbolType =
    | Number // ol->1
    | CapitalRomanNumber // ol->I
    | SmallRomanNumber // ol->i
    | CapitalLetter // ol->A
    | SmallLetter // ol->a
    | Bullet // ul->disc
    | Circle // ul->circle
    | Square // ul->square
type CodeBlockLang = string
type ImageAlt = string
type ImageURL = string

type Heading =
    | Heading of HeadingLevel * MDToken
    | MultiLineHeading of HeadingLevel // for =/- line, need to include previous row for handling, 2nd pass will convert to Heading
and Emphasis =
    | Bold of MDToken
    | Italic of MDToken
and List' =
    | Ordered of ListLayer * Option<ListSymbolType> * MDToken // multiline will merge as one, a-z/A-Z/0-9
    | Unordered of ListLayer * Option<ListSymbolType> * MDToken // must be same char
and CodeBlock = CodeBlockLang * string
and Image = ImageAlt * ImageURL
and MDToken =
    | MDText of string
    | MDHeading of Heading
    | MDParagraph of MDToken
    | MDLineBreak
    | MDEmphasis of Emphasis
    | MDBlockQuotes of MDToken // multiline block quote will merge as one
    | MDList of List'
    | MDCodeBlock of CodeBlock