namespace FsMdParser.Parse

type ParseError =
    | NeedNextLine of MDRow
    | ParseFailure