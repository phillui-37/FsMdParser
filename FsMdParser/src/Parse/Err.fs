namespace FsMdParser.Parse

type ParseError =
    | NeedNextLine of MDRow
    | NeedPreviousLine of MDRow
    | ParseFailure
    | NoMoreLines