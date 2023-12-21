namespace FsMdParser.Parse

type ParseError =
    | NeedNextLine of MDRow option
    | NeedPreviousLine of MDRow option
    | ParseFailure
    | NoMoreLines