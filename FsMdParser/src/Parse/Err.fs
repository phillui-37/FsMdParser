namespace FsMdParser.Parse

type ParseError =
    | NeedNextLine of MDToken option
    | NeedPreviousLine of MDToken option
    | ParseFailure
    | NoMoreLines