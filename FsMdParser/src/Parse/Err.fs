namespace FsMdParser.Parse

type StopPredicate = MDToken -> bool
type CxLineData = string * MDToken * StopPredicate

type ParseError =
    | ParseFailure
    | NoMoreLines
    | NeedPrevLine of CxLineData
    | NeedNextLine of CxLineData

