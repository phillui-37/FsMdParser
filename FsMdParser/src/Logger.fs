namespace FsMdParser.Logger

type LogLevel =
    | Error
    | Warn
    | Info
    | Debug

module Logger =
    let getLogger = ()