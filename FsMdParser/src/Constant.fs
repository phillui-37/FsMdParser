namespace FsMdParser

type Tag =
    | Logger

module TtyColor =
    let RESET   = "\x1b[0m"
    let BLACK   = "\x1b[30m"
    let RED     = "\x1b[31m"
    let GREEN   = "\x1b[32m"
    let YELLOW  = "\x1b[33m"
    let BLUE    = "\x1b[34m"
    let MAGENTA = "\x1b[35m"
    let CYAN    = "\x1b[36m"
    let WHITE   = "\x1b[37m"