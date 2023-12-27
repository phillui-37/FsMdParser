namespace FsMdParser

type Tag =
    | Logger

module ConstStr =
    let ORDERED_LIST_CHARS   = "[\divxlcdmIVXLCDM]+"
    let UNORDERED_LIST_CHARS = "[-*+]"

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

module RegexPattern =
    open System.Text.RegularExpressions
    let LINE_BREAK             = Regex "^(.*?) {2,}$"
    let BLOCK_QUOTE            = Regex "^\>*? (.*)$"
    let ORDERED_LIST           = Regex $"^( *?)({ConstStr.ORDERED_LIST_CHARS})\. (.*)$"
    let UNORDERED_LIST         = Regex $"^( *?)({ConstStr.UNORDERED_LIST_CHARS}) (.*)$"
    let CHECKED_UNORDERED_LIST = Regex $"^( *?)({ConstStr.UNORDERED_LIST_CHARS}) \[([ x]{1})\] (.*)$"
    let CODEBLOCK              = Regex "^(?:\t| {4,})(.*?)"
    let FENCED_CODEBLOCK       = Regex "^```(\w*?)(?!```)$"
    let CODE                   = Regex "(.*?)(```.*?```|`.*?`)(.*?)"
    let HRULE                  = Regex "^(?:\*{3,}|-{3,}|_{3,})$"
    let HEADING                = Regex "^(#{1,6}) (.*)$"
    let MULTI_HEADING_1        = Regex "^={1,}$"
    let MULTI_HEADING_2        = Regex "^-{1,}$"
    let LINK                   = Regex "\[(\w+\)]\(.+\)"