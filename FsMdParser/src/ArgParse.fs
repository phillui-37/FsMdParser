namespace FsMdParser

module ArgParse =
    open Argu

    type OutputFormat =
        | Html

    // TODO plugin, css...
    type Arguments =
        | [<AltCommandLine "-o">] Out_Format of output_format:string
        | [<Mandatory>] In of input_path:string
        | Log_Level of log_level:string
        | Quiet

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Out_Format _ -> "output format"
                | In _         -> "input file"
                | Log_Level _  -> "log level"
                | Quiet        -> "silence all output"