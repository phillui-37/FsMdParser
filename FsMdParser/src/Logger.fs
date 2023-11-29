namespace FsMdParser.Logger
open FSharpx.Collections
open System.IO

/// This Logger is used in IO env only
/// To keep functions pure, seperate the execution environment yourself
/// No monad will be provided, write your own state monad or writer monad instead of this if requrie 
/// The logging will be implemented as PubSub in a seperate process

/// TODO formatter?

// enum
type LogLevel =
    | Fatal = 0
    | Error = 1
    | Warn = 2
    | Info = 3
    | Debug = 4

type LogOutput =
    | File = 0x1
    | Terminal = 0x2
    | Stream = 0x4
    | Other = 0x10

// Input
type LogConfig =
    { logLevelMask: LogLevel
      criticalLogLevelList: LogLevel DList
      outputOptions: LogOutput DList
      outputWriters: Option<PersistentHashMap<LogOutput, DList<LogLevel * StringWriter>>> }
    static member Of(
        logLevelMask: LogLevel,
        criticalLogLevelList: LogLevel list,
        outputOptions: LogOutput list,
        outputWriters: Map<LogOutput, List<LogLevel * StringWriter>> option
    ): LogConfig =
        { logLevelMask=logLevelMask;
          criticalLogLevelList=Seq.ofList criticalLogLevelList |> DList.ofSeq;
          outputOptions=Seq.ofList outputOptions |> DList.ofSeq;
          outputWriters=outputWriters
                       |> Option.map (fun o ->
                            Map.toSeq o 
                            |> Seq.map (fun o' -> (fst o', snd o' |> List.toSeq |> DList.ofSeq))
                            |> PersistentHashMap.ofSeq) }
    static member Default =
        { logLevelMask=LogLevel.Info;
          criticalLogLevelList=DList.empty.Add(LogLevel.Fatal).Add(LogLevel.Error);
          outputOptions=DList.empty.Add(LogOutput.Terminal);
          outputWriters= } // TODO terminal stderr and stdout

// Output
type ILogger = 
    { Fatal: string -> unit
      Error: string -> unit
      Warn: string -> unit
      Info: string -> unit
      Debug: string -> unit }

type LoggerState =
    struct
        // if log function's level higher than mask, log will be ignored
        val logLevelMask: LogLevel
        // this property will ignore logLevelMask, i.e. when specific log level is included here, they will always work
        // default value: [Fatal; Error]
        val criticalLogLevelList: LogLevel DList
        // flag
        val output: int

        new(conf: LogConfig) = {
            logLevelMask=conf.logLevelMask;
            criticalLogLevelList=conf.criticalLogLevelList;
            output=conf.outputOptions |> DList.fold (fun acc o -> acc ||| int o) 0
        }
    end

module Logger =
    // logging core
    
    // pub log to message pipeline of different output
    let private DispatchLogToHandler config log = ()

    let private FileLogHandler config log = ()
    let private TerminalLogHandler config log = ()


    // clousre functions of ILogger
    let private Fatal msg = ()

    // TODO Reader
    let GetLogger config  =
        let state = new LoggerState(config)
        ()