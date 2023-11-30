namespace FsMdParser.Logger
open FSharpx.Collections
open System.IO

/// This Logger is used in IO env only
/// To keep functions pure, seperate the execution environment yourself
/// No monad will be provided, write your own state monad or writer monad instead of this if requrie 
/// The logging will be implemented by using MailboxProcessor


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

// default TextWriter implementation
module private DefaultWriters =
    open System.Text
    open System
    open System.Threading.Tasks
    
    let private encoding = lazy ( new UnicodeEncoding(false, false) )

    type StdOutWriter(formatProvider) =
        inherit TextWriter(formatProvider)

        override _.get_Encoding() = encoding.Value
        override this.Close() = this.Dispose(true)
        override _.Dispose(disposing) = base.Dispose(disposing)
        override _.Write(c: char) = Console.Write c
        override _.Write(s: string) = Console.Write s
        override _.WriteLine(c: char) = Console.WriteLine c
        override _.WriteLine(s: string) = Console.WriteLine s
        override this.WriteAsync(c: char) =
            this.Write(c)
            Task.CompletedTask
        override this.WriteAsync(s: string) =
            this.Write(s)
            Task.CompletedTask
        override this.WriteLineAsync(c: char) =
            this.WriteLine(c)
            Task.CompletedTask
        override this.WriteLineAsync(s: string) =
            this.WriteLine(s)
            Task.CompletedTask
        override _.FlushAsync() = Task.CompletedTask
        override _.ToString() = "StdOutWriter"

    type StdErrWriter(formatProvider) =
        inherit TextWriter(formatProvider)

        let _out = Console.Error

        override _.get_Encoding() = encoding.Value
        override this.Close() = this.Dispose(true)
        override _.Dispose(disposing) = base.Dispose(disposing)
        override _.Write(c: char) = _out.Write c
        override _.Write(s: string) = _out.Write s
        override _.WriteLine(c: char) = _out.WriteLine c
        override _.WriteLine(s: string) = _out.WriteLine s
        override this.WriteAsync(c: char) =
            this.Write(c)
            Task.CompletedTask
        override this.WriteAsync(s: string) =
            this.Write(s)
            Task.CompletedTask
        override this.WriteLineAsync(c: char) =
            this.WriteLine(c)
            Task.CompletedTask
        override this.WriteLineAsync(s: string) =
            this.WriteLine(s)
            Task.CompletedTask
        override _.FlushAsync() = Task.CompletedTask
        override _.ToString() = "StdErrWriter"

    let GetStdOutWriter(): TextWriter = new StdOutWriter(null)

    let GetStdErrWriter(): TextWriter = new StdErrWriter(null)

    ()

// Input
type LogConfig =
    { logLevelMask: LogLevel
      criticalLogLevelList: LogLevel DList
      outputOptions: LogOutput DList
      outputWriters: Option<PersistentHashMap<LogOutput, DList<LogLevel * TextWriter>>> }
    static member Of(
        logLevelMask: LogLevel,
        criticalLogLevelList: LogLevel list,
        outputOptions: LogOutput list,
        outputWriters: Map<LogOutput, List<LogLevel * TextWriter>> option
    ): LogConfig =
        { logLevelMask=logLevelMask;
          criticalLogLevelList=Seq.ofList criticalLogLevelList |> DList.ofSeq;
          outputOptions=Seq.ofList outputOptions |> DList.ofSeq;
          outputWriters=outputWriters
                       |> Option.map (fun o ->
                            Map.toSeq o 
                            |> Seq.map (fun o' -> (fst o', snd o' |> DList.ofSeq))
                            |> PersistentHashMap.ofSeq) }
    static member Default =
        let _writers =
            [|(LogOutput.Terminal, [|
                (LogLevel.Debug, DefaultWriters.GetStdOutWriter());
                (LogLevel.Info, DefaultWriters.GetStdOutWriter());
                (LogLevel.Warn, DefaultWriters.GetStdOutWriter());
                (LogLevel.Error, DefaultWriters.GetStdErrWriter());
                (LogLevel.Fatal, DefaultWriters.GetStdErrWriter());|] |> DList.ofSeq )|]
        { logLevelMask=LogLevel.Info;
          criticalLogLevelList=DList.empty.Cons(LogLevel.Fatal).Cons(LogLevel.Error);
          outputOptions=DList.empty.Cons(LogOutput.Terminal);
          outputWriters=PersistentHashMap.ofSeq _writers |> Some}

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