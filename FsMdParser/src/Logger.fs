namespace FsMdParser.Logger

open FSharpPlus
open FSharpx.Collections
open System.IO
open FsMdParser.SimpleTools

/// This Logger is used in IO env only
/// To keep functions pure, seperate the execution environment yourself
/// No monad will be provided, write your own state monad or writer monad instead of this if requrie
/// The logging will be implemented by using MailboxProcessor


// enum
type LogLevel =
    | Fatal = 0
    | Error = 1
    | Warn  = 2
    | Info  = 3
    | Debug = 4

module LogLevel =
    let ToString = function
        | LogLevel.Fatal -> "FATAL"
        | LogLevel.Error -> "ERROR"
        | LogLevel.Warn  -> "WARN"
        | LogLevel.Info  -> "INFO"
        | LogLevel.Debug -> "DEBUG"
        | _              -> raise <| new InvalidDataException ""

type LogOutput =
    | File     = 0x1
    | Terminal = 0x2
    | Stream   = 0x4
    | Other    = 0x10

// type alias
type LogWriterMapEntry     = LogLevel * TextWriter
type RawLogWriterMap       = LogWriterMapEntry list
type RawLogOutputWriterMap = Map<LogOutput, RawLogWriterMap>
type LogWriterMap          = LogWriterMapEntry DList
type LogOutputWriterMap    = PersistentHashMap<LogOutput, LogWriterMap>

// default TextWriter implementation
module private DefaultWriters =
    open System.Text
    open System
    open System.Threading.Tasks

    let private encoding = lazy (new UnicodeEncoding(false, false))

    type StdOutWriter() =
        inherit TextWriter(null)

        static let _instance = Lazy<StdOutWriter>.Create(fun () -> new StdOutWriter())
        static member Of() = _instance.Value

        override _.get_Encoding() = encoding.Value
        override this.Close() = this.Dispose(true)
        override _.Dispose(disposing) = base.Dispose(disposing)
        override _.Write(c: char) =
            Console.Write c
            Console.Out.Flush ()
        override _.Write(s: string) =
            Console.Write s
            Console.Out.Flush ()
        override _.WriteLine(c: char) =
            Console.WriteLine c
            Console.Out.Flush ()
        override _.WriteLine(s: string) =
            Console.WriteLine s
            Console.Out.Flush ()

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

    type StdErrWriter() =
        inherit TextWriter(null)

        let _out = Console.Error

        static let _instance = Lazy<StdErrWriter>.Create(fun () -> new StdErrWriter())
        static member Of() = _instance.Value


        override _.get_Encoding() = encoding.Value
        override this.Close() = this.Dispose(true)
        override _.Dispose(disposing) = base.Dispose(disposing)
        override _.Write(c: char) =
            _out.Write c
            _out.Flush ()
        override _.Write(s: string) =
            _out.Write s
            _out.Flush ()
        override _.WriteLine(c: char) =
            _out.Flush ()
            _out.WriteLine c
        override _.WriteLine(s: string) =
            _out.WriteLine s
            _out.Flush ()

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

    let GetStdOutWriter () : TextWriter = StdOutWriter.Of()

    let GetStdErrWriter () : TextWriter = StdErrWriter.Of()

    ()

// Input
type LogConfig =
    { logLevelMask: LogLevel
      criticalLogLevelList: LogLevel DList
      outputOptions: LogOutput DList
      outputWriters: LogOutputWriterMap }

    static member private GetDefaultWriters =
        [| (LogOutput.Terminal,
            [| (LogLevel.Debug, DefaultWriters.GetStdOutWriter())
               (LogLevel.Info, DefaultWriters.GetStdOutWriter())
               (LogLevel.Warn, DefaultWriters.GetStdOutWriter())
               (LogLevel.Error, DefaultWriters.GetStdErrWriter())
               (LogLevel.Fatal, DefaultWriters.GetStdErrWriter()) |]
            |> DList.ofSeq) |]

    static member Of
        (
            logLevelMask: LogLevel,
            criticalLogLevelList: LogLevel list,
            outputOptions: LogOutput list,
            outputWriters: RawLogOutputWriterMap option
        ) : LogConfig =
        let _writers = LogConfig.GetDefaultWriters

        { logLevelMask = logLevelMask
          criticalLogLevelList = Seq.ofList criticalLogLevelList |> DList.ofSeq
          outputOptions = Seq.ofList outputOptions |> DList.ofSeq
          outputWriters =
            match outputWriters with
            | Some o ->
                Map.toSeq o
                |> Seq.map (Funs.bimap Funs.id DList.ofSeq)
                |> PersistentHashMap.ofSeq
            | None -> PersistentHashMap.ofSeq _writers }

    static member Default =
        let _writers = LogConfig.GetDefaultWriters

        { logLevelMask = LogLevel.Info
          criticalLogLevelList = DList.singleton(LogLevel.Fatal).Cons(LogLevel.Error)
          outputOptions = DList.singleton (LogOutput.Terminal)
          outputWriters = PersistentHashMap.ofSeq _writers }

// Output
type ILogger =
    { Fatal:  string -> unit;
      Error:  string -> unit;
      Warn:   string -> unit;
      Info:   string -> unit;
      Debug:  string -> unit;
      IsBusy: unit -> bool }

type LoggerState =
    struct
        // if log function's level higher than mask, log will be ignored
        val logLevelMask: LogLevel
        // this property will ignore logLevelMask, i.e. when specific log level is included here, they will always work
        // default value: [Fatal; Error]
        val criticalLogLevelList: LogLevel DList
        // flag
        val output: int
        // writers
        val writers: LogOutputWriterMap

        new(conf: LogConfig) =
            { logLevelMask = conf.logLevelMask
              criticalLogLevelList = conf.criticalLogLevelList
              output = conf.outputOptions |> DList.fold (fun acc o -> acc ||| int o) 0
              writers = conf.outputWriters }
    end

module Logger =
    open System
    open System.Globalization
    open FsMdParser

    let levelCheck (state: LoggerState) lv =
        lv <= int state.logLevelMask
        || DList.toSeq state.criticalLogLevelList
           |> Seq.map int
           |> Seq.tryFind (Funs.eq lv)
           <> None

    // logging core
    let private getLogConsumer state outputFlag (logWriterMap: LogOutputWriterMap) =
        MailboxProcessor<LogLevel * string>.Start(fun inbox ->
            let rec loop () =
                async {
                    let! (level, msg) = inbox.Receive()
                    let now = DateTime.UtcNow.ToString("yyyy-MM-dd HH:mm:ss.fff", CultureInfo.CurrentCulture)
                    let color =
                        match level with
                        | LogLevel.Fatal -> TtyColor.RED
                        | LogLevel.Error -> TtyColor.MAGENTA
                        | LogLevel.Warn  -> TtyColor.YELLOW
                        | _ -> TtyColor.WHITE
                    
                    PersistentHashMap.toSeq logWriterMap
                    |> Seq.filter (fst >> int >> Funs.hasFlag (int outputFlag))
                    |> Seq.fold (fun _ (_, writerMap) ->
                        DList.toSeq writerMap
                        |> Seq.filter (fst >> Funs.and'([|int >> levelCheck state; Funs.eq level|]))
                        |> Seq.fold (fun _ (_, writer) ->
                            $"{color}[{LogLevel.ToString level}]\t[{now} UTC] {msg}{TtyColor.RESET}"
                            |> writer.WriteLine
                            ())
                            ())
                        ()

                    return! loop ()
                }

            loop ())

    // pub log to message pipeline of different output
    let private LogHandler (state: LoggerState) =
        let _writer =
            state.writers
            |> getLogConsumer state (PersistentHashMap.toSeq state.writers |> Seq.fold (fun acc (output, _) -> acc ||| int output) 0)

        let Logger = fun level log -> _writer.Post((level, log))    
        let IsBusy() = _writer.CurrentQueueLength > 0

        Logger, IsBusy

    // TODO Reader Monad
    let GetLogger config : ILogger =
        let (handler, IsBusy) = new LoggerState(config) |> LogHandler

        { Info   = handler LogLevel.Info
          Debug  = handler LogLevel.Debug
          Warn   = handler LogLevel.Warn
          Error  = handler LogLevel.Error
          Fatal  = handler LogLevel.Fatal
          IsBusy = IsBusy }