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

    type StdOutWriter(formatProvider) =
        inherit TextWriter(formatProvider)

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

    type StdErrWriter(formatProvider) =
        inherit TextWriter(formatProvider)

        let _out = Console.Error

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

    let GetStdOutWriter () : TextWriter = new StdOutWriter(null)

    let GetStdErrWriter () : TextWriter = new StdErrWriter(null)

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
    open System.Threading
    // logging core
    let private _logConsumer tag (logWriter: TextWriter) =
        MailboxProcessor<LogLevel * string>.Start(fun inbox ->
            let rec loop () =
                async {
                    let! msg = inbox.Receive()
                    let now = DateTime.UtcNow.ToString(new CultureInfo("en-US"))
                    let getMsg level log = $"[{level}]\t[{now} UTC] {log}"

                    let log =
                        match fst msg with
                        | LogLevel.Fatal -> snd msg |> getMsg "FATAL"
                        | LogLevel.Error -> snd msg |> getMsg "ERROR"
                        | LogLevel.Warn  -> snd msg |> getMsg "WARN"
                        | LogLevel.Info  -> snd msg |> getMsg "INFO"
                        | LogLevel.Debug -> snd msg |> getMsg "DEBUG"
                        | _              -> $"Unexpected log request received: {msg}"
                    logWriter.WriteLine log
                    // TODO add sleep to avoid queue stuck?
                    return! loop ()
                }

            loop ())

    // pub log to message pipeline of different output
    let private LogHandler (state: LoggerState) =
        let levelCheck lv =
            lv <= int state.logLevelMask
            || DList.toSeq state.criticalLogLevelList
               |> Seq.map int
               |> Seq.tryFind (Funs.eq lv)
               <> None

        let consumers =
            PersistentHashMap.toSeq state.writers
            |> Seq.bind (fun (output, writerMap) ->
                if int output &&& state.output = 0 then
                    DList.empty
                else
                    DList.toSeq writerMap
                    |> Seq.filter (fst >> int >> levelCheck)
                    |> Seq.map (fun (level, writer) -> (level, _logConsumer writer)))
            |> DList.ofSeq

        fun level log ->
            if levelCheck <| int level then
                DList.toSeq consumers
                |> Seq.fold
                    (fun _ (_level, writer) ->
                        if _level = level then
                            writer.Post((_level, log))

                        ())
                    ()


    // TODO Reader
    let GetLogger config : ILogger =
        let handler = new LoggerState(config) |> LogHandler

        { Fatal = handler LogLevel.Fatal
          Error = handler LogLevel.Error
          Warn = handler LogLevel.Warn
          Info = handler LogLevel.Info
          Debug = handler LogLevel.Debug }
