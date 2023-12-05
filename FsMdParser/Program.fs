namespace FsMdParser

// TODO add register module code
// all implement an interface to provide now can be closed or not
// and a method to change its state, to stop new task

module Main =
    open FsMdParser.Logger
    open FsMdParser.Context
    open System
    open System.Threading
    
    type CtxLogger =
        { logger: ILogger;
          status: Context.ContextComponentStatus ref }
        interface Context.IContextComponent with
            member this.Get() = this.logger
            member this.Close() =
                this.status.Value <- Context.ContextComponentStatus.Closing
                while this.logger.IsBusy() do
                    Thread.Sleep 50
                this.status.Value <- Context.ContextComponentStatus.Dead
            member this.Status() = this.status.Value

    let getCtxLogger() =
        let status = ref Context.ContextComponentStatus.Idle
        let _logger = Logger.GetLogger LogConfig.Default
        let logGuard logFn (msg: string) =
            let _status = status.Value
            if _status <> Context.ContextComponentStatus.Closing && _status <> Context.ContextComponentStatus.Dead then
                logFn msg
        { logger={ Fatal =logGuard _logger.Fatal;
                   Error =logGuard _logger.Error;
                   Warn  =logGuard _logger.Warn;
                   Info  =logGuard _logger.Info;
                   Debug =logGuard _logger.Debug;
                   IsBusy=_logger.IsBusy };
          status=status }
        

    let initContext() =
        Context.Of()
        |> Context.Register Logger (getCtxLogger())

    
    [<EntryPoint>]
    let main args =
        let context = initContext()
        let logger: ILogger = (Context.GetComponent Logger context).Get() :?> ILogger
        
        Context.Close context
        logger.Warn "Test after close"
        while context |> Context.IsClosed |> not do
            printfn "waiting"
            Thread.Sleep 50

        0