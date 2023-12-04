namespace FsMdParser

// TODO add register module code
// all implement an interface to provide now can be closed or not
// and a method to change its state, to stop new task

module Main =
    open FsMdParser.Logger
    open FsMdParser.Context
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
          status=ref Context.ContextComponentStatus.Idle }
        

    let initContext() =
        Context.Of()
        |> Context.Register Logger (getCtxLogger())

    
    [<EntryPoint>]
    let main args =
        let context = initContext()
        let logger: ILogger = (Context.GetComponent Logger context).Get() :?> ILogger

        logger.Debug "test1"
        logger.Info "test2"
        logger.Warn "test3"
        logger.Error "test4"
        logger.Fatal "test5"
        logger.Debug "test6"
        logger.Info "test7"
        logger.Warn "test8"
        logger.Error "test9"
        logger.Fatal "test10"
        logger.Debug "test11"
        logger.Info "test12"
        logger.Warn "test13"
        logger.Error "test14"
        logger.Fatal "test15"

        Context.Close context
        while context |> Context.IsClosed |> not do
            printfn "waiting"
            Thread.Sleep 50

        0