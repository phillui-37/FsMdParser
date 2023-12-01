namespace FsMdParser

module Main =
    open FsMdParser.Logger
    open System.Threading

    
    [<EntryPoint>]
    let main args =
        let logger = Logger.GetLogger LogConfig.Default

        logger.Debug "test1"
        logger.Info "test2"
        logger.Warn "test3"
        logger.Error "test4"
        logger.Fatal "test5"

        Thread.Sleep 200

        0