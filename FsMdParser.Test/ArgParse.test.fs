namespace FsMdParser.Tests

open NUnit.Framework
open FsUnit
open FsMdParser.ArgParse

module ArgParseTest =
    open Argu
    let parser = ArgumentParser.Create<Arguments>("testing")

    [<Test>]
    let TestAllArg() =
        let results = (parser.Parse [|"--out-format"; "html"; "--in"; "testing.md"; "--log-level"; "info"; "--quiet"|]).GetAllResults()
        results |> should equivalent [Out_Format "html"; In "testing.md"; Log_Level "info"; Quiet]