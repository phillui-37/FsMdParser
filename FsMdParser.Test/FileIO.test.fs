namespace FsMdParser.Tests

open NUnit.Framework
open FsUnit
open FsMdParser.FileIO

// TODO setup teardown

module FileIOTest =
    [<Test>]
    let TestReadTestFile() =
        let content = (
            task {
               let! ret = ReadTextFile "test/test.md"
               return ret
            }
            |> Async.AwaitTask
            |> Async.RunSynchronously)
        content |> should equal "testing"