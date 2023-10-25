namespace FsMdParser.Tests

open NUnit.Framework
open FsUnit
open FsMdParser.FileIO
open System.IO

// TODO setup teardown

module FileIOTest =
    let filepath = "test.txt"
    let test_content = "testing"

    [<SetUp>]
    let Init() =
        File.WriteAllLines(filepath, [test_content])

    [<TearDown>]
    let Final() =
        File.Delete filepath

    [<Test>]
    let TestReadTestFile() =
        let content = 
            ReadTextFile filepath
            |> Async.AwaitTask
            |> Async.RunSynchronously
        match content with
        | Ok result -> result |> should equivalent [test_content]
        | Error e -> Assert.Fail(e)
    
    [<Test>]
    let TestReadNonExistFile() =
        task {
            match! ReadTextFile "123.hahaha" with
            | Error _ -> ignore()
            | _       -> Assert.Fail()
        } |> Async.AwaitTask |> Async.RunSynchronously