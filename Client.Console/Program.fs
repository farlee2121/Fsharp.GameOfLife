// Learn more about F# at http://fsharp.org

open System
open System.CommandLine
open System.CommandLine.Invocation
open LifeRules
//open System.CommandLine.DragonFruit

open System.IO

let rootCommand description (options: Symbol seq) = 
    let command = new RootCommand(description)
    options |> Seq.iter (fun opt -> command.Add(opt)) 
    //command.Handler <- CommandHandler.Create(handler)
    command

let invokeCommand (command: Command) (argv: string array) = 
    command.InvokeAsync(argv)

// what could happen here
// - multiple colonies specified
// - colony not valid in some way (uneven dimensions, empty?)


type ParseColonyErrors = UnevenDimensions | FileNotFound
type ParseColonyResult = Result<Colony, ParseColonyErrors>
// core parsing should be devoid of error handling, add error handling as decorators
// choosing a parser will also require a result type, maybe InputMatchingResult?
let parseColonyFromString = ignore
let parseColonyFromFile = ignore

let parseColony = ignore
  
// what does a track-oriented way of handling the run options look like? runIfValid validator f (args|hasrun)?? 

[<EntryPoint>]
let main argv =
    //printf "%A" argv
    // Async in f# https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/asynchronous-and-concurrent-programming/async
    let command = (rootCommand "Play game of life yo" [ 
            new Option<string>([|"--str"; "-s" |], "String representing the colony, use \n for newlines")
            new Option<FileInfo>([|"-f"; "--file"|], "File with the colony to run, x or space for empty and o for live cells")
        ])

    //IMPORTANT: it won't properly match the arguments to a let-bound function, I have to use lambdas 
    command.Handler <- CommandHandler.Create<FileInfo, string>((fun file str -> (printf "how about now %s" str)))

    invokeCommand command argv
    |> Async.AwaitTask 
    |> Async.RunSynchronously


// input/output with validation
// shared save file read/write
// shared random colony generators