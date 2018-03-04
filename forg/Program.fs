﻿open FParsec
open ForgCore

let context =
    ForgContext.createContext [typeof<Core>.Assembly]
[<EntryPoint>]
let main argv =
    let code = System.IO.File.ReadAllText(Array.last argv);
    
    match run ForgParser.parser code with
    | Success(result, _, _)   -> ForgWriter.push result context;
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
    
    0 // return an integer exit code
