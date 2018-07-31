open FParsec
open ForgCore

let context =
    ForgContext.createContext [typeof<Core>.Assembly]
[<EntryPoint>]
let main argv =
    let file= Array.last argv;
    System.Console.WriteLine("Compiling{0}",file)
    
    let code = System.IO.File.ReadAllText(file);
    
    match run ForgParser.parser code with
    | Success(result, _, _)   -> ForgWriter.push result context;
                                 0
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
                                 1