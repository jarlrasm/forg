module  SystemTests.Tests
open System;
open System.Collections.Generic;
open System.Diagnostics;
open System.IO;
open System.Linq;
open System.Reflection;
open Xunit;

let sourcePath ()= Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) + "/source"
let sourceFiles ()= System.IO.Directory.GetFiles(sourcePath())  
let ExecuteProgramAndReturnStdOut filename=
   let start = new ProcessStartInfo();
   start.FileName <- filename;
   start.UseShellExecute <- false;
   start.RedirectStandardOutput <- true;
   let mutable result = "";
   use  process = Process.Start(start) 
   use reader = process.StandardOutput
   result <- result + reader.ReadToEnd()
   process.WaitForExit();
   result;
let GetSourceFileName name = sprintf "%s/%s" (sourcePath()) name;
    
let CompileFile  fileName = Program.main [|fileName|]

type Tests () =
    
    static member ForgFiles 
        with get() = sourceFiles() |> Array.map (fun x -> [|x|]) 
    static member ProgramOutputs 
        with get() =
            [ [|"helloworld.forg";"HelloWorld.exe";"Hello world!"+Environment.NewLine|];
            [|"hellosomeone.forg";"HelloSomeone.exe";"Yo"+Environment.NewLine|];
            [|"lambda.forg";"LambdaTest.exe";"Hello lambda!"+Environment.NewLine|];]
    
    
    
     
    [<Theory>]
    [<MemberData("ForgFiles")>]
    member x.CompileFilesWithoutErrors filename=
        Assert.Equal(0,CompileFile(filename))
    

    
    [<Theory>]
    [<MemberData("ProgramOutputs")>]
    member x.RunPrograms sourceFile exefile  expectedOutput =
        CompileFile(GetSourceFileName(sourceFile))
        let result = ExecuteProgramAndReturnStdOut(exefile)
        Assert.Equal(expectedOutput,result)
