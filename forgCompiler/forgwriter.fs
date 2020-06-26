module ForgWriter
open ForgParser
open System.Reflection
open System.Reflection.Emit
open System.Threading
open NetAst
open ForgContext
open ForgParser
open ForgParser
open ForgParser
open ForgParser
open System
open ForgTypes

let push (code:List<NetAst.ClassDeclaration>) (context:Context):unit=  
    printf "%A\n" code