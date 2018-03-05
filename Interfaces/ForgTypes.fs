module ForgTypes

open Ast
open System

type IForgType =
    abstract member AST:FullAssignment
    
type IForgModule =
    inherit IForgType
    
type IForgPrimitive<'a> =
    inherit IForgType
    abstract member Value: 'a
    
type IForgLambda<'output> =
    inherit IForgType
    
type IForgParameterlessFunc<'output> =
    inherit IForgLambda<'output>
    abstract member Execute:unit->unit
    abstract member HasResult: bool
    abstract member Result: 'output
    
type IForgFunc<'output, 'input> =
    inherit IForgLambda<'output>
    abstract member Execute:'input->unit
    abstract member HasResult: bool
    abstract member Result: 'output
    
type Helper() =
     static member getResult<'output>(func:IForgParameterlessFunc<'output>)=
         if(not func.HasResult) then
             func.Execute()
         func.Result;
         