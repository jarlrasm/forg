module ForgTypes

open Ast
open System

type IForgType =
    abstract member AST:FullAssignment
    
type IForgModule =
    inherit IForgType
    
type IForgLambda<'output> =
    inherit IForgType
    abstract member HasResult: bool
    abstract member Result: 'output
    
type IForgParameterlessFunc<'output> =
    inherit IForgLambda<'output>
    abstract member Execute:unit->unit
    
type IForgFunc<'output, 'input> =
    inherit IForgLambda<'output>
    abstract member Execute:'input->unit
    
type ForgValue<'a>(data:'a) =
  let name=Guid.NewGuid().ToString()
  interface IForgParameterlessFunc<'a>  with
    override this.Execute() = ignore()
    override this.HasResult with get () = true
    override this.Result with get() = data
    override this.AST  with get() =  {Name=name;Assignment=ParameterlessAssignment ModuleKeyword;Where=List.empty}

    

    
type Helper() =
     static member getResult<'output>(func:IForgParameterlessFunc<'output>)=
         if(not func.HasResult) then
             func.Execute()
         func.Result;
         