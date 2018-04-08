module ForgContext

open Ast

open System.Reflection
open ForgTypes
type Ref = 
  SystemType of System.Type 
  |Parameter of System.Type 
  |ClosureRef of System.Type 
type Symbol = {SymbolName:string; Namespace:List<string>; Ref:Ref;}
type Context = {Symbols:List<List<Symbol>>}

let createContext (assemblies:List<Assembly>)=

    { Symbols= [assemblies
         |> List.collect (fun x-> x.GetTypes() |>List.ofSeq) 
         |> List.filter (fun x-> typeof<IForgModule>.IsAssignableFrom(x))
         |> List.map 
            (fun x-> (x,x.GetNestedTypes()  
                |>List.ofSeq
                |> List.filter (fun (y)-> typeof<IForgType>.IsAssignableFrom(y))
            ))
         |> List.collect (fun (x,y)-> y|> List.map (fun z->{SymbolName=z.Name; Namespace=[x.Name]; Ref=SystemType z}))
        ]
     }
 
let pushFrame context symbols= {Symbols = symbols::context.Symbols}
let pushLambda context symbols= {Symbols = symbols::
                                             (context.Symbols
                                                 |> List.collect (fun x->x)
                                                 |> List.filter (fun x-> match x.Ref with
                                                                                  | Parameter para -> true
                                                                                  | _ -> false) 
                                                 |> List.map (fun x -> {SymbolName=x.SymbolName; Namespace=x.Namespace; Ref=ClosureRef (match x.Ref with
                                                                                                                                        | Parameter para -> para)}))::context.Symbols}
        
let popFrame context = {Symbols = context.Symbols.Tail}
    
let lookup context (reference:Ast.Reference)=
    context.Symbols 
     |> List.collect (fun x->x)
     |> List.filter (fun x-> x.SymbolName = reference.Name)//TODO namespace
     |> List.tryHead
     
let getParameters context = 
     context.Symbols 
     |> List.collect (fun x->x)
     |> List.filter (fun x-> match x.Ref with
                                      | Parameter para -> (lookup context {Name=x.SymbolName; Namespace=[]}).Value = x
                                      | _ -> false)