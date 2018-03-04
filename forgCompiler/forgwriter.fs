module ForgWriter
open ForgParser
open System.Reflection
open System.Reflection.Emit
open System.Threading
open Ast
open ForgContext
open System
open ForgTypes

exception InvalidRef of Ref
let createMainFunc (typeBuilder:TypeBuilder) (assemblyBuilder:AssemblyBuilder)  (context:Context)=
   Console.WriteLine "Creating main"
   let methodBuilder = typeBuilder.DefineMethod("Main", MethodAttributes.HideBySig ||| MethodAttributes.Static ||| MethodAttributes.Public, typeof<int>, [|typeof<string>|])                          
   let ilGenerator = methodBuilder.GetILGenerator()
   let mainclass=ForgContext.lookup context {Name ="main"; Namespace =[]}
   match mainclass.Ref with
   | SystemType maintype ->
       let constructor= (maintype.GetConstructor(Array.Empty()))
       ilGenerator.Emit(OpCodes.Newobj,constructor)
       let method=maintype.GetMethod "Execute"
       ilGenerator.Emit(OpCodes.Newobj,(Seq.head (method.GetParameters())).ParameterType.GetConstructor(Array.Empty()))
       ilGenerator.Emit(OpCodes.Call, method)
   | _ -> raise (InvalidRef mainclass.Ref)
   ilGenerator.Emit(OpCodes.Ldc_I4, 0)
   ilGenerator.Emit(OpCodes.Ret)
   assemblyBuilder.SetEntryPoint(methodBuilder, PEFileKinds.ConsoleApplication)

let GetSystemtypeFrom (symbol :Symbol) =
    match  symbol.Ref with
    | SystemType t ->  t
    | _ -> raise (InvalidRef symbol.Ref)
    
let rec getTypeOf (exp:Expression) (context:Context) : System.Type=
     match exp with
     | FunctionCall fcall->
        getTypeOf fcall.Function context

     | Reference ref-> 
           let symbol=ForgContext.lookup context ref
           match symbol.Ref with
           | SystemType sys ->
               printf "%A" sys
               sys
           | Parameter parameter -> 
               parameter
           | _ -> raise (InvalidRef symbol.Ref)
     | StringLiteral str-> 
           typeof<string>
 
    
let rec writeExpression (exp:Expression) (il:ILGenerator) (context:Context)=
     match exp with
     | FunctionCall fcall->
        printf "%A" fcall
        if(fcall.Argument.IsSome) then
            writeExpression fcall.Function il context
            writeExpression fcall.Argument.Value il context
            let functype=typeof<ForgTypes.Helper>.GetMethod("getResult").MakeGenericMethod([|getTypeOf fcall.Function context|])
            il.Emit(OpCodes.Callvirt, functype) 
            let functype=getTypeOf fcall.Function context
            il.Emit(OpCodes.Callvirt, functype.GetMethod("Execute"))
        else
            raise (NotImplementedException "TODO")

     | Reference ref-> 
           let symbol=ForgContext.lookup context ref
           match symbol.Ref with
           | SystemType sys ->
               let constructor=(sys.GetConstructor(Array.Empty()))
               il.Emit(OpCodes.Newobj, constructor)
           | Parameter parameter -> 
               il.Emit(OpCodes.Ldarg_1)
           | _ -> raise (InvalidRef symbol.Ref)
     | StringLiteral str-> 
               let constructor=(typeof<ForgTypes.ForgValue<string>>.GetConstructor([|typeof<string>|] ))
               il.Emit(OpCodes.Ldstr,str)
               il.Emit(OpCodes.Newobj, constructor)

       
    
let rec writeAssignment assignment (typeBuilder:TypeBuilder) (context:Context):Option<Symbol>=   
        Console.WriteLine ("Creating " + assignment.Name)
        match assignment.Assignment with
        | ParameterlessAssignment(parameterlessassignment)->
            match parameterlessassignment with
             |ModuleKeyword ->
                let assemblyName = new AssemblyName ()
                assemblyName.Name <- assignment.Name
                let assemblyBuilder =  Thread.GetDomain().DefineDynamicAssembly(assemblyName,
                                                            AssemblyBuilderAccess.RunAndSave)
                let modl = assemblyBuilder.DefineDynamicModule(assignment.Name+".exe")
    
                let typeBuilder = modl.DefineType(assignment.Name,
                                                TypeAttributes.Public |||
                                                TypeAttributes.Class)

                let frame = (List.map (fun innerAssignment -> writeAssignment innerAssignment typeBuilder context) assignment.Where)
                            |> List.filter Option.isSome
                            |> List.map Option.get 
                let context=ForgContext.pushFrame context frame
                if List.exists (fun x-> x.Name = "main") assignment.Where then
                        createMainFunc typeBuilder assemblyBuilder context|> ignore
                        let created=typeBuilder.CreateType()
                        assemblyBuilder.Save( assignment.Name+".exe")
                        Some {SymbolName=created.Name;Namespace=[];Ref=SystemType created}
                    else
                        let created=typeBuilder.CreateType()
                        assemblyBuilder.Save( assignment.Name+".dll")
                        Some{SymbolName=created.Name;Namespace=[];Ref=SystemType created}
                
             | Expression(exp) -> None
        | FunctionAssignment functionAssignment -> 
            //let returntype=ForgContext.lookup context functionAssignment.

            let nestedTypeBuilder=typeBuilder.DefineNestedType(assignment.Name,TypeAttributes.Public)//TODO  implement interface
            let frame = List.map (fun innerAssignment -> writeAssignment innerAssignment typeBuilder context) assignment.Where
                               |> List.filter Option.isSome
                               |> List.map Option.get  
            let context=ForgContext.pushFrame context frame
            let constructorBuilder=nestedTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||] )
            let ilGenerator=constructorBuilder.GetILGenerator()
            ilGenerator.Emit(OpCodes.Ldarg_0)
            ilGenerator.Emit(OpCodes.Call,typeof<Object>.GetConstructor(Array.Empty()))
            ilGenerator.Emit(OpCodes.Ret)
            let argtype=GetSystemtypeFrom (ForgContext.lookup context functionAssignment.Parameter.TypeReference.Value)
            let methodBuilder = nestedTypeBuilder.DefineMethod("Execute",  MethodAttributes.HideBySig ||| MethodAttributes.Public,null, [|argtype|] )
            
            let context=ForgContext.pushFrame context [{SymbolName = functionAssignment.Parameter.Name; Namespace=[]; Ref= Parameter argtype}]
            let ilGenerator = methodBuilder.GetILGenerator()
            writeExpression functionAssignment.Expression ilGenerator context
            ilGenerator.Emit(OpCodes.Ret)
            let created = nestedTypeBuilder.CreateType()
            Some {SymbolName=created.Name;Namespace=[];Ref=SystemType created}

let push (code:List<FullAssignment>) (context:Context):unit=  
    for assignment in code do
        writeAssignment assignment null context|>ignore
    Console.WriteLine "Done"
    //Console.WriteLine code