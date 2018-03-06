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
     | FunctionCall fcall-> //TODO This cannot be right
        (getTypeOf fcall.Function context).GetMethod("get_Result").ReturnType;

     | Reference ref-> 
           let symbol=ForgContext.lookup context ref
           match symbol.Ref with
           | SystemType sys ->
               sys
           | Parameter parameter -> 
               parameter
           | _ -> raise (InvalidRef symbol.Ref)
     | StringLiteral str-> 
           typeof<string>
 
    
let rec writeExpression (exp:Expression) (il:ILGenerator) (context:Context)=
     match exp with
     | FunctionCall fcall->
        if(fcall.Argument.IsSome) then
            writeExpression fcall.Function il context
            il.Emit(OpCodes.Dup)
            writeExpression fcall.Argument.Value il context
            let functype=getTypeOf fcall.Function context
            il.Emit(OpCodes.Callvirt, functype.GetMethod("Execute"))
            il.Emit(OpCodes.Callvirt, functype.GetMethod("get_Result",BindingFlags.FlattenHierarchy|||BindingFlags.Public|||BindingFlags.Instance))
        else
            writeExpression fcall.Function il context
            il.Emit(OpCodes.Dup)
            let functype=getTypeOf fcall.Function context
            il.Emit(OpCodes.Callvirt, functype.GetMethod("Execute"))
            il.Emit(OpCodes.Callvirt, functype.GetMethod("get_Result",BindingFlags.FlattenHierarchy|||BindingFlags.Public|||BindingFlags.Instance))

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
            let symbol=ForgContext.lookup context {Name="String";Namespace=["ForgCore"]} 
            match symbol.Ref with
            | SystemType sys ->
               il.Emit(OpCodes.Ldstr,str)
               let constructor=(sys.GetConstructor([|typeof<string>|]))
               il.Emit(OpCodes.Newobj, constructor)
            | _ -> raise (InvalidRef symbol.Ref)

let buildProperty (typeBuilder:TypeBuilder) (systemType:System.Type) (name:string) : PropertyBuilder=
     let backingName="_"+name.ToLower()
     let field = typeBuilder.DefineField(backingName,  systemType,  FieldAttributes.Private)
     let propertyBuilder = typeBuilder.DefineProperty(name,PropertyAttributes.None,systemType,null)
     let getSetAttr = 
                 MethodAttributes.Public ||| MethodAttributes.SpecialName |||
                     MethodAttributes.HideBySig
     let getBuilder =  typeBuilder.DefineMethod("get_"+name,
                                            getSetAttr,        
                                            systemType,
                                            Type.EmptyTypes)
     
     let ilgen = getBuilder.GetILGenerator()
     ilgen.Emit(OpCodes.Ldarg_0)
     ilgen.Emit(OpCodes.Ldfld, field)
     ilgen.Emit(OpCodes.Ret)
     let setBuilder =  typeBuilder.DefineMethod("set_"+name,
                                            getSetAttr,     
                                            null,
                                            [|systemType|])
     
     let ilgen = setBuilder.GetILGenerator()
     ilgen.Emit(OpCodes.Ldarg_0)
     ilgen.Emit(OpCodes.Ldarg_1)
     ilgen.Emit(OpCodes.Stfld, field)
     ilgen.Emit(OpCodes.Ret)
     propertyBuilder.SetGetMethod(getBuilder)
     propertyBuilder.SetSetMethod(setBuilder)
     propertyBuilder
    
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
                
             | Expression expression -> 
                let nestedTypeBuilder=typeBuilder.DefineNestedType(assignment.Name,TypeAttributes.NestedPublic)//TODO  implement interface
                let frame = List.map (fun innerAssignment -> writeAssignment innerAssignment typeBuilder context) assignment.Where
                                   |> List.filter Option.isSome
                                   |> List.map Option.get  
                let context=ForgContext.pushFrame context frame
                let constructorBuilder=nestedTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||] )
                let ilGenerator=constructorBuilder.GetILGenerator()
                ilGenerator.Emit(OpCodes.Ldarg_0)
                ilGenerator.Emit(OpCodes.Call,typeof<Object>.GetConstructor(Array.Empty()))
                ilGenerator.Emit(OpCodes.Ret)
                let propertyBuilder= buildProperty nestedTypeBuilder (getTypeOf expression context) "Result" 
                let methodBuilder = nestedTypeBuilder.DefineMethod("Execute",  MethodAttributes.HideBySig ||| MethodAttributes.Public,null, [||] )
                
                let ilGenerator = methodBuilder.GetILGenerator() 
                ilGenerator.Emit(OpCodes.Ldarg_0)
                writeExpression expression ilGenerator context
                ilGenerator.Emit(OpCodes.Call, propertyBuilder.GetSetMethod())
                ilGenerator.Emit(OpCodes.Ret)
                let created = nestedTypeBuilder.CreateType()
                Some {SymbolName=created.Name;Namespace=[];Ref=SystemType created}
                
        | FunctionAssignment functionAssignment -> 
            let nestedTypeBuilder=typeBuilder.DefineNestedType(assignment.Name,TypeAttributes.NestedPublic)//TODO  implement interface
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
            let propertyBuilder= buildProperty nestedTypeBuilder (getTypeOf functionAssignment.Expression context) "Result" 
            let methodBuilder = nestedTypeBuilder.DefineMethod("Execute",  MethodAttributes.HideBySig ||| MethodAttributes.Public,null, [|argtype|] )
            
            let context=ForgContext.pushFrame context [{SymbolName = functionAssignment.Parameter.Name; Namespace=[]; Ref= Parameter argtype}]
            let ilGenerator = methodBuilder.GetILGenerator() 
            ilGenerator.Emit(OpCodes.Ldarg_0)
            writeExpression functionAssignment.Expression ilGenerator context
            ilGenerator.Emit(OpCodes.Call, propertyBuilder.GetSetMethod())
            ilGenerator.Emit(OpCodes.Ret)
            let created = nestedTypeBuilder.CreateType()
            Some {SymbolName=created.Name;Namespace=[];Ref=SystemType created}
        | TypeDeclaration typedeclaration ->  None
        | GenericTypeDeclaration generictypedecl -> None

let push (code:List<FullAssignment>) (context:Context):unit=  
    for assignment in code do
        writeAssignment assignment null context|>ignore
    Console.WriteLine "Done"
    Console.WriteLine code