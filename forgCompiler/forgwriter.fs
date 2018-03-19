module ForgWriter
open ForgParser
open System.Reflection
open System.Reflection.Emit
open System.Threading
open Ast
open ForgContext
open ForgParser
open ForgParser
open System
open ForgTypes

exception InvalidRef of Ref
exception InvalidExpression of Expression
exception InvalidParameter of Parameter
let createMainFunc (typeBuilder:TypeBuilder) (assemblyBuilder:AssemblyBuilder)  (context:Context)=
   Console.WriteLine "Creating main"
   let methodBuilder = typeBuilder.DefineMethod("Main", MethodAttributes.HideBySig ||| MethodAttributes.Static ||| MethodAttributes.Public, typeof<int>, [|typeof<string>|])                          
   let ilGenerator = methodBuilder.GetILGenerator()
   let mainclass=ForgContext.lookup context {Name ="main"; Namespace =[]}
   match mainclass.Value.Ref with
   | SystemType maintype ->
       let constructor= (maintype.GetConstructor(Array.Empty()))
       ilGenerator.Emit(OpCodes.Newobj,constructor)
       let method=maintype.GetMethod "Execute"
       ilGenerator.Emit(OpCodes.Newobj,(Seq.head (method.GetParameters())).ParameterType.GetConstructor(Array.Empty()))
       ilGenerator.Emit(OpCodes.Call, method)
   | _ -> raise (InvalidRef mainclass.Value.Ref)
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
        (getTypeOf fcall.Function context).GetMethod("Execute").ReturnType;

     | Reference ref-> 
           let symbol=ForgContext.lookup context ref
           match symbol.Value.Ref with
           | SystemType sys ->
               sys
           | Parameter parameter -> 
               parameter
           | _ -> raise (InvalidRef symbol.Value.Ref)
     | StringLiteral str-> 
           typeof<string>
     | IntLiteral intl-> 
           typeof<int>
     | Constructor constructor-> 
           let symbol=ForgContext.lookup context constructor.TypeReference.Value //Again:TODO
           match symbol.Value.Ref with
           | SystemType sys ->
               sys
           | Parameter parameter -> 
               parameter
           | _ -> raise (InvalidRef symbol.Value.Ref)
           
let rec getLambdaParameterType (lambdaParam:Parameter) context =
          match lambdaParam.TypeReference  with
            Some typeRef ->
                match typeRef with
                |SimpleTypeReference simpleTypeReference -> GetSystemtypeFrom (ForgContext.lookup context simpleTypeReference).Value
                |LambdaReference lambdatypeRef -> raise (NotImplementedException "Nope")
                
let rec generateLambdaClass (lambda:Lambda) (typeBuilder:TypeBuilder) (context:Context) =
    let name="ForgLambda"+Guid.NewGuid().ToString()
    printf "Creating %A\n" name
    let symbol=ForgContext.lookup context {Name="Closure";Namespace=["ForgCore"]} 
    match symbol.Value.Ref with
    | SystemType closure ->
        match lambda.Parameter with
            |Some parameter ->
                let paramType= getLambdaParameterType parameter context
                let context=ForgContext.pushFrame context [{SymbolName = parameter.Name; Namespace=[]; Ref=Parameter paramType}]
                let expressionType=getTypeOf lambda.LambdaExpression context
                let functype=typeof<IForgFunc<_,_>>.GetGenericTypeDefinition().MakeGenericType(expressionType,paramType)
                let nestedTypeBuilder = typeBuilder.DefineNestedType(name,TypeAttributes.NestedPublic , functype)
                let constructorBuilder=nestedTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|closure|] )
                let closureProp= buildProperty nestedTypeBuilder closure "Closure" 
                let ilGenerator=constructorBuilder.GetILGenerator()
                ilGenerator.Emit(OpCodes.Ldarg_0)
                ilGenerator.Emit(OpCodes.Dup)
                ilGenerator.Emit(OpCodes.Call,typeof<Object>.GetConstructor(Array.Empty()))
                ilGenerator.Emit(OpCodes.Ldarg_1)
                ilGenerator.Emit(OpCodes.Call, closureProp.GetSetMethod(true))
                ilGenerator.Emit(OpCodes.Ret)
                
                let methodBuilder = nestedTypeBuilder.DefineMethod("Execute",  MethodAttributes.HideBySig ||| MethodAttributes.Public,expressionType, [|paramType|] )
                
                let ilGenerator = methodBuilder.GetILGenerator() 
                writeExpression lambda.LambdaExpression ilGenerator context typeBuilder
                ilGenerator.Emit(OpCodes.Ret)
                nestedTypeBuilder.CreateType()
            |None ->
                let expressionType=getTypeOf lambda.LambdaExpression context
                let functype=typeof<IForgParameterlessFunc<_>>.GetGenericTypeDefinition().MakeGenericType(expressionType)
                let nestedTypeBuilder =typeBuilder.DefineNestedType(name,TypeAttributes.NestedPublic , functype)
                let constructorBuilder=nestedTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|closure|] )
                let closureProp= buildProperty nestedTypeBuilder closure "Closure" 
                let ilGenerator=constructorBuilder.GetILGenerator()
                ilGenerator.Emit(OpCodes.Ldarg_0)
                ilGenerator.Emit(OpCodes.Dup)
                ilGenerator.Emit(OpCodes.Call,typeof<Object>.GetConstructor(Array.Empty()))
                ilGenerator.Emit(OpCodes.Ldarg_1)
                ilGenerator.Emit(OpCodes.Call, closureProp.GetSetMethod(true))
                ilGenerator.Emit(OpCodes.Ret)
                let methodBuilder = nestedTypeBuilder.DefineMethod("Execute",  MethodAttributes.HideBySig ||| MethodAttributes.Public,expressionType, [||] )
                
                let ilGenerator = methodBuilder.GetILGenerator() 
                writeExpression lambda.LambdaExpression ilGenerator context typeBuilder
                ilGenerator.Emit(OpCodes.Ret)
                nestedTypeBuilder.CreateType()
                
    | _-> raise (InvalidRef symbol.Value.Ref)
    
and writeExpression (exp:Expression) (il:ILGenerator) (context:Context) (typeBuilder:TypeBuilder)=
     match exp with
     | FunctionCall fcall->
        match fcall.Argument with
        | Some arg ->
            writeExpression fcall.Function il context typeBuilder
            il.Emit(OpCodes.Dup)
            writeExpression arg il context typeBuilder
            let functype=getTypeOf fcall.Function context
            il.Emit(OpCodes.Callvirt, functype.GetMethod("Execute"))
        | None ->
            writeExpression fcall.Function il context typeBuilder
            il.Emit(OpCodes.Dup)
            let functype=getTypeOf fcall.Function context
            il.Emit(OpCodes.Callvirt, functype.GetMethod("Execute"))

     | Reference ref-> 
           let maybeSymbol=ForgContext.lookup context ref
           match maybeSymbol with
           | None ->
              raise (InvalidExpression exp)
           | Some symbol ->
               match symbol.Ref with
               | SystemType sys ->
                   let constructor=(sys.GetConstructor(Array.Empty()))
                   il.Emit(OpCodes.Newobj, constructor)
               | Parameter parameter -> 
                   il.Emit(OpCodes.Ldarg_1)
               | _ -> raise (InvalidRef symbol.Ref)
     | StringLiteral str-> 
            let symbol=ForgContext.lookup context {Name="String";Namespace=["ForgCore"]} 
            match symbol.Value.Ref with
            | SystemType sys ->
               il.Emit(OpCodes.Ldstr,str)
               let constructor=(sys.GetConstructor([|typeof<string>|]))
               il.Emit(OpCodes.Newobj, constructor)
            | _ -> raise (InvalidRef symbol.Value.Ref)
     | IntLiteral int-> 
            let symbol=ForgContext.lookup context {Name="Int";Namespace=["ForgCore"]} 
            match symbol.Value.Ref with
            | SystemType sys ->
               il.Emit(OpCodes.Ldc_I4,int)
               let constructor=(sys.GetConstructor([|typeof<int>|]))
               il.Emit(OpCodes.Newobj, constructor)
            | _ -> raise (InvalidRef symbol.Value.Ref)
     | Constructor constructor ->
            match constructor.TypeReference with
                | Some typeref ->
                    let symbol=ForgContext.lookup context typeref 
                    match symbol.Value.Ref with
                    | SystemType sys ->
                       
                       let typeconstructor=Array.head (sys.GetConstructors())
                       let parameters=typeconstructor.GetParameters()
                       if(parameters.Length <> constructor.Assignments.Length) then
                         raise (InvalidRef symbol.Value.Ref)
                       for param in parameters do
                            let assignment = constructor.Assignments |> List.find (fun x-> x.Name.ToLower()=param.Name.ToLower())
                            writeExpression assignment.Value il context  typeBuilder
                       il.Emit(OpCodes.Newobj, typeconstructor)
                    | _ -> raise  (NotImplementedException "Nope")
                | None -> ignore()// TODO
     | SimpleDestructor destructor ->
            writeExpression destructor.DataObject il context typeBuilder
            let functype=getTypeOf destructor.DataObject context
            il.Emit(OpCodes.Call, functype.GetMethod("get_"+destructor.Name))
     | Lambda lambda -> 
            let lambdaClass=generateLambdaClass lambda typeBuilder context 
            let listType=typeof<System.Collections.Generic.List<System.Collections.Generic.KeyValuePair<string,Object>>>
            il.Emit(OpCodes.Newobj,listType.GetConstructor([||]))
            for symbol in getParameters context do 
                il.Emit(OpCodes.Dup)
                il.Emit(OpCodes.Ldstr, symbol.SymbolName)
                il.Emit(OpCodes.Ldarg_1)//TODO more than one arg here...
                il.Emit(OpCodes.Newobj,typeof<System.Collections.Generic.KeyValuePair<string,Object>>.GetConstructor([|typeof<string>;typeof<Object>|]))
                il.Emit(OpCodes.Callvirt, listType.GetMethod("Add"))
                
            let symbol=ForgContext.lookup context {Name="Closure";Namespace=["ForgCore"]} 
            match symbol.Value.Ref with
            | SystemType close ->
               il.Emit(OpCodes.Newobj, close.GetConstructor([|listType|]))
               il.Emit(OpCodes.Newobj, lambdaClass.GetConstructor([|close|]))
            | _ -> raise (InvalidRef symbol.Value.Ref)
                
                      
             
     
and buildProperty (typeBuilder:TypeBuilder) (systemType:System.Type) (name:string) : PropertyBuilder=
     printf "Creating %s\n"  name
     let backingName="_"+name.ToLower()
     let field = typeBuilder.DefineField(backingName,  systemType,  FieldAttributes.Private)
     let propertyBuilder = typeBuilder.DefineProperty(name,PropertyAttributes.None,systemType,null)
     let getAttr = 
                 MethodAttributes.Public ||| MethodAttributes.SpecialName |||
                     MethodAttributes.HideBySig
     let setAttr = 
                 MethodAttributes.Private ||| MethodAttributes.SpecialName |||
                     MethodAttributes.HideBySig
     let getBuilder =  typeBuilder.DefineMethod("get_"+name,
                                            getAttr,        
                                            systemType,
                                            Type.EmptyTypes)
     
     let ilgen = getBuilder.GetILGenerator()
     ilgen.Emit(OpCodes.Ldarg_0)
     ilgen.Emit(OpCodes.Ldfld, field)
     ilgen.Emit(OpCodes.Ret)
     let setBuilder =  typeBuilder.DefineMethod("set_"+name,
                                            setAttr,     
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

let writeDataType (data:DataType) (typeBuilder:TypeBuilder) (context:Context)=
    let props=data |>
              List.map (fun parameter->
                let systype=match parameter.TypeReference with 
                             | Some typeRef-> match typeRef with
                                               |SimpleTypeReference tyref -> GetSystemtypeFrom (ForgContext.lookup context tyref).Value
                             | None-> raise (InvalidParameter parameter)
                buildProperty typeBuilder systype parameter.Name)
    let constructorArgs= props |>
                            List.map (fun prop->prop.PropertyType) |> List.toArray    
           
    let constructorBuilder=typeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, constructorArgs )
    let ilGenerator=constructorBuilder.GetILGenerator()
    ilGenerator.Emit(OpCodes.Ldarg_0)
    ilGenerator.Emit(OpCodes.Call,typeof<Object>.GetConstructor(Array.Empty()))
    let mutable count = 1s
    for prop in props do
        ilGenerator.Emit(OpCodes.Ldarg_0)
        ilGenerator.Emit(OpCodes.Ldarg,count)
        ilGenerator.Emit(OpCodes.Call, prop.GetSetMethod(true))
        constructorBuilder.DefineParameter(int count,ParameterAttributes.None,prop.Name) |>ignore
        count <- count + 1s
    ilGenerator.Emit(OpCodes.Ret)
    
let buildPrivateDefaultConstructor (typeBuilder:TypeBuilder)=
    let constructorBuilder=typeBuilder.DefineConstructor(MethodAttributes.Private, CallingConventions.Standard, [||] )
    let ilGenerator=constructorBuilder.GetILGenerator()
    ilGenerator.Emit(OpCodes.Ldarg_0)
    ilGenerator.Emit(OpCodes.Call,typeof<Object>.GetConstructor(Array.Empty()))
    ilGenerator.Emit(OpCodes.Ret)
    constructorBuilder
    
let buildPropertySettingConstructor (typeBuilder:TypeBuilder) (propertyBuilder:PropertyBuilder)(attributes:MethodAttributes)=
    let constructorBuilder=typeBuilder.DefineConstructor(attributes, CallingConventions.Standard, [|propertyBuilder.PropertyType|] )
    let ilGenerator=constructorBuilder.GetILGenerator()
    ilGenerator.Emit(OpCodes.Ldarg_0)
    ilGenerator.Emit(OpCodes.Call,typeof<Object>.GetConstructor(Array.Empty()))
    ilGenerator.Emit(OpCodes.Ldarg_0)
    ilGenerator.Emit(OpCodes.Ldarg_1)
    ilGenerator.Emit(OpCodes.Call,propertyBuilder.GetSetMethod(true))
    ilGenerator.Emit(OpCodes.Ret)
    constructorBuilder

let buildPrivatePropertySettingConstructor (typeBuilder:TypeBuilder) (propertyBuilder:PropertyBuilder)=
 buildPropertySettingConstructor typeBuilder propertyBuilder MethodAttributes.Private

let writeAlgebraicType (algebraic:AlgebraicType) (typeBuilder:TypeBuilder) (context:Context)=
    buildPrivateDefaultConstructor typeBuilder |> ignore
    let valueProp=buildProperty typeBuilder typeof<Object> "Value"
    let constructor=buildPrivatePropertySettingConstructor typeBuilder valueProp
    for opt in algebraic do
     match opt with
        | TypeOption.Parameter param -> 
           let systype= match param.TypeReference with 
                        |Some ref -> 
                                match ref with 
                                | SimpleTypeReference tref -> GetSystemtypeFrom (ForgContext.lookup context tref).Value //TODO check(many places)
                        |None-> raise (NotImplementedException "TODO")
           let nestedtypeBuilder= typeBuilder.DefineNestedType(param.Name,TypeAttributes.NestedPublic)
           let propertyBuilder= buildProperty nestedtypeBuilder systype "Value"
           let innerClassConstructor=buildPropertySettingConstructor nestedtypeBuilder propertyBuilder MethodAttributes.Public
           let methodBuilder = typeBuilder.DefineMethod("New"+param.Name, MethodAttributes.Static |||  MethodAttributes.HideBySig ||| MethodAttributes.Public,typeBuilder, [|systype|] )
           let ilGenerator = methodBuilder.GetILGenerator()
           ilGenerator.Emit(OpCodes.Ldarg_0)
           ilGenerator.Emit(OpCodes.Newobj, innerClassConstructor)
           ilGenerator.Emit(OpCodes.Newobj,constructor)
           ilGenerator.Emit(OpCodes.Ret)
        | TypeOption.Atom atom-> 
           let methodBuilder = typeBuilder.DefineMethod(atom.Name, MethodAttributes.Static |||  MethodAttributes.HideBySig ||| MethodAttributes.Public,typeBuilder, [||] )
           let ilGenerator = methodBuilder.GetILGenerator()
           let symbol=ForgContext.lookup context {Name="Atom";Namespace=["ForgCore"]} 
           match symbol.Value.Ref with
            | SystemType sys ->
               ilGenerator.Emit(OpCodes.Ldstr,atom.Name)
               let constructorAtom=(sys.GetConstructor([|typeof<string>|]))
               ilGenerator.Emit(OpCodes.Newobj, constructorAtom)
               ilGenerator.Emit(OpCodes.Newobj, constructor)
               ilGenerator.Emit(OpCodes.Ret)
            | _ -> raise (InvalidRef symbol.Value.Ref)
        | _ -> ignore() 
        
    

let writeType  typedeclaration (typeBuilder:TypeBuilder) (context:Context)=   
     match typedeclaration with
        | Data data-> writeDataType  data typeBuilder context                   
        | Algebraic algebraic->writeAlgebraicType algebraic typeBuilder context    
        | Atom atom-> atom|> ignore  //TODO?
        | Primitive -> ignore() //Should never happen(famous last words)
let rec getLambdaType (lambdatypeRef:LambdaReference) context =
    let returnType=match lambdatypeRef.Return with
                   |SimpleTypeReference simpleTypeReference -> GetSystemtypeFrom (ForgContext.lookup context simpleTypeReference).Value
                   |LambdaReference lambdatypeRef -> getLambdaType lambdatypeRef context
    match lambdatypeRef.Parameter with
        |Some parameter-> let parameterType=
                            match parameter with
                            |SimpleTypeReference simpleTypeReference -> GetSystemtypeFrom (ForgContext.lookup context simpleTypeReference).Value
                            |LambdaReference lambdatypeRef -> getLambdaType lambdatypeRef context
                          let lambdaType=typeof<ForgTypes.IForgFunc<_, _>>.GetGenericTypeDefinition()
                          lambdaType.MakeGenericType([|returnType ; parameterType|])
        |None -> 
              let lambdaType=typeof<IForgParameterlessFunc<_>>.GetGenericTypeDefinition()
              lambdaType.MakeGenericType([|returnType |])
              
let rec writeAssignment assignment (typeBuilder:TypeBuilder) (context:Context):List<Symbol>=   
        Console.WriteLine ("Creating " + assignment.Name)
        match assignment.Assignment with
        | ParameterlessAssignment(parameterlessassignment)->
            match parameterlessassignment with
             |ModuleKeyword ->
                let assemblyName = new AssemblyName ()
                assemblyName.Name <- assignment.Name
                let assemblyBuilder =  Thread.GetDomain().DefineDynamicAssembly(assemblyName,
                                                            AssemblyBuilderAccess.RunAndSave)
                       
                let assemblyName=if List.exists (fun x-> x.Name = "main") assignment.Where then
                                    assignment.Name+".exe"     
                                 else                 
                                    assignment.Name+".dll"               
                let modl = assemblyBuilder.DefineDynamicModule(assemblyName)
    
                let typeBuilder = modl.DefineType(assignment.Name,
                                                TypeAttributes.Public |||
                                                TypeAttributes.Class)

                
                        
                        
                //Ugh.. Runs inner expression with a ever growing context
                let mutable frame = []
                let mutable newcontext = context
                for innerAssignment in assignment.Where do
                  newcontext<-ForgContext.pushFrame newcontext frame
                  let symbols =writeAssignment innerAssignment typeBuilder newcontext
                  newcontext<-ForgContext.popFrame(newcontext)
                  for symbol in symbols do
                    frame<-(List.append [symbol] frame)
                    
                    
                let context=ForgContext.pushFrame context frame
                if List.exists (fun x-> x.Name = "main") assignment.Where then
                        createMainFunc typeBuilder assemblyBuilder context|> ignore
                        
                let created=typeBuilder.CreateType()
                if List.exists (fun x-> x.Name = "main") assignment.Where then
                        assemblyBuilder.Save( assignment.Name+".exe")
                else
                        assemblyBuilder.Save( assignment.Name+".dll")
                [{SymbolName=created.Name;Namespace=[];Ref=SystemType created}]
                
             | Expression expression -> 
                let nestedTypeBuilder=typeBuilder.DefineNestedType(assignment.Name,TypeAttributes.NestedPublic)//TODO  implement interface
                
                
                //Ugh.. Runs inner expression with a ever growing context
                let mutable frame = []
                let mutable newcontext = context
                for innerAssignment in assignment.Where do
                  newcontext<-ForgContext.pushFrame newcontext frame
                  let symbols =writeAssignment innerAssignment nestedTypeBuilder newcontext
                  newcontext<-ForgContext.popFrame(newcontext)
                  for symbol in symbols do
                    frame<-(List.append [symbol] frame)
                  
                let context=ForgContext.pushFrame context frame
                
                let constructorBuilder=nestedTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||] )
                let ilGenerator=constructorBuilder.GetILGenerator()
                ilGenerator.Emit(OpCodes.Ldarg_0)
                ilGenerator.Emit(OpCodes.Call,typeof<Object>.GetConstructor(Array.Empty()))
                ilGenerator.Emit(OpCodes.Ret)
                
                let methodBuilder = nestedTypeBuilder.DefineMethod("Execute",  MethodAttributes.HideBySig ||| MethodAttributes.Public,(getTypeOf expression context), [||] )
                
                let ilGenerator = methodBuilder.GetILGenerator() 
                writeExpression expression ilGenerator context typeBuilder
                ilGenerator.Emit(OpCodes.Ret)
                let created = nestedTypeBuilder.CreateType()
                [ {SymbolName=created.Name;Namespace=[];Ref=SystemType created}]
                
        | FunctionAssignment functionAssignment -> 
        
            let nestedTypeBuilder=typeBuilder.DefineNestedType(assignment.Name,TypeAttributes.NestedPublic)//TODO  implement interface

            //Ugh.. Runs inner expression with a ever growing context
            let mutable frame = []
            let mutable newcontext = context
            for innerAssignment in assignment.Where do
              newcontext<-ForgContext.pushFrame newcontext frame
              let symbols =writeAssignment innerAssignment nestedTypeBuilder newcontext
              newcontext<-ForgContext.popFrame(newcontext)
              for symbol in symbols do
                frame<-(List.append [symbol] frame)
                    
            let context=ForgContext.pushFrame context frame
                
            let constructorBuilder=nestedTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [||] )
            let ilGenerator=constructorBuilder.GetILGenerator()
            ilGenerator.Emit(OpCodes.Ldarg_0)
            ilGenerator.Emit(OpCodes.Call,typeof<Object>.GetConstructor(Array.Empty()))
            ilGenerator.Emit(OpCodes.Ret)
            let argtype= match functionAssignment.Parameter.TypeReference with
                         |Some typeRef ->
                            match typeRef with
                            |SimpleTypeReference simpleTypeReference -> GetSystemtypeFrom (ForgContext.lookup context simpleTypeReference).Value
                            |LambdaReference lambdatypeRef -> getLambdaType lambdatypeRef context
                         | None -> raise (NotImplementedException "TODO")

            let context=ForgContext.pushFrame context [{SymbolName = functionAssignment.Parameter.Name; Namespace=[]; Ref= Parameter argtype}]
            
            let methodBuilder = nestedTypeBuilder.DefineMethod("Execute",  MethodAttributes.HideBySig ||| MethodAttributes.Public,(getTypeOf functionAssignment.Expression context), [|argtype|] )
            
            let ilGenerator = methodBuilder.GetILGenerator() 
            writeExpression functionAssignment.Expression ilGenerator context typeBuilder
            ilGenerator.Emit(OpCodes.Ret)
            let created = nestedTypeBuilder.CreateType()
            [ {SymbolName=created.Name;Namespace=[];Ref=SystemType created}]
        | TypeDeclaration typedeclaration ->
            let nestedTypeBuilder=typeBuilder.DefineNestedType(assignment.Name,TypeAttributes.NestedPublic)//TODO  implement interface
            writeType typedeclaration nestedTypeBuilder context
            let created = nestedTypeBuilder.CreateType()
            [ {SymbolName=created.Name;Namespace=[];Ref=SystemType created}]
        | GenericTypeDeclaration generictypedecl -> []

let push (code:List<FullAssignment>) (context:Context):unit=  
    Console.WriteLine code
    for assignment in code do
        writeAssignment assignment null context|>ignore
    Console.WriteLine "Done"