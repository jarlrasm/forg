module TypeInference
open Ast
open ForgContext

exception Corrupt of string 

let rec typeExpression expr=
    match expr with
    |FunctionCall f -> 
            None
    |Reference r -> 
            None
    |StringLiteral s -> 
            Some (SimpleTypeReference {Name="String";GenericType=None;Namespace=["Primitives"]})
    |IntLiteral i ->  
            Some (SimpleTypeReference {Name="Int";GenericType=None;Namespace=["Primitives"]})
    |ListLiteral l -> 
            None
    |Constructor c -> 
            None
    |SimpleDestructor sd -> 
            None
    |Lambda l -> 
            None


let typeFunction (func:FunctionAssignment):FunctionAssignment =
    match func.TypeReference with
    |None -> {func with TypeReference = typeExpression func.Expression}
    | _ -> func
    
let typeValue (value:ValueAssignment):ValueAssignment=
    match value.TypeReference with
    |None -> {value with TypeReference = typeExpression value.Expression}
    |_ -> value
   
let rec visitFullAssignment (code:FullAssignment):FullAssignment = 
    match code.Assignment with
        |FunctionAssignment f->
            {code with Where = List.map visitFullAssignment code.Where; Assignment=FunctionAssignment (typeFunction f)} 
        |ModuleAssignment ->
            {code with Where = List.map visitFullAssignment code.Where} 
        |ValueAssignment v->
            {code with Where = List.map visitFullAssignment code.Where; Assignment=ValueAssignment(typeValue v)} 
        |TypeDeclaration t->
                code //TODO

let findTypes (code:List<FullAssignment> ) (context : Context) : (List<FullAssignment>) = 
    (List.map  visitFullAssignment code)
