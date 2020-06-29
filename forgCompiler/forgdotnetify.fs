module ForgDotNetify
open Ast
open NetAst

exception Corrupt of string 


let visitTypeReference (typeref:ParameterType):TypeReference =
    match typeref with
    | SimpleTypeReference s->
        {
                NameSpace = ""; //TODO
                Name= s.Name
                GenericArguments = []
        }
    //TODO
   
let lambdifyType t=
    {
        NameSpace = "ForgTypes"; //TODO
        Name= "IForgLambda"
        GenericArguments = [t]
    }

let visitArgument (argument:Parameter) : ArgumentDeclaration=
    match argument.TypeReference with
    | Some t->
            {
                Name=argument.Name;
                ArgumentType=lambdifyType (visitTypeReference t) ;
            }
    | _ ->
            {
                Name=argument.Name;
                ArgumentType= {
                                 NameSpace = "System"; 
                                 Name= "Object"
                                 GenericArguments = []
                         }
            }
    
let visitConstructor  parameters =
    {
        Implementation= 
                        {
                            Arguments=List.map visitArgument parameters;
                            Code =[];
                        }
    }
    
let visitFunction  name returntype =
    match returntype with
    |Some t ->
        
        {
            Name = name;
            Implementation= 
                            {
                                Arguments=[ ];
                                Code =[];
                            };
            Returns =  visitTypeReference t
        }
     | _-> raise (Corrupt "No returntype") 
 
let rec visitFullAssignment (code:FullAssignment) :ClassDeclaration = 
    match code.Assignment with
        |FunctionAssignment f->
            {
                Name = code.Name;
                GenericArguments=[];
                Interfaces= [];
                InnerClasses =  List.map visitFullAssignment code.Where;
                Properties= [];
                Constructor= Some( visitConstructor [f.Parameter]);
                Functions= [visitFunction "Execute" f.TypeReference]
            }
        |ModuleAssignment ->
            {
                Name = code.Name;
                GenericArguments=[];
                Interfaces= [];
                InnerClasses =  List.map visitFullAssignment code.Where;
                Properties= [];
                Constructor= None;
                Functions= []
            }
        |ValueAssignment v->
            {
                Name = code.Name;
                GenericArguments=[];
                Interfaces= [];
                InnerClasses =  List.map visitFullAssignment code.Where;
                Properties= [];
                Constructor= Some( visitConstructor []);
                Functions= [visitFunction "Execute" v.TypeReference]
            }
        |TypeDeclaration t->
            {
                Name = code.Name;
                GenericArguments=[];
                Interfaces= [];
                InnerClasses =  List.map visitFullAssignment code.Where;
                Properties= [];
                Constructor= Some( visitConstructor []);
                Functions= []
            }

let dotNetify (code:List<FullAssignment> ) : (List<ClassDeclaration>) = 
    (List.map  visitFullAssignment code)