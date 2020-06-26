module ForgDotNetify
open Ast
open NetAst
let visitConstructor  code =
    {
        Implementation= 
                        {
                            Arguments=[];
                            Code =[];
                        }
    }
let rec visitFullAssignment (code:FullAssignment) :ClassDeclaration = 
    match code.Assignment with
        |FunctionAssignment f->
            {
                Name = code.Name;
                GenericArguments=[];
                Interfaces= [];
                InnerClasses =  List.map visitFullAssignment code.Where;
                Properties= [];
                Constructor= Some( visitConstructor code);
                Functions= []
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
                Constructor= Some( visitConstructor code);
                Functions= []
            }
        |TypeDeclaration t->
            {
                Name = code.Name;
                GenericArguments=[];
                Interfaces= [];
                InnerClasses =  List.map visitFullAssignment code.Where;
                Properties= [];
                Constructor= Some( visitConstructor code);
                Functions= []
            }

let dotNetify (code:List<FullAssignment> ) : (List<ClassDeclaration>) = 
    (List.map  visitFullAssignment code)