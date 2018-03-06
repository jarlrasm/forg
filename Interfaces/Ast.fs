module Ast

type Reference = {Name:string; Namespace:List<string>}
type Parameter = 
    {Name : string;
     TypeReference : Option<Reference>}
     
type FunctionCall = {Function:Expression;Argument:Option<Expression>}
and Expression = FunctionCall of FunctionCall|Reference of Reference|StringLiteral of string

type FunctionAssignment = 
    {Parameter : Parameter;
     Expression : Expression}

type ParameterlessAssignment = 
    | ModuleKeyword
    | Expression of Expression

type DataType =  List<Parameter>

type Atom={Name:string}

type AlgebraicType = List<TypeDeclaration>
and TypeDeclaration = 
    |Primitive 
    |Atom of Atom
    |Algebraic of AlgebraicType
    |Data of DataType
    
type GenericTypeDeclaration =
    {Parameters : List<Parameter>;
     TypeDeclaration : TypeDeclaration}

type Assignment = 
    | FunctionAssignment of FunctionAssignment
    | ParameterlessAssignment of ParameterlessAssignment
    | TypeDeclaration of TypeDeclaration
    | GenericTypeDeclaration of GenericTypeDeclaration

type FullAssignment = 
    {Name : string;
     Assignment : Assignment;
     Where : List<FullAssignment>}