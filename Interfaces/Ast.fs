module Ast

type Reference = {Name:string; Namespace:List<string>}
type LambdaReference = {Parameter: Option<ParameterType>; Return:ParameterType}
and ParameterType =
    |SimpleTypeReference of Reference
    |LambdaReference of LambdaReference
    
type Parameter = 
    {Name : string;
     TypeReference : Option<ParameterType>}
     
type FunctionCall = {Function:Expression;Argument:Option<Expression>}
and NameWithValue = {Name:string; Value:Expression}
and Constructor= {Assignments:List<NameWithValue>;TypeReference: Option<Reference>}
and SimpleDestructor= {DataObject:Expression ;Name:string;}
and Lambda= {Parameter:Option<Parameter>; LambdaExpression:Expression;}
and Expression = 
    FunctionCall of FunctionCall
    |Reference of Reference
    |StringLiteral of string
    |IntLiteral of int
    |Constructor of Constructor
    |SimpleDestructor of SimpleDestructor
    |Lambda of Lambda

type FunctionAssignment = 
    {Parameter : Parameter;
     Expression : Expression}

type ParameterlessAssignment = 
    | ModuleKeyword
    | Expression of Expression


type Atom={Name:string}

type TypeOption =
    |Atom of Atom
    |Parameter of Parameter
    
type AlgebraicType = List<TypeOption>

type DataType =  List<Parameter>

type TypeDeclaration = 
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