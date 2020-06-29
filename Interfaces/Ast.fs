module Ast

type Reference = {Name:string;GenericType:Option<ParameterType>;Namespace:List<string>}
and GenericTypeReference = {Reference:Reference; GenericReference:Reference;}
and LambdaReference = {Parameter: Option<ParameterType>; Return:ParameterType}
and ParameterType =
    |SimpleTypeReference of Reference
    |LambdaReference of LambdaReference
    |GenericTypeReference of GenericTypeReference
    
type Parameter = 
    {Name : string;
     TypeReference : Option<ParameterType>}
     
type FunctionCall = {Function:Expression;Argument:Option<Expression>}
and NameWithValue = {Name:string; Value:Expression}
and Constructor= {Assignments:List<NameWithValue>;TypeReference: Option<Reference>}
and SimpleDestructor= {DataObject:Expression ;Name:string;}
and Lambda= {Parameter:Option<Parameter>; LambdaExpression:Expression;}
and ListLiteral= {Expressions:List<Expression>}
and Expression = 
    FunctionCall of FunctionCall
    |Reference of Reference
    |StringLiteral of string
    |IntLiteral of int
    |ListLiteral of ListLiteral
    |Constructor of Constructor
    |SimpleDestructor of SimpleDestructor
    |Lambda of Lambda

type FunctionAssignment = 
    {Parameter : Parameter;
     Expression : Expression;
     TypeReference : Option<ParameterType>;}



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

type ValueAssignment =
    {
         Expression : Expression;
         TypeReference : Option<ParameterType>;
    }

type Assignment = 
    | FunctionAssignment of FunctionAssignment
    | ValueAssignment of ValueAssignment
    | TypeDeclaration of TypeDeclaration
    | ModuleAssignment

type FullAssignment = 
    {Name : string;
     Assignment : Assignment;
     GenericParameter : Option<string>;
     Where : List<FullAssignment>}