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
type TypeAssignment() = class end

type Assignment = 
    | FunctionAssignment of FunctionAssignment
    | ParameterlessAssignment of ParameterlessAssignment
    | TypeAssignment of TypeAssignment

type FullAssignment = 
    {Name : string;
     Assignment : Assignment;
     Where : List<FullAssignment>}