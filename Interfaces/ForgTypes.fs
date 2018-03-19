module ForgTypes

open Ast
open System

type IForgType =
    abstract member AST:FullAssignment 
    
type IForgModule =
    inherit IForgType
    
type IForgPrimitive<'a> =
    inherit IForgType
    abstract member Value: 'a
    
type IForgDataType =
    inherit IForgType
    
type IForgAlgebraicType =
    inherit IForgType
    abstract member Value: IForgType
    
type IForgLambda<'output> =
    inherit IForgType
    
type IForgParameterlessFunc<'output> =
    inherit IForgLambda<'output>
    abstract member Execute:unit->'output
    
type IForgFunc<'output, 'input> =
    inherit IForgLambda<'output>
    abstract member Execute:'input->'output
    