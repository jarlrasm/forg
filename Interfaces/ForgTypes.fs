module ForgTypes

open Ast
open System

type IForgType = interface end
    
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
    abstract member Execute:unit->'output
    
type IForgParameterlessFunc<'output> =
    inherit IForgLambda<'output>
    
type IForgFunc<'output, 'input> =
    inherit IForgLambda<'output>
    