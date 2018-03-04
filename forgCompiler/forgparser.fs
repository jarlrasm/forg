module ForgParser

open FParsec
open Ast

type UserState = unit

type Parser<'t> = Parser<'t, UserState>



let moduleKeyword = pstring "module"
let whereKeyword = pstring "where"
let equalityKeyword = pstring "="

let name : Parser<string> = 
    let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
    let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'|| c = '-'
    identifier (IdentifierOptions(
                    isAsciiIdStart = isAsciiIdStart,
                    isAsciiIdContinue = isAsciiIdContinue,
                    normalization = System.Text.NormalizationForm.FormKC,
                    normalizeBeforeValidation = true,
                    allowAllNonAsciiCharsInPreCheck = true))
let stringLiteral =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)

let expression, expressionImplementation= createParserForwardedToRef()
let functioncall:Parser<FunctionCall> = (pipe2 (spaces>>. pstring "(" >>. spaces >>. expression .>> spaces) ((opt expression) .>> spaces .>> pstring ")" .>> spaces)  (fun f arg-> 
    {Function = f; Argument=arg}))
    
let reference: Parser<Reference> =
    spaces >>. (sepBy1 name (pstring ".") )|>> (fun x -> {Name=List.rev(x).Head; Namespace =List.rev(x).Tail|>List.rev})

do expressionImplementation :=   (functioncall |>> fun x -> Expression.FunctionCall x)<|>(reference |>> fun x -> Expression.Reference x)<|>(stringLiteral |>> fun x -> Expression.StringLiteral x)

let assignment, assignmentImplementation= createParserForwardedToRef()
let whereblock : Parser<List<FullAssignment>> = 
    whereKeyword >>. spaces >>. (pstring "{") >>. spaces >>. many assignment 
    .>> spaces .>> (pstring "}") .>> spaces

let parameter : Parser<Parameter> =(pipe2 name (spaces >>. pstring "::" >>. reference) (fun name typereference -> {Name = name; TypeReference=Some typereference})) <|> (name |>> (fun x -> {Name = x; TypeReference=None})) 
let functionassignment : Parser<Assignment> = 
    (pipe2 parameter (spaces >>. equalityKeyword >>. spaces >>. expression) (fun arg exp -> 
         FunctionAssignment {Parameter = arg;
                             Expression = exp}))

let parameterlessassignment : Parser<Assignment> = 
    equalityKeyword >>. spaces 
    >>. ((moduleKeyword 
          |>> (fun x -> 
          ParameterlessAssignment ParameterlessAssignment.ModuleKeyword)) 
         <|> (expression 
              |>> (fun x -> 
              ParameterlessAssignment(ParameterlessAssignment.Expression x))))

do assignmentImplementation := (pipe3 name 
                                    (spaces1 
                                     >>. (parameterlessassignment 
                                          <|> functionassignment)) 
                                    (spaces >>. opt whereblock) (fun name assignment where -> 
                                    {FullAssignment.Name = name;
                                     FullAssignment.Assignment = assignment;
                                     FullAssignment.Where = 
                                         match where with
                                         | Some x -> x
                                         | None -> []}))

let parser : Parser<List<FullAssignment>> = many assignment