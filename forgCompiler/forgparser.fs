module ForgParser

open FParsec
open Ast
open System

type UserState = unit

type Parser<'t> = Parser<'t, UserState>



let moduleKeyword = pstring "module"
let whereKeyword = pstring "where"
let typeKeyword = pstring "type"
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

let constructorAssignment:Parser<NameWithValue> =
    pipe2 (spaces >>. name ) ((spaces >>. (pstring "="))>>. spaces >>. expression)(fun name exp -> {Name=name; Value =exp})
        
let constructor: Parser<Constructor> =
    (pipe2 (spaces >>. pstring "{" >>.(sepBy1 constructorAssignment (pstring ",")).>> pstring "}") (spaces >>. (opt (pstring "::" >>. reference))) (fun assignments typeReference -> {Assignments=assignments; TypeReference =typeReference}))
    
let simpledestructor: Parser<SimpleDestructor> =
    (pipe2 (spaces >>. name) (spaces  >>.  (pstring "#") >>. expression)  (fun name ex -> {DataObject=ex; Name =name}))
    
do expressionImplementation := 
    (attempt simpledestructor |>> fun x -> Expression.SimpleDestructor x)
    <|>(attempt functioncall |>> fun x -> Expression.FunctionCall x)
    <|>(attempt reference |>> fun x -> Expression.Reference x)
    <|>(attempt stringLiteral |>> fun x -> Expression.StringLiteral x)
    <|>( constructor |>> fun x -> Expression.Constructor x)

let assignment, assignmentImplementation= createParserForwardedToRef()

let whereblock : Parser<List<FullAssignment>> = 
    whereKeyword >>. spaces >>. (pstring "[") >>. spaces >>. many assignment 
    .>> spaces .>> (pstring "]") .>> spaces

let parameter : Parser<Parameter> =(pipe2 name (spaces >>. (opt (pstring "::" >>. reference))) (fun name typereference -> {Name = name; TypeReference=typereference})) <|> (name |>> (fun x -> {Name = x; TypeReference=None})) 

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

let atom:Parser<Atom>=
     name .>> spaces|>> fun x-> {Name=x}


    
let datatypeparameter : Parser<Parameter> =(pipe2 name (spaces >>.  (pstring "::" >>. reference)) (fun name typereference -> {Name = name; TypeReference=Some typereference})) 

let typeoption: Parser<TypeOption> =
    (attempt datatypeparameter .>> spaces |>> (fun x-> Parameter  x)) <|> (atom |>> (fun x-> TypeOption.Atom x))
    
let algebraic  : Parser<AlgebraicType> = 
    many1 (pstring "|" >>. spaces >>. typeoption .>>spaces) 
let data  : Parser<DataType> = (sepBy1 (spaces >>.datatypeparameter) (pstring ","))
let typedeclaration=
    ((attempt(algebraic|>> (fun x -> TypeDeclaration.Algebraic x))) <|> (attempt (data|>> (fun x -> TypeDeclaration.Data x)))<|> (atom |>> fun x-> TypeDeclaration.Atom x)) 
                             
let typeassignment  : Parser<TypeDeclaration> = (spaces >>. equalityKeyword >>. spaces >>. typeKeyword >>. spaces >>. typedeclaration)

let generictypeassignment : Parser<GenericTypeDeclaration> = 
    (pipe2 (many parameter) (typeassignment) (fun arg dec -> 
         {Parameters = arg; TypeDeclaration = dec}))
  
let typedecl=  
    (typeassignment |>> fun x -> Assignment.TypeDeclaration x)                           
                             
let generictypedecl=  
    (generictypeassignment|>> fun x -> Assignment.GenericTypeDeclaration x)  
    
do assignmentImplementation := (pipe3 name 
                                    (spaces
                                     >>. ((attempt typedecl)
                                          <|> (attempt generictypedecl)
                                          <|> (attempt parameterlessassignment) 
                                          <|> functionassignment))
                                    (spaces >>. opt whereblock  ) (fun name assignment where -> 
                                    {FullAssignment.Name = name;
                                     FullAssignment.Assignment = assignment;
                                     FullAssignment.Where = 
                                         match where with
                                         | Some x -> x
                                         | None -> []}))

let parser : Parser<List<FullAssignment>> = many assignment