module ForgParser

open FParsec
open Ast
open System

type UserState = unit

type Parser<'t> = Parser<'t, UserState>



let moduleKeyword = pstring "module"
let whereKeyword = pstring "where"
let typeKeyword = pstring "type"
let funKeyword = pstring "fun"

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
let intLiteral = pint32
    

let expression, expressionImplementation= createParserForwardedToRef()

let listLiteral =
    (pstring "[")>>. (sepBy expression (pstring ",")) .>> (pstring "]")|>> fun x -> {Expressions =x} 
    
let functioncall:Parser<FunctionCall> = (pipe2 (pstring "(" >>.  expression ) ((opt expression) .>> pstring ")")  (fun f arg-> 
    {Function = f; Argument=arg}))
 
     
let namespaces =
    (many1 name  .>> (pstring ".")) 
    
let reference: Parser<Reference> =
    (attempt  (pipe2 namespaces name (fun namespaces name  -> {Name=name; Namespace =namespaces})))
     <|> (name |>> fun x-> {Name=x; Namespace =[]})

let genericReference: Parser<GenericTypeReference> =
     (pipe2
            reference
            ((pstring "<") >>. reference .>> (pstring ">"))
            (fun reference genericreference -> {Reference=reference;GenericReference =genericreference;}))
    
let constructorAssignment:Parser<NameWithValue> =
    pipe2 (spaces >>. name ) ((spaces >>. (pstring "="))>>. spaces >>. expression)(fun name exp -> {Name=name; Value =exp})
        
let constructor: Parser<Constructor> =
    (pipe2 ( pstring "{" >>.(sepBy1 constructorAssignment (pstring ",")).>> pstring "}") (spaces >>. (opt (pstring "::" >>. reference))) (fun assignments typeReference -> {Assignments=assignments; TypeReference =typeReference}))
    
let simpledestructor: Parser<SimpleDestructor> =
    (pipe2 name (spaces  >>.  (pstring "#") >>. expression)  (fun name ex -> {DataObject=ex; Name =name}))
    
let parmeterlesslambda: Parser<Lambda> =
    (funKeyword >>. spaces  >>. (pstring "->") >>.  spaces  >>. expression) |>> 
        (fun expr -> {Parameter=None; LambdaExpression=expr})
    
let parameter, parameterImplementation= createParserForwardedToRef()

let lambda: Parser<Lambda> =
    (pipe2 
        (funKeyword >>. spaces  >>. parameter)
        (spaces >>. (pstring "->") >>. spaces  >>. expression) 
        (fun param expr -> {Parameter= Some param; LambdaExpression=expr}))
    
do expressionImplementation := 
    spaces  >>. 
    (attempt simpledestructor |>> fun x -> Expression.SimpleDestructor x)
    <|>(attempt lambda |>> fun x -> Expression.Lambda x)
    <|>(parmeterlesslambda |>> fun x -> Expression.Lambda x)
    <|>(functioncall |>> fun x -> Expression.FunctionCall x)
    <|>(reference |>> fun x -> Expression.Reference x)
    <|>(stringLiteral |>> fun x -> Expression.StringLiteral x)
    <|>(intLiteral |>> fun x -> Expression.IntLiteral x)
    <|>(listLiteral |>> fun x -> Expression.ListLiteral x)
    <|>(constructor |>> fun x -> Expression.Constructor x)
    .>> spaces

let assignment, assignmentImplementation= createParserForwardedToRef()

let whereblock : Parser<List<FullAssignment>> = 
    whereKeyword >>. spaces >>. (pstring "[") >>. spaces >>. many assignment 
    .>> spaces .>> (pstring "]") .>> spaces
    
let parameterType, parameterTypenImplementation= createParserForwardedToRef()
let lambdaRef =
    spaces >>. pstring "(" >>. pipe2 
        (opt (spaces  >>. parameterType))
        (spaces >>. pstring "->" >>. spaces >>. parameterType .>>  spaces .>> pstring ")" ) 
        (fun para ret ->{Parameter= para; Return=ret})
    
do parameterTypenImplementation  :=
    spaces >>. (attempt reference |>> fun x -> SimpleTypeReference x)
    <|>(attempt genericReference |>> fun x -> GenericTypeReference x)
    <|>(lambdaRef |>> fun x -> LambdaReference x)
    
do parameterImplementation := 
    (pipe2 name (spaces >>. (opt (pstring "::" >>. parameterType)).>> spaces) (fun name typereference -> {Name = name; TypeReference=typereference})) <|> (name |>> (fun x -> {Name = x; TypeReference=None})) 

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


    
let datatypeparameter : Parser<Parameter> =(pipe2 name (spaces >>.  (pstring "::" >>. parameterType)) (fun name typereference -> {Name = name; TypeReference=Some typereference})) 

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