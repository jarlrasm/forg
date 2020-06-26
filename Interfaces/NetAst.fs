module NetAst



type TypeReference =
    {
        NameSpace : string;
        Name : string;
        GenericArguments: List<TypeReference>;
    }
type PropertyDeclaration =
    {
        Name : string;
        Type : TypeReference;
    }

type OpCode = NoOp

type OpCodeArgument =
 IntArg of int
 |StringArg of string
 
type Code=
    {
        OpCode : OpCode;
        Arguments : List<OpCodeArgument>
    }
type ArgumentDeclarations=
    {
        Name : string;
        ArgumentType :TypeReference
    }
type FunctionImplementation =
    {
        Arguments:List<ArgumentDeclarations>;
        Code : List<Code>;
    }
type ConstructorDeclaration =
    {
        Implementation : FunctionImplementation;
    }
type NamedFunctionDeclaration =
    {
        Name : string;
        Implementation : FunctionImplementation;
    }
type ClassDeclaration =
    {
        Name : string;
        GenericArguments:  List<TypeReference>;
        Interfaces: List<TypeReference>;
        InnerClasses :  List<ClassDeclaration>;
        Properties: List<PropertyDeclaration>;
        Constructor: Option<ConstructorDeclaration>;
        Functions: List<NamedFunctionDeclaration>;
    }