using Microsoft.FSharp.Collections;

namespace ForgCore
{
    public class  Primitives : ForgTypes.IForgModule
    {
        public Ast.FullAssignment AST => new Ast.FullAssignment("Primitives",Ast.Assignment.NewParameterlessAssignment(Ast.ParameterlessAssignment.ModuleKeyword), FSharpList<Ast.FullAssignment>.Empty);

        public class String : ForgTypes.IForgPrimitive<string>
        {
            public String(string value)
            {
                Value = value;
            }

            public string Value { get; }
            public Ast.FullAssignment AST => new Ast.FullAssignment("String",Ast.Assignment.NewTypeDeclaration(Ast.TypeDeclaration.Primitive), FSharpList<Ast.FullAssignment>.Empty);
        }
    }
}