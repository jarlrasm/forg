using System;
using Microsoft.FSharp.Collections;

namespace ForgCore
{
    public class Debug : ForgTypes.IForgModule
    {
        public Ast.FullAssignment AST  => new Ast.FullAssignment("Debug",Ast.Assignment.NewParameterlessAssignment(Ast.ParameterlessAssignment.ModuleKeyword), FSharpList<Ast.FullAssignment>.Empty);

        public class print : ForgTypes.IForgFunc<ForgTypes.IForgFunc<Primitives.String,Core.World>, Primitives.String>
        {
            private bool _ran;


            public Ast.FullAssignment AST => throw new NotImplementedException();

            public void Execute(Primitives.String content)
            {
                Result = new Core.worldfunc<Primitives.String>(world =>
                {
                    Console.WriteLine(content.Value);
                    return content;
                });
                _ran = true;
            }

            public bool HasResult => _ran;
            public ForgTypes.IForgFunc<Primitives.String,Core.World> Result { get; private set; }
        }
        
    }
}