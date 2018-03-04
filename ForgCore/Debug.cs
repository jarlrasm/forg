using System;
using Microsoft.FSharp.Collections;

namespace ForgCore
{
    public class Debug : ForgTypes.IForgModule
    {
        public Ast.FullAssignment AST  => new Ast.FullAssignment("Debug",Ast.Assignment.NewParameterlessAssignment(Ast.ParameterlessAssignment.ModuleKeyword), FSharpList<Ast.FullAssignment>.Empty);

        public class print : ForgTypes.IForgFunc<ForgTypes.IForgFunc<string,Core.World>, string>
        {
            private bool _ran;


            public Ast.FullAssignment AST => throw new NotImplementedException();

            public void Execute(string content)
            {
                Result = new Core.worldfunc<string>(world =>
                {
                    Console.WriteLine(content);
                    return content;
                });

            }

            public bool HasResult => _ran;
            public ForgTypes.IForgFunc<string,Core.World> Result { get; private set; }
        }
        
    }
}