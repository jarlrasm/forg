using System;
using System.Collections.Generic;
using Microsoft.FSharp.Collections;

namespace ForgCore
{

    public class Debug : ForgTypes.IForgModule
    {
        public Ast.FullAssignment AST  => new Ast.FullAssignment("Debug",Ast.Assignment.NewParameterlessAssignment(Ast.ParameterlessAssignment.ModuleKeyword), FSharpList<Ast.FullAssignment>.Empty);

        public class print : ForgTypes.IForgFunc<ForgTypes.IForgFunc<Primitives.String,Core.World>, Primitives.String>
        {
            private  ForgTypes.IForgLambda<Primitives.String> _parameter;


            public ForgTypes.IForgFunc<Primitives.String, Core.World> Execute()
            {
                return new Core.worldfunc<Primitives.String>(world =>
                {
                    Console.WriteLine(_parameter.Execute().Value);
                    return _parameter.Execute();
                });
            }

            public ForgTypes.IForgLambda<Primitives.String> Parameter
            {
                get => _parameter;
                set => _parameter = value;
            }
        }
        
    }
}