﻿using System;
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

            public ForgTypes.IForgFunc<Primitives.String, Core.World> Execute(Primitives.String content)
            {
                return new Core.worldfunc<Primitives.String>(world =>
                {
                    Console.WriteLine(content.Value);
                    return content;
                });
            }

            public bool HasResult => _ran;
            public ForgTypes.IForgFunc<Primitives.String,Core.World> Result { get; private set; }
        }
        
    }
}