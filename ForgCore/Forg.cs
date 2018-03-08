﻿using System;
using Microsoft.FSharp.Collections;

namespace ForgCore
{
    public class Forg: ForgTypes.IForgModule
    {
        public Ast.FullAssignment AST => new Ast.FullAssignment("Forg",Ast.Assignment.NewParameterlessAssignment(Ast.ParameterlessAssignment.ModuleKeyword), FSharpList<Ast.FullAssignment>.Empty);

        public class Bool : ForgTypes.IForgAlgebraicType
        {
            private Bool(ForgTypes.IForgType value)
            {
                Value = value;
            }

            public static Bool New_true()
            {
                return new Bool(new Primitives.Atom("true"));
            }
            public static Bool New_false()
            {
                return new Bool(new Primitives.Atom("false"));
            }

            public Ast.FullAssignment AST => throw new NotImplementedException();

            public ForgTypes.IForgType Value { get; }
        }
    }
}