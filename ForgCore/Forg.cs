using System;
using System.Collections.Generic;
using System.Linq;
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

        public class ParameterlessLambda<TOutput> : ForgTypes.IForgParameterlessFunc<TOutput>
        {
            public ParameterlessLambda(Func<ForgClosure, TOutput> lambdafunc, string paramName,
                IEnumerable<KeyValuePair<string, object>> closed)
            {
                Lambdafunc = lambdafunc;
                ParamName = paramName;
                Closed = closed;
            }

            public Func<ForgClosure, TOutput> Lambdafunc { get; }
            public string ParamName { get; }
            public IEnumerable<KeyValuePair<string, object>> Closed { get; }


            public Ast.FullAssignment AST => throw new NotImplementedException();

            public void Execute()
            {
                Result = Lambdafunc(new ForgClosure(Closed));
                HasResult = true;
            }

            public bool HasResult { get; private set; }
            public TOutput Result { get; private set; }

        }

        public class Lambda<TOutput, TInput> : ForgTypes.IForgFunc<TOutput, TInput>
        {
            public Lambda(Func<ForgClosure, TOutput> lambdafunc, string paramName,
                IEnumerable<KeyValuePair<string, object>> closed)
            {
                Lambdafunc = lambdafunc;
                ParamName = paramName;
                Closed = closed;
            }

            public Func<ForgClosure, TOutput> Lambdafunc { get; }
            public string ParamName { get; }
            public IEnumerable<KeyValuePair<string, object>> Closed { get; }


            public Ast.FullAssignment AST => throw new NotImplementedException();

            public void Execute(TInput input)
            {
                Result = Lambdafunc(new ForgClosure(Closed.Concat(new[]
                    {new KeyValuePair<string, object>(ParamName, input)})));
                HasResult = true;
            }

            public bool HasResult { get; private set; }
            public TOutput Result { get; private set; }

        }
        public class ForgClosure : ForgTypes.IForgType
        {
            public ForgClosure(IEnumerable<KeyValuePair<string, object>> closed)
            {
                Closed = closed;
            }

            public IEnumerable<KeyValuePair<string, object>> Closed { get; }

            public Ast.FullAssignment AST => throw new NotImplementedException();

            public TItem Get<TItem>(string name)
            {
                return (TItem) Closed.First(x => x.Key == name).Value;
            }
        }
    }
}