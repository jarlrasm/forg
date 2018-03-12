using System;
using System.Collections.Generic;
using System.Linq;

namespace ForgCore
{
    public class ForgLambda<TOutput, TInput> : ForgTypes.IForgFunc<TOutput, TInput>
    {
        public ForgLambda(Func<ForgClosure<TInput>, TOutput> lambdafunc, string paramName,
            IEnumerable<KeyValuePair<string, object>> closed)
        {
            Lambdafunc = lambdafunc;
            ParamName = paramName;
            Closed = closed;
        }

        public Func<ForgClosure<TInput>, TOutput> Lambdafunc { get; }
        public string ParamName { get; }
        public IEnumerable<KeyValuePair<string, object>> Closed { get; }


        public Ast.FullAssignment AST => throw new NotImplementedException();

        public void Execute(TInput input)
        {
            Result = Lambdafunc(new ForgClosure<TInput>(Closed.Concat(new[]
                {new KeyValuePair<string, object>(ParamName, input)})));
            HasResult = true;
        }

        public bool HasResult { get; private set; }
        public TOutput Result { get; private set; }

        public class ForgClosure<T> : ForgTypes.IForgType
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