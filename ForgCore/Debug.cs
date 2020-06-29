using System;
using System.Collections.Generic;
using Microsoft.FSharp.Collections;

namespace ForgCore
{

    public class Debug : ForgTypes.IForgModule
    {
        public class print : ForgTypes.IForgFunc<ForgTypes.IForgFunc<Primitives.String,Core.World>, Primitives.String>
        {
            private  ForgTypes.IForgLambda<Primitives.String> _parameter;
            public print(ForgTypes.IForgLambda<Primitives.String> parameter)
            {
                _parameter = parameter;
            }

            public ForgTypes.IForgFunc<Primitives.String, Core.World> Execute()
            {
                return new Core.worldfunc<Primitives.String>(world =>
                {
                    Console.WriteLine(_parameter.Execute().Value);
                    return _parameter.Execute();
                });
            }

        }
        
    }
}