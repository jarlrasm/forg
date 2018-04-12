using System;
using System.Diagnostics.Eventing.Reader;
using Microsoft.FSharp.Collections;

namespace ForgCore
{
    public class Core : ForgTypes.IForgModule
    {
        public Ast.FullAssignment AST => new Ast.FullAssignment("Core",Ast.Assignment.NewParameterlessAssignment(Ast.ParameterlessAssignment.ModuleKeyword), FSharpList<Ast.FullAssignment>.Empty);


        public class WorldCreator : ForgTypes.IForgLambda<World>
        {
            public World Execute()
            {
                return new World();
                    
            }
        }
        public class World : ForgTypes.IForgType
        {
            public Ast.FullAssignment AST => new Ast.FullAssignment("World",Ast.Assignment.NewTypeDeclaration(Ast.TypeDeclaration.Primitive), FSharpList<Ast.FullAssignment>.Empty); 
        }
        internal class worldfunc<T> : ForgTypes.IForgFunc<T,World>
        {
            private readonly Func<World, T> _sysFunc;
            private ForgTypes.IForgLambda<World> _parameter;

            public worldfunc(Func <World,T> sysFunc)
            {
                _sysFunc = sysFunc;
            }

            public T Execute()
            {
                return _sysFunc(_parameter.Execute());

            }


            public ForgTypes.IForgLambda<World> Parameter
            {
                get => _parameter;
                set => _parameter = value;
            }

        }
    }
}