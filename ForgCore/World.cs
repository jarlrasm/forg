using System;
using Microsoft.FSharp.Collections;

namespace ForgCore
{
    public class Core : ForgTypes.IForgModule
    {
        public Ast.FullAssignment AST => new Ast.FullAssignment("Core",Ast.Assignment.NewParameterlessAssignment(Ast.ParameterlessAssignment.ModuleKeyword), FSharpList<Ast.FullAssignment>.Empty);

        public class World : ForgTypes.IForgType
        {
            public Ast.FullAssignment AST => new Ast.FullAssignment("World",Ast.Assignment.NewTypeDeclaration(Ast.TypeDeclaration.Primitive), FSharpList<Ast.FullAssignment>.Empty); 
        }
        internal class worldfunc<T> : ForgTypes.IForgFunc<T,World>
        {
            private readonly Func<World, T> _sysFunc;
            public worldfunc(Func <World,T> sysFunc)
            {
                _sysFunc = sysFunc;
            }

            public Ast.FullAssignment AST => throw new NotImplementedException();

            public T Execute(World world)
            {
                if (world == null) throw new ArgumentNullException(nameof(world));
                return _sysFunc(world);

            }

        }
    }
}