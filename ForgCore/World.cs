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
            private readonly World _world;
            private bool _ran=false;

            public worldfunc(Func <World,T> sysFunc)
            {
                _sysFunc = sysFunc;
            }

            public Ast.FullAssignment AST => throw new NotImplementedException();

            public void Execute(World world)
            {
                if (world == null) throw new ArgumentNullException(nameof(world));
                Result = _sysFunc(_world);
                _ran = true;

            }

            public bool HasResult => _ran;

            public T Result { get; private set; }
        }
    }
}