using System.Linq;

namespace ForgCore
{
    public class List : ForgTypes.IForgModule
    {
        public class head<T> : ForgTypes.IForgFunc<T,Primitives.List<T>>
        {
            private ForgTypes.IForgLambda<Primitives.List<T>> _list; //TODO should return Maybe<T>
            public head(ForgTypes.IForgLambda<Primitives.List<T>> list)
            {
                _list = list;
            }
            public T Execute()
            {
                return _list.Execute().Value.First();
            }

        }
    }
}