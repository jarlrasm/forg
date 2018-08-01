using System.Linq;

namespace ForgCore
{
    public class List : ForgTypes.IForgModule
    {
        public class head<T> : ForgTypes.IForgFunc<T,Primitives.List<T>>
        {//TODO should return Maybe<T>
            public T Execute()
            {
                return Parameter.Execute().Value.First();
            }

            public ForgTypes.IForgLambda<Primitives.List<T>> Parameter { get; set; }
        }
    }
}