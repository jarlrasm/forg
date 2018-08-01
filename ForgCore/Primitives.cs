using System.Collections.Generic;
using Microsoft.FSharp.Collections;

namespace ForgCore
{
    public class  Primitives : ForgTypes.IForgModule
    {
        public class String : ForgTypes.IForgPrimitive<string>
        {
            public String(string value)
            {
                Value = value;
            }

            public string Value { get; }        }
        public class Int : ForgTypes.IForgPrimitive<int>
        {
            public Int(int value)
            {
                Value = value;
            }

            public int Value { get; }        } 
        public class Atom : ForgTypes.IForgPrimitive<string>
        {
            public Atom(string value)
            {
                Value = value;
            }

            public string Value { get; }        }
        
        public class List<T> : ForgTypes.IForgPrimitive<IEnumerable<T>>
        {
            public List(IEnumerable<T> value)
            {
                Value = value;
            }

            public IEnumerable<T> Value { get; }        }
    }
}