using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FSharp.Quotations.Compiler.Tests.CSharp
{
    public class CSharpClass
    {
        public static int StaticField = 42;

        public static int OptionalArgument(int x, int y = -1)
        {
            return x + y;
        }
    }
}
