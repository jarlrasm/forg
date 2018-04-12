using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using Xunit;

namespace Tests
{
    public class Tests
    {

        static IEnumerable<string> SourceFiles=>Directory.GetFiles(SourcePath);

        private static string SourcePath => Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) + "/source";

        public static IEnumerable<object[]> ForgFiles()
        {
            return SourceFiles
                .Select(x => new object[] {x});
        }
        public static IEnumerable<object[]> ProgramOutputs()
        {
            yield return new object[]{"helloworld.forg","HelloWorld.exe","Hello world!"+Environment.NewLine};
            yield return new object[]{"hellosomeone.forg","HelloSomeone.exe","Yo"+Environment.NewLine};
            yield return new object[]{"lambda.forg","LambdaTest.exe","Hello lambda!"+Environment.NewLine};
        }


        private static string GetSourceFileName(string name) =>
            SourcePath + "/" +name;

        private int CompileFile(string fileName) => Program.main(new[] {fileName});
            
        [Theory]
        [MemberData(nameof(ForgFiles))]
        public void CompileFilesWithoutErrors(string filename)
        {
            Assert.Equal(0,CompileFile(filename));
        }
        
        [Theory]
        [MemberData(nameof(ProgramOutputs))]
        public void RunPrograms(string sourceFile,string exefile, string expectedOutput)
        {
            CompileFile(GetSourceFileName(sourceFile));
            var result = ExecuteProgramAndReturnStdOut(exefile);
            Assert.Equal(expectedOutput,result);
            
        }

        private static string ExecuteProgramAndReturnStdOut(string filename)
        {
            ProcessStartInfo start = new ProcessStartInfo();
            start.FileName = filename;
            start.UseShellExecute = false;
            start.RedirectStandardOutput = true;
            string result = "";
            using (Process process = Process.Start(start))
            {
                using (StreamReader reader = process.StandardOutput)
                {
                    result += reader.ReadToEnd();
                }

                process.WaitForExit();
            }

            return result;
        }
    }
}