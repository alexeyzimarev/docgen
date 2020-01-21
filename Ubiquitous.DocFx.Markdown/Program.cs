using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using Buildalyzer;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Serilog;
using Ubiquitous.DocFx.Markdown.Visitors;

namespace Ubiquitous.DocFx.Markdown
{
    class Program
    {
        static void Main(string[] args)
        {
            Log.Logger = new LoggerConfiguration().WriteTo.Console().MinimumLevel.Verbose().CreateLogger();
            
            var manager  = new AnalyzerManager();
            var analyzer = manager.GetProject("/Users/alexey/github/RestSharp/src/RestSharp/RestSharp.csproj");

            var result = analyzer.Build();

            var csFiles = result.First().SourceFiles.Where(x => x.EndsWith(".cs"));

            var trees = csFiles
                .Select(x => SyntaxFactory.ParseSyntaxTree(File.ReadAllText(x), new CSharpParseOptions(), x));
            
            var defaultReferences = new List<MetadataReference> { MetadataReference.CreateFromFile(typeof(object).Assembly.Location), MetadataReference.CreateFromFile(typeof(EditorBrowsableAttribute).Assembly.Location) };
            var compilation = CSharpCompilation.Create(
                "docs.dll",
                options: new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary),
                syntaxTrees: trees,
                references: defaultReferences);

            var assembly = compilation.Assembly;
            var visitor  = new MetadataSymbolVisitor(new CsMetadataGenerator(), compilation, new ExtractMetadataOptions());
            var metadata = assembly.Accept(visitor);
        }
    }
}