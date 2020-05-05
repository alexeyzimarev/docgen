using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Buildalyzer;
using DocGen.Metadata.Models;
using DocGen.Metadata.Roslyn;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;

namespace DocGen.Metadata
{
    public class ProjectAnalyzer : IDisposable
    {
        readonly IProjectAnalyzer[] _analyzers;

        ProjectAnalyzer(AnalyzerManager manager, IEnumerable<string> projectFiles)
            => _analyzers = projectFiles.Select(manager.GetProject).ToArray();

        ProjectAnalyzer(AnalyzerManager manager)
            => _analyzers = manager.Projects.Select(x => x.Value).ToArray();

        public static ProjectAnalyzer ForProjects(
            IEnumerable<string> projectFiles,
            ILoggerFactory? loggerFactory = null
        )
        {
            var log     = loggerFactory ?? NullLoggerFactory.Instance;
            var manager = new AnalyzerManager(new AnalyzerManagerOptions {LoggerFactory = log});
            return new ProjectAnalyzer(manager, projectFiles);
        }

        public static ProjectAnalyzer ForSolution(
            string solutionFile,
            ILoggerFactory? loggerFactory = null
        )
        {
            var log = loggerFactory ?? NullLoggerFactory.Instance;

            var manager = new AnalyzerManager(
                solutionFile,
                new AnalyzerManagerOptions {LoggerFactory = log}
            );
            return new ProjectAnalyzer(manager);
        }

        public async Task Analyze()
        {
            _compilations = await Task.WhenAll(_analyzers.Select(AnalyzeProject));

            foreach (var (_, compilation) in _compilations)
                _extensionMethods.Add(compilation, compilation.GetExtensionMethods());
        }

        static async Task<(string File, Compilation Compilation)> AnalyzeProject(
            IProjectAnalyzer projectAnalyzer
        )
        {
            var result = projectAnalyzer.Build();

            var csFiles = result.First().SourceFiles.Where(x => x.EndsWith(".cs"));

            var trees = await Task.WhenAll(
                csFiles.Select(
                    async x => SyntaxFactory.ParseSyntaxTree(
                        await File.ReadAllTextAsync(x),
                        new CSharpParseOptions(),
                        x
                    )
                )
            );

            var defaultReferences = new List<MetadataReference>
            {
                MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
                MetadataReference.CreateFromFile(
                    typeof(EditorBrowsableAttribute).Assembly.Location
                )
            };

            var compilationFile = Path.Combine(
                Path.GetTempPath(),
                $"{Path.GetFileNameWithoutExtension(projectAnalyzer.ProjectFile.Path)}.dll"
            );

            var compilation = CSharpCompilation.Create(
                compilationFile,
                options: new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary),
                syntaxTrees: trees,
                references: defaultReferences
            );

            return (compilationFile, compilation);
        }

        (string File, Compilation Compilation)[] _compilations = null!;

        readonly Dictionary<Compilation, IEnumerable<IMethodSymbol>> _extensionMethods =
            new Dictionary<Compilation, IEnumerable<IMethodSymbol>>();

        public MetadataItem[] ExtractMetadata(Action<ExtractMetadataOptions>? configure = null)
        {
            if (_compilations == null || _compilations.Length == 0)
                throw new InvalidOperationException("Call the Analyze method first");

            var options = new ExtractMetadataOptions {RoslynExtensionMethods = _extensionMethods};
            configure?.Invoke(options);

            return _compilations.Select(x => x.Compilation.ExtractMetadata(options: options))
                .ToArray();
        }

        public void Dispose()
        {
            foreach (var compilation in _compilations)
            {
                File.Delete(compilation.File);
            }
        }
    }
}
