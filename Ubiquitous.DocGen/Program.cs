using System;
using System.Collections.Generic;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Ubiquitous.DocGen.Markdown;
using Ubiquitous.DocGen.Metadata;
using ProjectAnalyzer = Ubiquitous.DocGen.Metadata.ProjectAnalyzer;

namespace Ubiquitous.DocGen
{
    class Program
    {
        static async Task Main(string[] args)
        {
            var command = ConfigureCommand();

            await command.InvokeAsync(args);
        }

        static RootCommand ConfigureCommand()
        {
            var rootCommand = new RootCommand();

            rootCommand.AddOption(
                new Option<IEnumerable<FileInfo>>(new[] {"--project", "-p"}, "Analyze one project, use the project file path").ExistingOnly()
            );

            rootCommand.AddOption(
                new Option<FileInfo>(new[] {"--solution", "-s"}, "Analyze all projects in the solution, use the solution file path").ExistingOnly()
            );

            rootCommand.AddOption(
                new Option<DirectoryInfo>(
                    new[] {"--output", "-o"}, () => new DirectoryInfo(Directory.GetCurrentDirectory()), "Output path for generated Markdown files"
                )
            );

            rootCommand.AddOption(new Option<bool>("--verbose", () => false, "Verbose logging"));
            // rootCommand.AddOption(new Option<string>("--include", "Specify regex to include projects"));
            // rootCommand.AddOption(new Option<string>("--exclude", "Specify regex to exclude projects"));
            rootCommand.Handler = CommandHandler.Create<IEnumerable<FileInfo>, FileInfo, DirectoryInfo, bool>(GenerateMarkdown);
            return rootCommand;
        }

        static async Task GenerateMarkdown(IEnumerable<FileInfo> project, FileInfo solution, DirectoryInfo output, bool verbose)
        {
            var loggerFactory = Logging.ConfigureLogging(cfg => cfg.SetMinimumLevel(verbose ? LogLevel.Debug : LogLevel.Information).AddConsole());

            var log = loggerFactory.CreateLogger("DocGen");

            if (!Directory.Exists(output.FullName))
                Directory.CreateDirectory(output.FullName);

            ProjectAnalyzer analyzer;

            if (solution != null)
            {
                log.LogInformation("Processing solution {Solution}", solution);

                if (project != null)
                    log.LogWarning("Project parameters are ignored for solution processing");
                analyzer = ProjectAnalyzer.ForSolution(solution.FullName, loggerFactory);
            }
            else if (project == null)
            {
                log.LogError("No analysis scope provided, use --help to learn the options");
                return;
            }
            else
            {
                analyzer = ProjectAnalyzer.ForProjects(project.Select(x => x.FullName), loggerFactory);
            }

            await analyzer.Analyze();
            var metadata = analyzer.ExtractMetadata();
            await Task.WhenAll(metadata.Select(x => x.GenerateAssemblyMarkdown(output)));

            log.LogInformation("Done :)");
        }
    }
}