# DocGen

DocGen generates API documentation with markdown for libraries written for .NET.

It uses Roslyn code analysers and triple-slash comments.

DocGen _only_ supports libraries written in C#.

The code of DocGen is based on the original code of [DocFx](https://github.com/dotnet/docfx),
the API documentation tool built by Microsoft.

## Installation

DocGen is distributed as a .NET tool via NuGet.org.

Install it globally by running:

```
dotnet tool install --global DocGen.Cli
``` 

## Usage

DocGen can analyse the whole solution or individual project files.
Output files match assembly names with `.md` extension.

To generate documentation for the whole solution, run the command:

```
docgen -s Solution.sln
```

For individual project files, run:

```
docgen -p Project.csproj
```

You can use a file mask to find and analyse multiple project files:

```
docgen -p ./src/**/*.csproj -o docs
```

Use the `-o` option to specify the output path for markdown files.

## Roadmap

- Handle `/// inherit-doc`
- Cross-reference known items with links
- Make frontmatter generation configurable
- Compile-time library in addition to the CLI tool
