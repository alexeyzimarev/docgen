using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Ubiquitous.DocGen.Markdown.Extensions;
using Ubiquitous.DocGen.Metadata.Models;

namespace Ubiquitous.DocGen.Markdown
{
    public static class MarkdownBase
    {
        public static StringBuilder GenerateBaseMarkdown(this MetadataItem item, int level)
        {
            // if (item.References?.Count > 0)
            // {
            //     Console.WriteLine("refs!");
            // }

            var md = new StringBuilder()
                .AppendLine(Header(level, $"{item.Type} `{item.DisplayName}`"))
                .AppendLine(item.Summary);

            if (!string.IsNullOrWhiteSpace(item.Remarks))
                md.AppendLine(Header(level + 1, "Remarks")).AppendLine(item.Remarks);

            if (item.SeeAlsos?.Count > 0)
                md
                    .AppendLine(Header(level + 1, "See also"))
                    .AppendLines(item.SeeAlsos.Select(x => $"[{x.AltText}]({x.LinkId})"));

            if (item.Examples?.Count > 0)
            {
                md.AppendLine(Header(level + 1, "Examples"));

                foreach (var example in item.Examples)
                    md.AddSourceCode(example);
            }

            return md;
        }

        public static string Header(int level, string header) => $"{new string('#', level)} {header}";

        public static IEnumerable<string> MakeList(this IEnumerable<string> lines) => lines.Select(x => $"-  `{x}`");
        
        public static StringBuilder AddSourceCode(this StringBuilder stringBuilder, string source)
            => stringBuilder
                .AppendLine("```csharp")
                .AppendLine(source)
                .AppendLine("```");
    }
}