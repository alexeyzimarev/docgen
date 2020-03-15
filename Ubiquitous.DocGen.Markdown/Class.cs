using System.Linq;
using Ubiquitous.DocGen.Markdown.Extensions;
using Ubiquitous.DocGen.Metadata.Extensions;
using Ubiquitous.DocGen.Metadata.Models;
using static Ubiquitous.DocGen.Markdown.MarkdownBase;

namespace Ubiquitous.DocGen.Markdown
{
    public static class Class
    {
        public static string GenerateClassMarkdown(this MetadataItem item, int level)
        {
            var builder = item.GenerateBaseMarkdown(level);

            if (item.Inheritance != null)
                builder
                    .AppendLine(Header(level + 1, "Inheritance"))
                    .AppendLines(item.Inheritance.Select((x, l) => $"{new string(' ', l)}↳ {x.Encode()}"));

            if (item.InheritedMembers != null)
                builder
                    .AppendLine()
                    .AppendLine(Header(level + 1, "Inherited members"))
                    .AppendLines(item.InheritedMembers.MakeList());

            builder.AppendLine(item.Syntax.GenerateMarkdown(level + 1));

            if (!item.ExtensionMethods.IsEmpty())
                builder
                    .AppendLine(Header(level + 1, "Extension methods"))
                    .AppendLines(item.ExtensionMethods.MakeList());

            return builder.ToString();
        }
    }
}