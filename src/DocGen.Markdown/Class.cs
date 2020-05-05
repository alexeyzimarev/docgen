using System.Linq;
using DocGen.Markdown.Extensions;
using DocGen.Metadata.Extensions;
using DocGen.Metadata.Models;
using static DocGen.Markdown.MarkdownBase;

namespace DocGen.Markdown
{
    public static class Class
    {
        public static string GenerateClassMarkdown(this MetadataItem item, int level)
        {
            var builder = item.GenerateBaseMarkdown(level);

            if (item.Inheritance!.IsNotEmpty())
                builder
                    .AppendLine(Header(level + 1, "Inheritance"))
                    .AppendLine(
                        string.Join(
                            "<br>",
                            item.Inheritance.Select(
                                (x, l)
                                    => $"{string.Concat(Enumerable.Repeat("&nbsp;", l * 2))}↳ `{x}`"
                            )
                        )
                    );

            if (item.InheritedMembers!.IsNotEmpty())
                builder
                    .AppendLine()
                    .AppendLine(Header(level + 1, "Inherited members"))
                    .AppendLines(item.InheritedMembers.MakeList());

            builder.AppendLine(item.Syntax.GenerateMarkdown(level + 1));

            if (item.ExtensionMethods.IsNotEmpty())
                builder
                    .AppendLine(Header(level + 1, "Extension methods"))
                    .AppendLines(item.ExtensionMethods.MakeList());

            return builder.ToString();
        }
    }
}
