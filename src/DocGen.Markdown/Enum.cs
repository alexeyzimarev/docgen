using DocGen.Metadata.Models;

namespace DocGen.Markdown
{
    public static class Enum
    {
        public static string GenerateEnumMarkdown(this MetadataItem item, int level)
        {
            var builder = item.GenerateBaseMarkdown(level);

            builder
                .AppendLine(item.Syntax.GenerateMarkdown(level + 1))
                .AppendLine(MarkdownBase.Header(level + 1, "Fields"))
                .AppendLine("Name | Description")
                .AppendLine("--- | ---");

            return builder.ToString();
        }
    }
}