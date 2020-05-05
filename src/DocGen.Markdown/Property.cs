using DocGen.Metadata.Models;

namespace DocGen.Markdown
{
    public static class Property
    {
        public static string GeneratePropertyMarkdown(this MetadataItem item, int level)
            => item
                .GenerateBaseMarkdown(level)
                .AppendLine(item.Syntax.GenerateMarkdown(level + 1, "Property value", false))
                .ToString();
    }
}
