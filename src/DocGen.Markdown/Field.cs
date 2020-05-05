using DocGen.Metadata.Models;

namespace DocGen.Markdown
{
    public static class Field
    {
        public static string GenerateFieldMarkdown(
            this MetadataItem item,
            int level,
            MetadataItem parent
        )
            => parent.Type == MemberType.Enum
                ? $"{item.DisplayName} | {item.Summary?.Replace("\n", "")}"
                : item
                    .GenerateBaseMarkdown(level)
                    .AppendLine(item.Syntax.GenerateMarkdown(level + 1, addReturn: false))
                    .ToString();
    }
}
