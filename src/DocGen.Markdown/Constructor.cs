using DocGen.Metadata.Models;

namespace DocGen.Markdown
{
    public static class Constructor
    {
        public static string GenerateConstructorMarkdown(this MetadataItem item, int level)
        {
            var builder = item.GenerateBaseMarkdown(level);
            
            builder.AppendLine(item.Syntax.GenerateMarkdown(level + 1));

            return builder.ToString();
        }
    }
}