using Ubiquitous.DocGen.Metadata.Models;

namespace Ubiquitous.DocGen.Markdown
{
    public static class Method
    {
        public static string GenerateMethodMarkdown(this MetadataItem item, int level)
        {
            var builder = item.GenerateBaseMarkdown(level);

            builder.AppendLine(item.Syntax.GenerateMarkdown(level + 1));

            return builder.ToString();
        }
    }
}