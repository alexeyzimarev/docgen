using Ubiquitous.DocGen.Metadata.Models;

namespace Ubiquitous.DocGen.Markdown
{
    public static class Property
    {
        public static string GeneratePropertyMarkdown(this MetadataItem item, int level)
        {
            var builder = item.GenerateBaseMarkdown(level);
            
            builder.AppendLine(item.Syntax.GenerateMarkdown(level + 1, "Property value"));

            return builder.ToString();
        }
    }
}