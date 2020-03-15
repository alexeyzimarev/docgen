using Ubiquitous.DocGen.Metadata.Models;

namespace Ubiquitous.DocGen.Markdown
{
    public static class Field
    {
        public static string GenerateFieldMarkdown(this MetadataItem item) => $"{item.DisplayName} | {item.Summary?.Replace("\n", "")}";
    }
}