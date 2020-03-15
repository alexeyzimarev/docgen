using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Ubiquitous.DocGen.Metadata.Models;
using static Ubiquitous.DocGen.Markdown.Strings;

namespace Ubiquitous.DocGen.Markdown
{
    public static class Assembly
    {
        public static Task GenerateAssemblyMarkdown(this MetadataItem item, DirectoryInfo output)
        {
            if (item.Type != MemberType.Assembly)
                throw new ArgumentException("Item is not an assembly", nameof(item));

            return Task.WhenAll(
                item.Items
                    .Where(x => x.Type == MemberType.Namespace)
                    .Select(
                        x =>
                        {
                            var content  = GenerateMarkdown(x, 1);
                            var fileName = Path.Combine(output.FullName, $"{x.Name}.md");
                            return File.WriteAllTextAsync(fileName, content);
                        }
                    )
            );
        }

        static string GenerateMarkdown(MetadataItem item, int level)
        {
            var itemContent = item.Type switch
            {
                MemberType.Class       => item.GenerateClassMarkdown(level),
                MemberType.Interface   => item.GenerateClassMarkdown(level),
                MemberType.Constructor => item.GenerateConstructorMarkdown(level),
                MemberType.Method      => item.GenerateMethodMarkdown(level),
                MemberType.Property    => item.GeneratePropertyMarkdown(level),
                MemberType.Enum        => item.GenerateEnumMarkdown(level),
                MemberType.Field       => item.GenerateFieldMarkdown(),
                MemberType.Namespace   => MarkdownBase.Header(level, $"Namespace: {item.Name}{NewLine}"),
                _                      => item + NewLine
            };

            var nestedContent = item.Items == null
                ? ""
                : string.Join(NewLine, item.Items.Select(x => GenerateMarkdown(x, level + 1)));

            return itemContent + nestedContent;
        }
    }
}