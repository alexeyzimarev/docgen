using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using DocGen.Metadata.Models;
using static DocGen.Markdown.Strings;

namespace DocGen.Markdown
{
    public static class Assembly
    {
        public static Task GenerateAssemblyMarkdown(
            this MetadataItem item,
            DirectoryInfo output,
            MemberType scope
        )
        {
            if (item.Type != MemberType.Assembly) throw new ArgumentException("Item is not an assembly", nameof(item));

            item.Name = Path.GetFileNameWithoutExtension(item.Name);

            var scopes = scope == MemberType.Assembly
                ? new[] {item}
                : item.Items.Where(x => x.Type == scope);

            return Task.WhenAll(
                scopes.Select(
                    x =>
                    {
                        var content     = GenerateMarkdown(x, 1);
                        var fileName    = Path.Combine(output.FullName, $"{x.Name}.md");
                        var frontMatter = $"---\r\ntitle: {x.Name}\r\n---\r\n\r\n";
                        return File.WriteAllTextAsync(fileName, frontMatter + content);
                    }
                )
            );
        }

        static string GenerateMarkdown(MetadataItem item, int level, MetadataItem parent = null)
        {
            var itemContent = item.Type switch
            {
                MemberType.Class       => item.GenerateClassMarkdown(level),
                MemberType.Interface   => item.GenerateClassMarkdown(level),
                MemberType.Struct      => item.GenerateClassMarkdown(level),
                MemberType.Constructor => item.GenerateConstructorMarkdown(level),
                MemberType.Method      => item.GenerateMethodMarkdown(level),
                MemberType.Property    => item.GeneratePropertyMarkdown(level),
                MemberType.Enum        => item.GenerateEnumMarkdown(level),
                MemberType.Field       => item.GenerateFieldMarkdown(level, parent),
                MemberType.Namespace   => MarkdownBase.Header(level, $"Namespace: {item.Name}{NewLine}"),
                MemberType.Assembly    => MarkdownBase.Header(level, $"Assembly: {item.Name}{NewLine}"),
                _                      => item + NewLine
            };

            var nestedContent = item.Items == null
                ? ""
                : string.Join(
                    NewLine,
                    item.Items.Select(x => GenerateMarkdown(x, level + 1, item))
                );

            return itemContent + nestedContent;
        }
    }
}
