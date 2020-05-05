using System.Collections.Generic;
using System.Text;

namespace DocGen.Markdown.Extensions
{
    public static class StringBuilderExtensions
    {
        public static StringBuilder AppendLines(this StringBuilder builder, IEnumerable<string> lines)
        {
            builder.AppendJoin("\r\n", lines);
            return builder.AppendLine();
        }
    }
}