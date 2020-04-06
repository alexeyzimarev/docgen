using System.Collections.Generic;
using System.Linq;
using System.Text;
using Ubiquitous.DocGen.Markdown.Extensions;
using Ubiquitous.DocGen.Metadata.Extensions;
using Ubiquitous.DocGen.Metadata.Models;
using static Ubiquitous.DocGen.Markdown.MarkdownBase;

namespace Ubiquitous.DocGen.Markdown
{
    public static class Syntax
    {
        public static string GenerateMarkdown(this SyntaxDetail syntax, int level, string returnHeader = null)
        {
            var builder = new StringBuilder()
                .AppendLine(Header(level, "Syntax"))
                .AddSourceCode(syntax.Content.Replace("<>", ""));

            AppendGenericParameters(syntax.TypeParameters);
            AppendParameters(syntax.Parameters);
            AppendReturn();

            return builder.ToString();

            void AppendParameters(List<ApiParameter> parameters)
            {
                if (parameters.IsEmpty()) return;

                builder
                    .AppendLine(Header(level, "Parameters"))
                    .AppendLine("Name | Type | Description")
                    .AppendLine("--- | --- | ---")
                    .AppendLines(parameters.Select(x => $"`{x.Name}` | `{x.Type}` | {x.Description}"))
                    .AppendLine();
            }

            void AppendGenericParameters(List<ApiParameter> parameters)
            {
                if (parameters.IsEmpty()) return;

                builder
                    .AppendLine(Header(level, "Generic parameters"))
                    .AppendLine("Name | Description")
                    .AppendLine("--- | ---")
                    .AppendLines(parameters.Select(x => $"`{x.Name}` | {x.Description}"))
                    .AppendLine();
            }

            void AppendReturn()
            {
                if (syntax.Return == null) return;
                
                builder
                    .AppendLine(Header(level, $"{returnHeader ?? "Returns"}"))
                    .AppendLine("Type | Description")
                    .AppendLine("--- | ---")
                    .AppendLine($"`{syntax.Return.Type}` | {syntax.Return.Description}")
                    .AppendLine();
            }
        }
    }
}