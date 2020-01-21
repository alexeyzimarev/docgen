using System.Collections.Generic;

namespace Ubiquitous.DocFx.Markdown.Models
{
    public class AttributeInfo
    {
        public string Type { get; set; }

        public string Constructor { get; set; }

        public List<ArgumentInfo> Arguments { get; set; }

        public List<NamedArgumentInfo> NamedArguments { get; set; }
    }
}