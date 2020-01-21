using System;
using Ubiquitous.DocFx.Markdown.Models;

namespace Ubiquitous.DocFx.Markdown.Parsers
{
    public class TripleSlashCommentParserContext
    {
        public Action<string, string> AddReferenceDelegate { get; set; }

        public SourceDetail Source { get; set; }

        public string CodeSourceBasePath { get; set; }
    }
}
