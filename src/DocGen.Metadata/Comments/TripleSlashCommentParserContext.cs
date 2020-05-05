using System;
using DocGen.Metadata.Models;

namespace DocGen.Metadata.Comments
{
    public class TripleSlashCommentParserContext
    {
        public Action<string, string>? AddReferenceDelegate { get; set; }

        public SourceDetail? Source { get; set; }

        public string? CodeSourceBasePath { get; set; }
    }
}
