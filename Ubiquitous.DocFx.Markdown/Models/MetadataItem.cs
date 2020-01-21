using System.Collections.Generic;
using Ubiquitous.DocFx.Markdown.Parsers;

namespace Ubiquitous.DocFx.Markdown.Models
{
    public class MetadataItem
    {
        public string RawComment { get; set; }

        public bool IsExplicitInterfaceImplementation { get; set; }

        public bool IsExtensionMethod { get; set; }

        public string Name { get; set; }

        public string CommentId { get; set; }

        public SyntaxLanguage Language { get; set; }

        public string DisplayName { get; set; }

        public string DisplayNameWithType { get; set; }

        public string DisplayQualifiedName { get; set; }

        public MemberType Type { get; set; }

        public List<string> AssemblyNameList { get; set; }

        public string NamespaceName { get; set; }

        public SourceDetail Source { get; set; }

        public string Summary { get; set; }

        public string Remarks { get; set; }

        public List<string> Examples { get; set; }

        public SyntaxDetail Syntax { get; set; }

        public string Overload { get; set; }

        public string Overridden { get; set; }

        public List<ExceptionInfo> Exceptions { get; set; }

        public List<LinkInfo> Sees { get; set; }

        public List<LinkInfo> SeeAlsos { get; set; }

        public List<string> Inheritance { get; set; }

        public List<string> DerivedClasses { get; set; }

        public List<string> Implements { get; set; }

        public List<string> InheritedMembers { get; set; }

        public List<string> ExtensionMethods { get; set; }

        public List<AttributeInfo> Attributes { get; set; }

        public List<string> Modifiers { get; set; } = new List<string>();

        public List<MetadataItem> Items { get; set; }

        public Dictionary<string, ReferenceItem> References { get; set; }

        public bool IsInheritDoc { get; set; }

        public TripleSlashCommentModel CommentModel { get; set; }

        public override string ToString() => Type + ": " + Name;
    }
}