using DocGen.Metadata.Comments;
using DocGen.Metadata.Models;

namespace DocGen.Metadata.Extensions
{
    public static class MetadataItemExtensions
    {
        public static void EnsureSyntax(this MetadataItem item) => item.Syntax ??= new SyntaxDetail();

        public static void AddComments(this MetadataItem item, TripleSlashCommentParserContext context)
        {
            if (string.IsNullOrEmpty(item.RawComment)) return;

            var commentModel = TripleSlashCommentModel.CreateModel(item.RawComment,  context);
            if (commentModel == null) return;

            item.Summary      = commentModel.Summary;
            item.Remarks      = commentModel.Remarks;
            item.Exceptions   = commentModel.Exceptions;
            item.Sees         = commentModel.Sees;
            item.SeeAlsos     = commentModel.SeeAlsos;
            item.Examples     = commentModel.Examples;
            item.IsInheritDoc = commentModel.IsInheritDoc;
            item.CommentModel = commentModel;
        }
    }
}