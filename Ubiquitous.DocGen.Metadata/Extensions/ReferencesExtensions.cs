using System.Collections.Generic;
using System.Linq;
using Ubiquitous.DocGen.Metadata.CodeAnalysis;
using Ubiquitous.DocGen.Metadata.Models;

namespace Ubiquitous.DocGen.Metadata.Extensions
{
    public static class ReferencesExtensions
    {
        static void AddComments(this References references, IEnumerable<(string id, string comment)> comments)
        {
            var commentsList = comments?.ToList();
            if (commentsList == null || commentsList.Count == 0) return;
            
            foreach (var (id, comment) in commentsList)
                references.AddCommentReference(id, comment);
        }

        public static void AddLinks(this References references, IEnumerable<LinkInfo> links)
            => references.AddComments(links?.Select(x => (x.LinkId, x.CommentId)));
        
        public static void AddExceptions(this References references, IEnumerable<ExceptionInfo> exceptions)
            => references.AddComments(exceptions?.Select(x => (x.Type, x.CommentId)));
    }
}