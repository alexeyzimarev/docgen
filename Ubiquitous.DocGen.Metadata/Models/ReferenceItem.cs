using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Ubiquitous.DocGen.Metadata.Models
{
    public class ReferenceItem
    {
        public ReferenceItem(string id) => Id = id;
        
        public string Id { get; }
        
        public List<LinkItem> Parts { get; set; }

        public bool? IsDefinition { get; set; }

        public string Definition { get; set; }

        public string Parent { get; set; }

        public string CommentId { get; set; }

        static T? Merge<T>(T? source, T? target) where T : struct
        {
            Debug.Assert(source == null || target == null || Nullable.Equals(source, target));
            return source ?? target;
        }

        static T Merge<T>(T source, T target) where T : class
        {
            Debug.Assert(source == null || target == null || Equals(source, target), $"{source} {target}");
            return source ?? target;
        }

        static string MergeCommentId(string source, string target)
        {
            var sourceIsNotError = source?.StartsWith("!:", StringComparison.Ordinal) == false;
            var targetIsNotError = target?.StartsWith("!:", StringComparison.Ordinal) == false;
            return sourceIsNotError && targetIsNotError ? Merge(source, target) :
                sourceIsNotError ? source :
                targetIsNotError ? target : null;
        }

        public void Merge(ReferenceItem other)
        {
            if (other == null)
            {
                throw new ArgumentNullException(nameof(other));
            }

            IsDefinition = Merge(other.IsDefinition, IsDefinition);
            Definition   = Merge(other.Definition, Definition);
            Parent       = Merge(other.Parent, Parent);
            if (IsDefinition == true)
            {
                CommentId = MergeCommentId(other.CommentId, CommentId);
            }

            if (other.Parts != null && Parts != null && other.Parts.Count > 0)
            {
                if (Parts.Count == 0)
                {
                    Parts.AddRange(other.Parts);
                }
                else
                {
                    Debug.Assert(Parts.Count == other.Parts.Count);

                    if (Parts.Count != other.Parts.Count) return;
                    
                    for (var i = 0; i < other.Parts.Count; i++)
                    {
                        Debug.Assert(other.Parts[i].Name                 == Parts[i].Name);
                        Debug.Assert(other.Parts[i].DisplayName          == Parts[i].DisplayName);
                        Debug.Assert(other.Parts[i].DisplayQualifiedName == Parts[i].DisplayQualifiedName);
                            
                        Parts[i].IsExternalPath &=  other.Parts[i].IsExternalPath;
                    }
                }
            }
            else
            {
                Parts ??= other.Parts;
            }
        }
    }

    public class LinkItem
    {
        public string Name { get; set; }

        public string DisplayName { get; set; }

        public string DisplayNameWithType { get; set; }

        public string DisplayQualifiedName { get; set; }

        /// <summary>
        /// The external path for current source if it is not locally available
        /// </summary>
        public bool IsExternalPath { get; set; }
    }
}