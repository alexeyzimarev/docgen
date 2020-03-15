using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.CodeAnalysis;
using Ubiquitous.DocGen.Metadata.Extensions;
using Ubiquitous.DocGen.Metadata.Models;

namespace Ubiquitous.DocGen.Metadata.CodeAnalysis
{
#nullable enable
    public class References : Dictionary<string, ReferenceItem?>
    {
        void AddOrMergeReference(ReferenceItem item)
        {
            if (!TryAdd(item.Id, item)) this[item.Id]?.Merge(item);
        }

        internal void ClearReference(string id) => this[id] = null;

        internal string AddReference(ISymbol symbol)
        {
            var id = symbol.GetRawId();

            var reference = new ReferenceItem(id)
            {
                Parts        = new List<LinkItem>(),
                IsDefinition = symbol.IsDefinition,
                CommentId    = symbol.GetCommentId()
            };
            symbol.GenerateReference(reference);

            AddOrMergeReference(reference);

            return id;
        }

        internal void AddCommentReference(string id, string commentId) => TryAdd(id, new ReferenceItem(id) {CommentId = commentId});

        internal string AddOverloadReference(ISymbol symbol)
        {
            var uidBody = symbol.GetOverloadIdBody();

            var reference = new ReferenceItem(uidBody + "*")
            {
                Parts        = new List<LinkItem>(),
                IsDefinition = true,
                CommentId    = "Overload:" + uidBody
            };
            symbol.GenerateReference(reference, true);

            AddOrMergeReference(reference);

            return reference.Id;
        }

        internal string AddSpecReference(ISymbol symbol)
        {
            var rawId = symbol.GetRawId();
            var id    = symbol?.ToString()?.Replace(" ", "").Replace("()", "");

            if (rawId != id)
            {
                Console.WriteLine($"{id} {rawId}");
            }

            if (string.IsNullOrEmpty(id))
            {
                throw new InvalidDataException(
                    $"Fail to parse id for symbol {symbol.MetadataName} in namespace {symbol.ContainingSymbol?.MetadataName}."
                );
            }

            var reference = new ReferenceItem(id) {Parts = new List<LinkItem>()};

            symbol.GenerateReference(reference);
            var reducedFrom    = (symbol as IMethodSymbol)?.ReducedFrom;
            var originalSymbol = reducedFrom ?? symbol;

            reference.IsDefinition = originalSymbol.Equals(symbol) && id == rawId &&
                (symbol.IsDefinition || symbol.OriginalDefinition.GetRawId() == rawId);

            if (!reference.IsDefinition.Value && rawId != null)
            {
                reference.Definition = AddReference(originalSymbol.OriginalDefinition);
            }

            reference.Parent    = GetReferenceParent(originalSymbol);
            reference.CommentId = originalSymbol.GetCommentId();

            AddOrMergeReference(reference);

            return id;
        }

        string? GetReferenceParent(ISymbol symbol)
        {
            switch (symbol.Kind)
            {
                case SymbolKind.Event:
                case SymbolKind.Field:
                case SymbolKind.Method:
                case SymbolKind.NamedType:
                case SymbolKind.Property:
                {
                    var parentSymbol = symbol;

                    do
                    {
                        parentSymbol = parentSymbol.ContainingSymbol;
                    } while (parentSymbol.Kind == symbol.Kind); // the parent of nested type is namespace.

                    return IsGlobalNamespace(parentSymbol)
                        ? null
                        : AddSpecReference(parentSymbol);
                }
                default:
                    return null;
            }

            static bool IsGlobalNamespace(ISymbol symbol) => (symbol as INamespaceSymbol)?.IsGlobalNamespace == true;
        }
    }
}