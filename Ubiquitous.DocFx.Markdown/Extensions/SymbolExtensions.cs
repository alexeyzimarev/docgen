using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Ubiquitous.DocFx.Markdown.Models;

namespace Ubiquitous.DocFx.Markdown.Extensions
{
    internal static class SymbolExtensions
    {
        static readonly Regex GenericMethodPostFix = new Regex(@"``\d+$", RegexOptions.Compiled);

        public static string GetId(this ISymbol symbol)
            => symbol switch
            {
                null                                                                    => null,
                INamespaceSymbol namespaceSymbol when namespaceSymbol.IsGlobalNamespace => namespaceSymbol.MetadataName,
                IAssemblySymbol assemblySymbol                                          => assemblySymbol.MetadataName,
                IDynamicTypeSymbol _                                                    => typeof(object).FullName,
                _ =>
                symbol.GetDocumentationCommentId()?.Substring(2)
            };

        public static string GetCommentId(this ISymbol symbol) => symbol switch
        {
            null                 => null,
            IAssemblySymbol _    => null,
            IDynamicTypeSymbol _ => "T:" + typeof(object).FullName,
            _                    => symbol.GetDocumentationCommentId()
        };

        public static string GetOverloadId(this ISymbol symbol) => GetOverloadIdBody(symbol) + "*";

        public static string GetOverloadIdBody(this ISymbol symbol)
        {
            var id      = GetId(symbol);
            var uidBody = id;
            var index   = uidBody.IndexOf('(');
            if (index != -1)
            {
                uidBody = uidBody.Remove(index);
            }

            uidBody = GenericMethodPostFix.Replace(uidBody, string.Empty);
            return uidBody;
        }

        public static SourceDetail GetSourceDetail(this ISymbol symbol)
        {
            // For namespace, definition is meaningless
            if (symbol == null || symbol.Kind == SymbolKind.Namespace)
            {
                return null;
            }

            var syntaxRef = symbol.DeclaringSyntaxReferences.LastOrDefault();
            if (symbol.IsExtern || syntaxRef == null)
            {
                return new SourceDetail
                {
                    IsExternalPath = true,
                    Path           = symbol.ContainingAssembly?.Name,
                };
            }

            var syntaxNode = syntaxRef.GetSyntax();
            if (syntaxNode == null) return null;

            var source = new SourceDetail
            {
                StartLine = syntaxNode.SyntaxTree.GetLineSpan(syntaxNode.Span).StartLinePosition.Line,
                Path      = syntaxNode.SyntaxTree.FilePath,
                Name      = symbol.Name
            };

            return source;
        }

        public static T FindSymbol<T>(this Compilation compilation, T symbol) where T : ISymbol
            => (T) compilation.GlobalNamespace.FindSymbol(symbol);

        static ISymbol FindSymbol(this ISymbol container, ISymbol symbol)
            => FindCore(container, GetQualifiedNameList(symbol))
                .FirstOrDefault(m => GetCommentId(m) == GetCommentId(symbol));

        static IEnumerable<ISymbol> FindCore(ISymbol container, List<string> parts)
        {
            var stack = new Stack<(ISymbol Symbol, int Index)>();
            stack.Push((container, 0));

            while (stack.Count > 0)
            {
                var (parent, index) = stack.Pop();
                if (index == parts.Count)
                {
                    yield return parent;
                }
                else if (parent is INamespaceOrTypeSymbol symbol)
                {
                    foreach (var c in symbol.GetMembers(parts[index]))
                    {
                        stack.Push((c, index + 1));
                    }
                }
            }
        }

        static List<string> GetQualifiedNameList(ISymbol symbol)
        {
            var names   = new List<string>();
            var current = symbol;
            while ((current as INamespaceSymbol)?.IsGlobalNamespace != true)
            {
                names.Add(current.Name);
                current = current.ContainingSymbol;
            }

            names.Reverse();
            return names;
        }
    }
}