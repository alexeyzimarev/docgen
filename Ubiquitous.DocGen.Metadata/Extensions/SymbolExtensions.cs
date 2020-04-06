using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Ubiquitous.DocGen.Metadata.Models;

namespace Ubiquitous.DocGen.Metadata.Extensions
{
    static class SymbolExtensions
    {
        public static string GetRawId(this ISymbol symbol)
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

        internal static string GetSuffix(this IArrayTypeSymbol symbol) => symbol.Rank == 1 ? "[]" : $"[{new string(',', symbol.Rank - 1)}]";

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

        internal static MemberType GetMemberTypeFromSymbol(this ISymbol symbol)
            => symbol.Kind switch
            {
                SymbolKind.Namespace => MemberType.Namespace,
                SymbolKind.NamedType => (symbol is INamedTypeSymbol nameTypeSymbol
                    ? GetMemberTypeFromTypeKind(nameTypeSymbol)
                    : MemberType.Default),
                SymbolKind.Event    => MemberType.Event,
                SymbolKind.Field    => MemberType.Field,
                SymbolKind.Property => MemberType.Property,
                SymbolKind.Method   => GetMethodMemberType(symbol),
                _                   => MemberType.Default
            };

        internal static MemberType GetMethodMemberType(this ISymbol symbol)
        {
            if (!(symbol is IMethodSymbol methodSymbol)) return MemberType.Default;

            return methodSymbol.MethodKind switch
            {
                MethodKind.AnonymousFunction               => MemberType.Method,
                MethodKind.DelegateInvoke                  => MemberType.Method,
                MethodKind.Destructor                      => MemberType.Method,
                MethodKind.ExplicitInterfaceImplementation => MemberType.Method,
                MethodKind.Ordinary                        => MemberType.Method,
                MethodKind.ReducedExtension                => MemberType.Method,
                MethodKind.DeclareMethod                   => MemberType.Method,
                MethodKind.BuiltinOperator                 => MemberType.Operator,
                MethodKind.UserDefinedOperator             => MemberType.Operator,
                MethodKind.Conversion                      => MemberType.Operator,
                MethodKind.Constructor                     => MemberType.Constructor,
                MethodKind.StaticConstructor               => MemberType.Constructor,
                MethodKind.PropertyGet                     => MemberType.Default,
                MethodKind.PropertySet                     => MemberType.Default,
                MethodKind.EventAdd                        => MemberType.Default,
                MethodKind.EventRemove                     => MemberType.Default,
                MethodKind.EventRaise                      => MemberType.Default,
                _                                          => MemberType.Default
            };
        }
        
        internal static MemberType GetMemberTypeFromTypeKind(this INamedTypeSymbol symbol)
            => symbol.TypeKind switch
            {
                TypeKind.Module    => MemberType.Class,
                TypeKind.Class     => MemberType.Class,
                TypeKind.Enum      => MemberType.Enum,
                TypeKind.Interface => MemberType.Interface,
                TypeKind.Struct    => MemberType.Struct,
                TypeKind.Delegate  => MemberType.Delegate,
                _                  => MemberType.Default
            };
        
        internal static bool IsInheritable(this ISymbol memberSymbol)
            => (memberSymbol as IMethodSymbol)?.MethodKind switch
            {
                null                                       => true,
                MethodKind.ExplicitInterfaceImplementation => true,
                MethodKind.DeclareMethod                   => true,
                MethodKind.Ordinary                        => true,
                _                                          => false
            };

    }
}