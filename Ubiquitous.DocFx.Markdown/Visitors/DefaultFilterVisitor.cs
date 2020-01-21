using System;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace Ubiquitous.DocFx.Markdown.Visitors
{
    public class DefaultFilterVisitor : IFilterVisitor
    {
        public bool CanVisitApi(ISymbol symbol, bool wantProtectedMember, IFilterVisitor outer)
            => symbol != null &&
                CanVisitCore(symbol, (outer ?? this).CanVisitApi, wantProtectedMember, outer ?? this);

        public bool CanVisitAttribute(ISymbol symbol, bool wantProtectedMember, IFilterVisitor outer)
            => symbol != null &&
                CanVisitCore(symbol, (outer ?? this).CanVisitAttribute, wantProtectedMember, outer ?? this);

        static bool CanVisitCore(
            ISymbol symbol, Func<ISymbol, bool, IFilterVisitor, bool> visitFunc, bool wantProtectedMember,
            IFilterVisitor outer
        )
        {
            // check parent visibility
            var current = symbol;
            var parent  = symbol.ContainingSymbol;
            while (!(current is INamespaceSymbol) && parent != null)
            {
                if (!visitFunc(parent, wantProtectedMember, outer))
                {
                    return false;
                }

                current = parent;
                parent  = parent.ContainingSymbol;
            }

            if (!(symbol is INamespaceSymbol) && symbol.IsImplicitlyDeclared)
            {
                return false;
            }

            return symbol switch
            {
                {DeclaredAccessibility: Accessibility.NotApplicable } => true,
                IMethodSymbol methodSymbol => CanVisitSymbol(methodSymbol,
                    methodSymbol.ExplicitInterfaceImplementations),
                IPropertySymbol propertySymbol => CanVisitSymbol(propertySymbol,
                    propertySymbol.ExplicitInterfaceImplementations),
                IEventSymbol eventSymbol => CanVisitSymbol(eventSymbol,
                    eventSymbol.ExplicitInterfaceImplementations),
                IFieldSymbol fieldSymbol         => CanVisitFieldSymbol(fieldSymbol),
                INamedTypeSymbol namedTypeSymbol => CanVisitNamedTypeSymbol(namedTypeSymbol),
                ITypeSymbol ts                   => CanVisitTypeSymbol(ts),
                _                                => symbol.DeclaredAccessibility == Accessibility.Public
            };

            bool CanVisitTypeSymbol(ITypeSymbol typeSymbol)
                => typeSymbol.TypeKind switch
                {
                    TypeKind.Dynamic       => true,
                    TypeKind.TypeParameter => true,
                    TypeKind.Unknown       => false,
                    TypeKind.Error         => false,
                    TypeKind.Array => visitFunc(((IArrayTypeSymbol) typeSymbol).ElementType, wantProtectedMember,
                        outer),
                    TypeKind.Pointer => visitFunc(((IPointerTypeSymbol) typeSymbol).PointedAtType, wantProtectedMember,
                        outer),
                    _ => (typeSymbol.DeclaredAccessibility == Accessibility.Public)
                };

            bool CanVisitNamedTypeSymbol(INamedTypeSymbol namedTypeSymbol)
            {
                if (namedTypeSymbol.ContainingType == null)
                    return namedTypeSymbol.DeclaredAccessibility == Accessibility.Public;

                return namedTypeSymbol.DeclaredAccessibility switch
                {
                    Accessibility.Public              => CanVisit(),
                    Accessibility.Protected           => CanVisitProtected(),
                    Accessibility.ProtectedOrInternal => CanVisitProtected(),
                    _                                 => false
                };

                bool CanVisit() => visitFunc(namedTypeSymbol.ContainingType, wantProtectedMember, outer);

                bool CanVisitProtected() => wantProtectedMember && CanVisit();
            }

            bool CanVisitSymbol<T>(
                T s, ImmutableArray<T> elementsToFilter
            ) where T : ISymbol
                => s.DeclaredAccessibility switch
                {
                    Accessibility.Public              => true,
                    Accessibility.Protected           => wantProtectedMember,
                    Accessibility.ProtectedOrInternal => wantProtectedMember,
                    _                                 => elementsToFilter.Any(t => visitFunc(t, false, outer))
                };

            bool CanVisitFieldSymbol(IFieldSymbol fieldSymbol)
                => fieldSymbol.DeclaredAccessibility switch
                {
                    Accessibility.Public              => true,
                    Accessibility.Protected           => wantProtectedMember,
                    Accessibility.ProtectedOrInternal => wantProtectedMember,
                    _                                 => false
                };
        }
    }
}