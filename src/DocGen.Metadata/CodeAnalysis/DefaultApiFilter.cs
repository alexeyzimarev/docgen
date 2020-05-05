using System;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;

// ReSharper disable SuggestBaseTypeForParameter

namespace DocGen.Metadata.CodeAnalysis
{
    public class DefaultApiFilter : IApiFilter
    {
        public bool CanVisitApi(ISymbol symbol, bool wantProtectedMember)
            => symbol != null &&
                CanVisitCore(symbol, CanVisitApi, wantProtectedMember);

        public bool CanVisitAttribute(ISymbol symbol, bool wantProtectedMember)
            => symbol != null &&
                CanVisitCore(symbol, CanVisitAttribute, wantProtectedMember);

        static bool CanVisitCore(ISymbol symbol, Func<ISymbol, bool, bool> filter, bool wantProtectedMember)
        {
            // check parent visibility
            var current = symbol;
            var parent  = symbol.ContainingSymbol;

            while (!(current is INamespaceSymbol) && parent != null)
            {
                if (!filter(parent, wantProtectedMember))
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
                IMethodSymbol m                                       => CanVisitSymbol(m, m.ExplicitInterfaceImplementations),
                IPropertySymbol p                                     => CanVisitSymbol(p, p.ExplicitInterfaceImplementations),
                IEventSymbol e                                        => CanVisitSymbol(e, e.ExplicitInterfaceImplementations),
                IFieldSymbol f                                        => CanVisitFieldSymbol(f),
                INamedTypeSymbol nt                                   => CanVisitNamedTypeSymbol(nt),
                ITypeSymbol ts                                        => CanVisitTypeSymbol(ts),
                _                                                     => symbol.DeclaredAccessibility == Accessibility.Public
            };

            bool CanVisitTypeSymbol(ITypeSymbol typeSymbol)
                => typeSymbol.TypeKind switch
                {
                    TypeKind.Dynamic       => true,
                    TypeKind.TypeParameter => true,
                    TypeKind.Unknown       => false,
                    TypeKind.Error         => false,
                    TypeKind.Array         => filter(((IArrayTypeSymbol) typeSymbol).ElementType, wantProtectedMember),
                    TypeKind.Pointer       => filter(((IPointerTypeSymbol) typeSymbol).PointedAtType, wantProtectedMember),
                    _                      => typeSymbol.DeclaredAccessibility == Accessibility.Public
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

                bool CanVisit() => filter(namedTypeSymbol.ContainingType, wantProtectedMember);

                bool CanVisitProtected() => wantProtectedMember && CanVisit();
            }

            bool CanVisitSymbol<T>(T s, ImmutableArray<T> elementsToFilter) where T : ISymbol
                => s.DeclaredAccessibility switch
                {
                    Accessibility.Public              => true,
                    Accessibility.Protected           => wantProtectedMember,
                    Accessibility.ProtectedOrInternal => wantProtectedMember,
                    _                                 => elementsToFilter.Any(t => filter(t, false))
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