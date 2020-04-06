using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Ubiquitous.DocGen.Metadata.Visitors;

namespace Ubiquitous.DocGen.Metadata.CodeAnalysis.Syntax
{
    static class Implements
    {
        static IEnumerable<ISymbol> GetImplements(ISymbol symbol, IApiFilter apiFilter)
            => symbol.ContainingType.AllInterfaces
                .Where(type => apiFilter.CanVisitApi(type))
                .SelectMany(type => type.GetMembers())
                .Where(
                    x => apiFilter.CanVisitApi(x) &&
                        symbol.Equals(
                            symbol.ContainingType.FindImplementationForInterfaceMember(x)
                        )
                );

        internal static List<string> GetMemberImplements(this ISymbol symbol, IApiFilter apiFilter)
            => GetImplements(symbol, apiFilter)
                .Select(x => x.ToDisplayString())
                .ToList();

        internal static List<string> GetMethodImplements(this IMethodSymbol symbol, IApiFilter apiFilter)
            => GetImplements(symbol, apiFilter)
                .Select(
                    x => symbol.TypeParameters.Length == 0
                        ? x
                        : ((IMethodSymbol) x).Construct(
                            symbol.TypeParameters.ToArray<ITypeSymbol>()
                        )
                )
                .Select(x => x.ToDisplayString())
                .ToList();

        
    }
}
