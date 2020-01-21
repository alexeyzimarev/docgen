using System.Collections.Generic;
using System.Diagnostics;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Ubiquitous.DocFx.Markdown.Visitors;

namespace Ubiquitous.DocFx.Markdown.Extensions
{
    internal static class SpecIdSymbolExtensions
    {
        static readonly Regex TypeParameterRegex   = new Regex(@"\B(?<!`)`\d+", RegexOptions.Compiled);
        static readonly Regex MethodParameterRegex = new Regex(@"\B``\d+", RegexOptions.Compiled);

        public static string GetSpecId(
            this ISymbol symbol,
            IReadOnlyList<string> typeGenericParameters,
            IReadOnlyList<string> methodGenericParameters = null
        )
        {
            var id = symbol.Accept(SpecIdVisitor.Instance);

            id = methodGenericParameters == null
                ? SpecMethodGenericParameter(symbol as IMethodSymbol ?? symbol.ContainingSymbol as IMethodSymbol, id)
                : SpecGenericParameter(methodGenericParameters, id, MethodParameterRegex, 2);

            id = SpecGenericParameter(typeGenericParameters, id, TypeParameterRegex, 1);

            return symbol is IMethodSymbol methodSymbol ? SpecExtensionMethodReceiverType(methodSymbol, id) : id;
        }

        static string SpecMethodGenericParameter(IMethodSymbol symbol, string id)
            => symbol == null
                ? id
                : MethodParameterRegex.Replace(id,
                    match =>
                    {
                        Debug.Assert(symbol.IsGenericMethod);
                        Debug.Assert(symbol.TypeParameters.Length > int.Parse(match.Value.Substring(2)));
                        return "{" + symbol.TypeParameters[int.Parse(match.Value.Substring(2))].Name + "}";
                    });

        static string SpecGenericParameter(IReadOnlyList<string> names, string id, Regex cleanup, int len)
            => names == null || names.Count == 0
                ? id
                : cleanup.Replace(
                    id,
                    match =>
                    {
                        Debug.Assert(names.Count > int.Parse(match.Value.Substring(len)));
                        return "{" + names[int.Parse(match.Value.Substring(len))] + "}";
                    });

        static string SpecExtensionMethodReceiverType(IMethodSymbol symbol, string id)
            => symbol.ReducedFrom == null || symbol.ReceiverType == null
                ? id
                : symbol.ReceiverType.GetId() + "." + id;
    }
}