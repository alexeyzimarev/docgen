using Microsoft.CodeAnalysis;
using Ubiquitous.DocFx.Markdown.Extensions;

namespace Ubiquitous.DocFx.Markdown.Visitors
{
    internal class SpecIdVisitor : SymbolVisitor<string>
    {
        public static readonly SpecIdVisitor Instance = new SpecIdVisitor();

        public override string DefaultVisit(ISymbol symbol) => symbol.GetId();

        public override string VisitPointerType(IPointerTypeSymbol symbol) => symbol.PointedAtType.Accept(this) + "*";

        public override string VisitArrayType(IArrayTypeSymbol symbol)
        {
            var suffix = symbol.Rank == 1 ? "[]" : "[" + new string(',', symbol.Rank - 1) + "]";
            return symbol.ElementType.Accept(this) + suffix;
        }

        public override string VisitTypeParameter(ITypeParameterSymbol symbol) => "{" + symbol.Name + "}";
    }
}