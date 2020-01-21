using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Ubiquitous.DocFx.Markdown.Extensions;

namespace Ubiquitous.DocFx.Markdown.Visitors
{
    internal class TypeGenericParameterNameVisitor : SymbolVisitor<List<string>>
    {
        public static readonly TypeGenericParameterNameVisitor Instance = new TypeGenericParameterNameVisitor();

        public override List<string> DefaultVisit(ISymbol symbol) => null;

        public override List<string> VisitNamedType(INamedTypeSymbol symbol)
        {
            List<string> result = null;
            if (symbol.ContainingType != null)
            {
                result = symbol.ContainingType.Accept(this);
            }
            result.CreateIfNull().AddRange(symbol.TypeParameters.Select(t => t.Name));
            
            return result;
        }

        public override List<string> VisitEvent(IEventSymbol symbol) => symbol.ContainingType.Accept(this);

        public override List<string> VisitField(IFieldSymbol symbol) => symbol.ContainingType.Accept(this);

        public override List<string> VisitMethod(IMethodSymbol symbol) => symbol.ContainingType.Accept(this);

        public override List<string> VisitProperty(IPropertySymbol symbol) => symbol.ContainingType.Accept(this);

        public override List<string> VisitTypeParameter(ITypeParameterSymbol symbol) => symbol.ContainingType.Accept(this);
    }
}
