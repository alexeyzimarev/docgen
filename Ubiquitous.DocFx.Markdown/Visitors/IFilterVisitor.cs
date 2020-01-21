using Microsoft.CodeAnalysis;

namespace Ubiquitous.DocFx.Markdown.Visitors
{
    public interface IFilterVisitor
    {
        bool CanVisitApi(ISymbol symbol, bool wantProtectedMember = true, IFilterVisitor outer = null);

        bool CanVisitAttribute(ISymbol symbol, bool wantProtectedMember = true, IFilterVisitor outer = null);
    }
}