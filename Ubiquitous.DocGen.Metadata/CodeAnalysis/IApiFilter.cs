using Microsoft.CodeAnalysis;

namespace Ubiquitous.DocGen.Metadata.Visitors
{
    public interface IApiFilter
    {
        bool CanVisitApi(ISymbol symbol, bool wantProtectedMember = true);

        bool CanVisitAttribute(ISymbol symbol, bool wantProtectedMember = true);
    }
}