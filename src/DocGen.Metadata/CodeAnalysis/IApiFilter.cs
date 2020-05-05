using Microsoft.CodeAnalysis;

namespace DocGen.Metadata.CodeAnalysis
{
    public interface IApiFilter
    {
        bool CanVisitApi(ISymbol symbol, bool wantProtectedMember = true);

        bool CanVisitAttribute(ISymbol symbol, bool wantProtectedMember = true);
    }
}