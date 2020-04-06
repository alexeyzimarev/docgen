using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Optional.Collections;
using Ubiquitous.DocGen.Metadata.CodeAnalysis.Syntax;
using Ubiquitous.DocGen.Metadata.Extensions;
using Ubiquitous.DocGen.Metadata.Models;
using Ubiquitous.DocGen.Metadata.Visitors;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using Attribute = System.Attribute;

namespace Ubiquitous.DocGen.Metadata.CodeAnalysis
{
    public static class SyntaxGenerator
    {
        static readonly Regex BracesRegex = new Regex(@"\s*\{(;|\s)*\}\s*$", RegexOptions.Compiled);

        internal static string GenerateSyntax(
            this ISymbol symbol,
            MemberType type,
            IApiFilter apiFilter
        )
            => GetSyntaxContent(type, symbol, apiFilter);

        static string GetSyntaxContent(MemberType typeKind, ISymbol symbol, IApiFilter apiFilter)
            => typeKind switch
            {
                MemberType.Class       => GetClassSyntax((INamedTypeSymbol) symbol, apiFilter),
                MemberType.Enum        => GetEnumSyntax((INamedTypeSymbol) symbol, apiFilter),
                MemberType.Interface   => GetInterfaceSyntax((INamedTypeSymbol) symbol, apiFilter),
                MemberType.Struct      => GetStructSyntax((INamedTypeSymbol) symbol, apiFilter),
                MemberType.Delegate    => GetDelegateSyntax((INamedTypeSymbol) symbol, apiFilter),
                MemberType.Method      => GetMethodSyntax((IMethodSymbol) symbol, apiFilter),
                MemberType.Operator    => GetOperatorSyntax((IMethodSymbol) symbol, apiFilter),
                MemberType.Constructor => GetConstructorSyntax((IMethodSymbol) symbol, apiFilter),
                MemberType.Field       => GetFieldSyntax((IFieldSymbol) symbol, apiFilter),
                MemberType.Event       => GetEventSyntax((IEventSymbol) symbol, apiFilter),
                MemberType.Property    => GetPropertySyntax((IPropertySymbol) symbol, apiFilter),
                _                      => null
            };

        static string GetClassSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => RemoveBraces(
                ClassDeclaration(
                        symbol.GetAttributes(apiFilter),
                        TokenList(GetTypeModifiers(symbol)),
                        Identifier(symbol.Name),
                        GetTypeParameters(symbol.TypeArguments),
                        symbol.GetBaseTypeList(),
                        List(symbol.TypeArguments.GetTypeParameterConstraints()),
                        new SyntaxList<MemberDeclarationSyntax>()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );

        static string GetEnumSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => RemoveBraces(
                EnumDeclaration(
                        symbol.GetAttributes(apiFilter),
                        TokenList(symbol.GetTypeModifiers()),
                        Identifier(symbol.Name),
                        symbol.GetEnumBaseTypeList(),
                        new SeparatedSyntaxList<EnumMemberDeclarationSyntax>()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );

        static string GetInterfaceSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => RemoveBraces(
                InterfaceDeclaration(
                        symbol.GetAttributes(apiFilter),
                        TokenList(symbol.GetTypeModifiers()),
                        Identifier(symbol.Name),
                        GetTypeParameters(symbol.TypeArguments),
                        symbol.GetBaseTypeList(),
                        List(symbol.TypeArguments.GetTypeParameterConstraints()),
                        new SyntaxList<MemberDeclarationSyntax>()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );

        static string GetStructSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => RemoveBraces(
                StructDeclaration(
                        symbol.GetAttributes(apiFilter),
                        TokenList(symbol.GetTypeModifiers()),
                        Identifier(symbol.Name),
                        GetTypeParameters(symbol.TypeArguments),
                        symbol.GetBaseTypeList(),
                        List(symbol.TypeArguments.GetTypeParameterConstraints()),
                        new SyntaxList<MemberDeclarationSyntax>()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );

        static string GetDelegateSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => DelegateDeclaration(
                    symbol.GetAttributes(apiFilter),
                    TokenList(symbol.GetTypeModifiers()),
                    symbol.DelegateInvokeMethod.ReturnType.GetTypeSyntax(),
                    Identifier(symbol.Name),
                    GetTypeParameters(symbol.TypeArguments),
                    symbol.DelegateInvokeMethod.Parameters.GetParametersSyntax(apiFilter),
                    List(symbol.TypeArguments.GetTypeParameterConstraints())
                )
                .NormalizeWhitespace()
                .ToString();

        static string GetMethodSyntax(this IMethodSymbol symbol, IApiFilter apiFilter)
        {
            var eii = symbol.GetEeiSyntax(apiFilter);

            return MethodDeclaration(
                    symbol.GetAttributes(apiFilter),
                    TokenList(symbol.GetMemberModifiers()),
                    symbol.ReturnType.GetTypeSyntax(),
                    eii,
                    Identifier(symbol.GetMemberName(apiFilter)),
                    GetTypeParameters(symbol.TypeArguments),
                    symbol.Parameters.GetParametersSyntax(apiFilter, (p, i) => i == 0 && symbol.IsExtensionMethod),
                    List(symbol.TypeArguments.GetTypeParameterConstraints()),
                    null,
                    null
                )
                .NormalizeWhitespace()
                .ToString();
        }

        static string GetOperatorSyntax(this IMethodSymbol symbol, IApiFilter apiFilter)
        {
            var operatorToken = GetOperatorToken(symbol);

            if (operatorToken == null) return "Not supported in C#";

            var attr  = symbol.GetAttributes(apiFilter);
            var token = TokenList(symbol.GetMemberModifiers());
            var ret   = symbol.ReturnType.GetTypeSyntax();
            var param = symbol.Parameters.GetParametersSyntax(apiFilter);

            return operatorToken.Value.Kind() == SyntaxKind.ImplicitKeyword ||
                operatorToken.Value.Kind() == SyntaxKind.ExplicitKeyword
                    ? ConversionOperatorDeclaration(
                            attr,
                            token,
                            operatorToken.Value,
                            ret,
                            param,
                            null,
                            null
                        )
                        .NormalizeWhitespace()
                        .ToString()
                    : OperatorDeclaration(
                            attr,
                            token,
                            ret,
                            operatorToken.Value,
                            param,
                            null,
                            null
                        )
                        .NormalizeWhitespace()
                        .ToString();
        }

        static string GetConstructorSyntax(this IMethodSymbol symbol, IApiFilter apiFilter)
            => ConstructorDeclaration(
                    symbol.GetAttributes(apiFilter),
                    TokenList(symbol.GetMemberModifiers()),
                    Identifier(symbol.ContainingType.Name),
                    symbol.Parameters.GetParametersSyntax(apiFilter),
                    null,
                    (BlockSyntax) null
                )
                .NormalizeWhitespace()
                .ToString();

        static string GetFieldSyntax(this IFieldSymbol symbol, IApiFilter apiFilter)
            => symbol.ContainingType.TypeKind == TypeKind.Enum
                ? EnumMemberDeclaration(
                        symbol.GetAttributes(apiFilter),
                        Identifier(symbol.Name),
                        symbol.GetDefaultValueClause()
                    )
                    .NormalizeWhitespace()
                    .ToString()
                : FieldDeclaration(
                        symbol.GetAttributes(apiFilter),
                        TokenList(symbol.GetMemberModifiers()),
                        VariableDeclaration(
                            symbol.Type.GetTypeSyntax(),
                            SingletonSeparatedList(
                                VariableDeclarator(
                                    Identifier(symbol.Name),
                                    null,
                                    symbol.GetDefaultValueClause()
                                )
                            )
                        )
                    )
                    .NormalizeWhitespace()
                    .ToString()
                    .TrimEnd(';');

        static string GetEventSyntax(this IEventSymbol symbol, IApiFilter apiFilter)
        {
            var eii = symbol.GetEeiSyntax(apiFilter);

            return RemoveBraces(
                EventDeclaration(
                        symbol.GetAttributes(apiFilter),
                        TokenList(symbol.GetMemberModifiers()),
                        Token(SyntaxKind.EventKeyword),
                        symbol.Type.GetTypeSyntax(),
                        eii,
                        Identifier(symbol.GetMemberName(apiFilter)),
                        AccessorList()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );
        }

        static string GetPropertySyntax(this IPropertySymbol symbol, IApiFilter apiFilter)
        {
            var eii          = symbol.GetEeiSyntax(apiFilter);
            var attributes   = symbol.GetAttributes(apiFilter);
            var tokens       = TokenList(symbol.GetMemberModifiers());
            var typeSyntax   = symbol.Type.GetTypeSyntax();
            var accessorList = AccessorList(List(symbol.GetPropertyAccessors(apiFilter)));

            var result = symbol.IsIndexer
                ? IndexerDeclaration(
                        attributes,
                        tokens,
                        typeSyntax,
                        eii,
                        BracketedParameterList(
                            SeparatedList(symbol.Parameters.Select(p => p.GetParameter(apiFilter)))
                        ),
                        accessorList
                    )
                    .NormalizeWhitespace()
                    .ToString()
                : PropertyDeclaration(
                        attributes,
                        tokens,
                        typeSyntax,
                        eii,
                        Identifier(symbol.GetMemberName(apiFilter)),
                        accessorList
                    )
                    .NormalizeWhitespace()
                    .ToString();

            result = Regex.Replace(result, @"\s*\{\s*get;\s*set;\s*}\s*$", " { get; set; }");
            result = Regex.Replace(result, @"\s*\{\s*get;\s*}\s*$", " { get; }");
            result = Regex.Replace(result, @"\s*\{\s*set;\s*}\s*$", " { set; }");

            result = Regex.Replace(
                result,
                @"\s*\{\s*get;\s*protected set;\s*}\s*$",
                " { get; protected set; }"
            );

            result = Regex.Replace(
                result,
                @"\s*\{\s*protected get;\s*set;\s*}\s*$",
                " { protected get; set; }"
            );
            return result;
        }

        static ParameterListSyntax GetParametersSyntax(
            this ImmutableArray<IParameterSymbol> parameterSymbols,
            IApiFilter apiFilter,
            Func<IParameterSymbol, int, bool> predicate = null
        )
            => ParameterList(
                SeparatedList(parameterSymbols.Select((p, i) => p.GetParameter(apiFilter, predicate?.Invoke(p, i) ?? false)))
            );

        static SyntaxList<AttributeListSyntax> GetAttributes(
            this ISymbol symbol,
            IApiFilter apiFilter,
            bool inOneLine = false
        )
        {
            var attributes = symbol.GetAttributes();
            if (attributes.Length <= 0) return new SyntaxList<AttributeListSyntax>();

            var attrList = attributes
                .Where(attr => !(attr.AttributeClass is IErrorTypeSymbol))
                .Where(
                    attr => attr.AttributeConstructor != null &&
                        apiFilter.CanVisitAttribute(attr.AttributeConstructor)
                )
                .Select(GetAttributeSyntax)
                .ToList();

            return attrList.Count > 0
                ? inOneLine
                    ? SingletonList(AttributeList(SeparatedList(attrList)))
                    : List(attrList.Select(attr => AttributeList(SingletonSeparatedList(attr))))
                : new SyntaxList<AttributeListSyntax>();
        }

        static AttributeSyntax GetAttributeSyntax(AttributeData attr)
        {
            var attrTypeName = NameVisitorFactory
                .GetNameVisitor(NameOptions.None)
                .GetName(attr.AttributeClass);

            if (attrTypeName.EndsWith(nameof(Attribute), StringComparison.Ordinal))
                attrTypeName = attrTypeName.Remove(attrTypeName.Length - nameof(Attribute).Length);

            if (attr.ConstructorArguments.Length == 0 && attr.NamedArguments.Length == 0)
                return Attribute(ParseName(attrTypeName));

            return Attribute(
                ParseName(attrTypeName),
                AttributeArgumentList(
                    SeparatedList(
                        attr.ConstructorArguments
                            .Select(x => x.GetLiteralExpression())
                            .Where(expr => expr != null)
                            .Select(AttributeArgument)
                            .Concat(
                                attr.NamedArguments
                                    .Select(
                                        item => new {item, expr = item.Value.GetLiteralExpression()}
                                    )
                                    .Where(t => t.expr != null)
                                    .Select(
                                        t => AttributeArgument(
                                            NameEquals(IdentifierName(t.item.Key)),
                                            null,
                                            t.expr
                                        )
                                    )
                            )
                    )
                )
            );
        }

        static string GetMemberName(this ISymbol symbol, IApiFilter apiFilter)
            => symbol.GetEei()
                .FirstOrNone(x => apiFilter.CanVisitApi(x))
                .Match(x => x.Name, () => symbol.Name);

        static IEnumerable<ISymbol> GetEei(this ISymbol symbol)
            => symbol switch
            {
                IMethodSymbol m   => m.ExplicitInterfaceImplementations,
                IEventSymbol e    => e.ExplicitInterfaceImplementations,
                IPropertySymbol p => p.ExplicitInterfaceImplementations,
                _                 => throw new ArgumentOutOfRangeException(nameof(symbol))
            };

        static ExplicitInterfaceSpecifierSyntax GetEeiSyntax(
            this ISymbol symbol,
            IApiFilter apiFilter
        )
        {
            var eei = symbol.GetEei().ToArray();

            return eei.Length > 0
                ? ExplicitInterfaceSpecifier(
                    ParseName(
                        GetEiiContainerTypeName(
                            eei,
                            apiFilter
                        )
                    )
                )
                : null;
        }

        static string GetEiiContainerTypeName<T>(
            IEnumerable<T> implementations,
            IApiFilter apiFilter
        )
            where T : ISymbol
            => implementations
                .Where(x => apiFilter.CanVisitApi(x))
                .Select(
                    t => NameVisitorFactory
                        .GetNameVisitor(NameOptions.UseAlias | NameOptions.WithGenericParameter)
                        .GetName(t.ContainingType)
                )
                .FirstOrDefault();

        static ParameterSyntax GetParameter(
            this IParameterSymbol p,
            IApiFilter apiFilter,
            bool isThisParameter = false
        )
            => Parameter(
                p.GetAttributes(apiFilter, true),
                TokenList(p.GetParameterModifiers(isThisParameter)),
                p.Type.GetTypeSyntax(),
                Identifier(p.Name),
                p.GetDefaultValueClause()
            );

        static IEnumerable<SyntaxToken> GetParameterModifiers(
            this IParameterSymbol parameter,
            bool isThisParameter
        )
        {
            if (isThisParameter) yield return Token(SyntaxKind.ThisKeyword);

            switch (parameter.RefKind)
            {
                case RefKind.None: break;
                case RefKind.Ref:
                    yield return Token(SyntaxKind.RefKeyword);

                    break;
                case RefKind.Out:
                    yield return Token(SyntaxKind.OutKeyword);

                    break;
                case RefKind.In:
                    yield return Token(SyntaxKind.InKeyword);

                    break;
            }

            if (parameter.IsParams) yield return Token(SyntaxKind.ParamsKeyword);
        }

        static IEnumerable<TypeParameterConstraintClauseSyntax>
            GetTypeParameterConstraints(this ImmutableArray<ITypeSymbol> typeArguments)
            => typeArguments
                .Cast<ITypeParameterSymbol>()
                .Where(
                    ta => ta.HasConstructorConstraint ||
                        ta.HasReferenceTypeConstraint ||
                        ta.HasValueTypeConstraint ||
                        ta.ConstraintTypes.Length > 0
                )
                .Select(
                    ta => TypeParameterConstraintClause(
                        IdentifierName(ta.Name),
                        SeparatedList(ta.GetTypeParameterConstraint())
                    )
                );

        static IEnumerable<TypeParameterConstraintSyntax> GetTypeParameterConstraint(
            this ITypeParameterSymbol symbol
        )
        {
            if (symbol.HasReferenceTypeConstraint)
                yield return ClassOrStructConstraint(SyntaxKind.ClassConstraint);

            if (symbol.HasValueTypeConstraint)
                yield return ClassOrStructConstraint(SyntaxKind.StructConstraint);

            if (symbol.ConstraintTypes.Length > 0)
                foreach (var t in symbol.ConstraintTypes)
                    yield return TypeConstraint(GetTypeSyntax(t));

            if (symbol.HasConstructorConstraint) yield return ConstructorConstraint();
        }

        static BaseListSyntax GetBaseTypeList(this INamedTypeSymbol symbol)
        {
            var baseTypeList =
                symbol.TypeKind != TypeKind.Class ||
                symbol.BaseType == null ||
                symbol.BaseType.GetDocumentationCommentId() == "T:System.Object"
                    ? (IReadOnlyList<INamedTypeSymbol>) symbol.AllInterfaces
                    : new[] {symbol.BaseType}.Concat(symbol.AllInterfaces).ToList();

            return baseTypeList.Count == 0
                ? null
                : BaseList(
                    SeparatedList<BaseTypeSyntax>(
                        from t in baseTypeList
                        select SimpleBaseType(GetTypeSyntax(t))
                    )
                );
        }

        static BaseListSyntax GetEnumBaseTypeList(this INamedTypeSymbol symbol)
        {
            var underlyingType = symbol.EnumUnderlyingType;

            return underlyingType?.GetDocumentationCommentId() == "T:System.Int32"
                ? null
                : BaseList(
                    SingletonSeparatedList<BaseTypeSyntax>(
                        SimpleBaseType(GetTypeSyntax(underlyingType))
                    )
                );
        }

        static TypeParameterListSyntax GetTypeParameters(ImmutableArray<ITypeSymbol> typeArguments)
            => typeArguments.IsEmpty
                ? null
                : TypeParameterList(
                    SeparatedList(
                        typeArguments.Cast<ITypeParameterSymbol>()
                            .Select(
                                t => TypeParameter(
                                    new SyntaxList<AttributeListSyntax>(),
                                    GetVarianceToken(t),
                                    Identifier(t.Name)
                                )
                            )
                    )
                );

        static SyntaxToken GetVarianceToken(this ITypeParameterSymbol t)
            => t.Variance switch
            {
                VarianceKind.In  => Token(SyntaxKind.InKeyword),
                VarianceKind.Out => Token(SyntaxKind.OutKeyword),
                _                => new SyntaxToken()
            };

        static IEnumerable<SyntaxToken> GetTypeModifiers(this INamedTypeSymbol symbol)
        {
            switch (symbol.DeclaredAccessibility)
            {
                case Accessibility.Protected:
                case Accessibility.ProtectedOrInternal:
                    yield return Token(SyntaxKind.ProtectedKeyword);

                    break;
                case Accessibility.Public:
                    yield return Token(SyntaxKind.PublicKeyword);

                    break;
            }

            if (symbol.TypeKind != TypeKind.Class) yield break;

            if (symbol.IsStatic)
            {
                yield return Token(SyntaxKind.StaticKeyword);
            }
            else
            {
                if (symbol.IsAbstract) yield return Token(SyntaxKind.AbstractKeyword);

                if (symbol.IsSealed) yield return Token(SyntaxKind.SealedKeyword);
            }
        }

        static IEnumerable<SyntaxToken> GetMemberModifiers(this ISymbol symbol)
        {
            if (symbol.ContainingType.TypeKind != TypeKind.Interface)
            {
                switch (symbol.DeclaredAccessibility)
                {
                    case Accessibility.Protected:
                    case Accessibility.ProtectedOrInternal:
                        yield return Token(SyntaxKind.ProtectedKeyword);

                        break;
                    case Accessibility.Public:
                        yield return Token(SyntaxKind.PublicKeyword);

                        break;
                }
            }

            if (symbol.IsStatic) yield return Token(SyntaxKind.StaticKeyword);

            if (symbol.IsAbstract && symbol.ContainingType.TypeKind != TypeKind.Interface)
                yield return Token(SyntaxKind.AbstractKeyword);

            if (symbol.IsVirtual) yield return Token(SyntaxKind.VirtualKeyword);

            if (symbol.IsOverride) yield return Token(SyntaxKind.OverrideKeyword);

            if (symbol.IsSealed) yield return Token(SyntaxKind.SealedKeyword);
        }

        static SyntaxToken? GetOperatorToken(this IMethodSymbol symbol)
            => symbol.Name switch
            {
                // unary
                "op_UnaryPlus"      => Token(SyntaxKind.PlusToken),
                "op_UnaryNegation"  => Token(SyntaxKind.MinusToken),
                "op_LogicalNot"     => Token(SyntaxKind.ExclamationToken),
                "op_OnesComplement" => Token(SyntaxKind.TildeToken),
                "op_Increment"      => Token(SyntaxKind.PlusPlusToken),
                "op_Decrement"      => Token(SyntaxKind.MinusMinusToken),
                "op_True"           => Token(SyntaxKind.TrueKeyword),
                "op_False"          => Token(SyntaxKind.FalseKeyword),
                // binary
                "op_Addition"    => Token(SyntaxKind.PlusToken),
                "op_Subtraction" => Token(SyntaxKind.MinusToken),
                "op_Multiply"    => Token(SyntaxKind.AsteriskToken),
                "op_Division"    => Token(SyntaxKind.SlashToken),
                "op_Modulus"     => Token(SyntaxKind.PercentToken),
                "op_BitwiseAnd"  => Token(SyntaxKind.AmpersandToken),
                "op_BitwiseOr"   => Token(SyntaxKind.BarToken),
                "op_ExclusiveOr" => Token(SyntaxKind.CaretToken),
                "op_RightShift"  => Token(SyntaxKind.GreaterThanGreaterThanToken),
                "op_LeftShift"   => Token(SyntaxKind.LessThanLessThanToken),
                // comparision
                "op_Equality"           => Token(SyntaxKind.EqualsEqualsToken),
                "op_Inequality"         => Token(SyntaxKind.ExclamationEqualsToken),
                "op_GreaterThan"        => Token(SyntaxKind.GreaterThanToken),
                "op_LessThan"           => Token(SyntaxKind.LessThanToken),
                "op_GreaterThanOrEqual" => Token(SyntaxKind.GreaterThanEqualsToken),
                "op_LessThanOrEqual"    => Token(SyntaxKind.LessThanEqualsToken),
                // conversion
                "op_Implicit" => Token(SyntaxKind.ImplicitKeyword),
                "op_Explicit" => Token(SyntaxKind.ExplicitKeyword),
                _             => null
            };

        static IEnumerable<AccessorDeclarationSyntax> GetPropertyAccessors(
            this IPropertySymbol propertySymbol,
            IApiFilter apiFilter
        )
        {
            var getAccessor = GetPropertyAccessorCore(
                propertySymbol,
                propertySymbol.GetMethod,
                SyntaxKind.GetAccessorDeclaration,
                SyntaxKind.GetKeyword,
                apiFilter
            );

            if (getAccessor != null) yield return getAccessor;

            var setAccessor = GetPropertyAccessorCore(
                propertySymbol,
                propertySymbol.SetMethod,
                SyntaxKind.SetAccessorDeclaration,
                SyntaxKind.SetKeyword,
                apiFilter
            );

            if (setAccessor != null) yield return setAccessor;
        }

        static AccessorDeclarationSyntax GetPropertyAccessorCore(
            IPropertySymbol propertySymbol,
            IMethodSymbol methodSymbol,
            SyntaxKind kind,
            SyntaxKind keyword,
            IApiFilter apiFilter
        )
        {
            if (methodSymbol == null) return null;

            return methodSymbol.DeclaredAccessibility switch
            {
                Accessibility.Protected           => Protected(),
                Accessibility.ProtectedOrInternal => Protected(),
                Accessibility.Public              => Public(),
                _ => methodSymbol.ExplicitInterfaceImplementations
                    .IsNotEmpty()
                    ? Public()
                    : null
            };

            AccessorDeclarationSyntax Protected()
                => propertySymbol.DeclaredAccessibility == Accessibility.Protected ||
                    propertySymbol.DeclaredAccessibility == Accessibility.ProtectedOrInternal
                        ? Public()
                        : AccessorDeclaration(
                            kind,
                            GetAttributes(methodSymbol, apiFilter),
                            TokenList(Token(SyntaxKind.ProtectedKeyword)),
                            Token(keyword),
                            (BlockSyntax) null,
                            Token(SyntaxKind.SemicolonToken)
                        );

            AccessorDeclarationSyntax Public()
                => AccessorDeclaration(
                    kind,
                    GetAttributes(methodSymbol, apiFilter),
                    new SyntaxTokenList(),
                    Token(keyword),
                    (BlockSyntax) null,
                    Token(SyntaxKind.SemicolonToken)
                );
        }

        static string RemoveBraces(string text) => BracesRegex.Replace(text, string.Empty);

        internal static TypeSyntax GetTypeSyntax(this ITypeSymbol type)
        {
            var name = NameVisitorFactory
                .GetNameVisitor(NameOptions.UseAlias | NameOptions.WithGenericParameter)
                .GetName(type);
            return ParseTypeName(name);
        }
    }
}
