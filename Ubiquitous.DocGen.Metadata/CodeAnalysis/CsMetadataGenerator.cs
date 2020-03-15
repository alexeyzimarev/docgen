using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Optional.Collections;
using Ubiquitous.DocGen.Metadata.Extensions;
using Ubiquitous.DocGen.Metadata.Models;
using Ubiquitous.DocGen.Metadata.Visitors;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static Microsoft.CodeAnalysis.SpecialType;
using Attribute = System.Attribute;

namespace Ubiquitous.DocGen.Metadata.CodeAnalysis
{
    public static class CsMetadataGenerator
    {
        static readonly Regex BracesRegex = new Regex(@"\s*\{(;|\s)*\}\s*$", RegexOptions.Compiled);

        internal static string GenerateSyntax(this ISymbol symbol, MemberType type, IApiFilter apiFilter) => GetSyntaxContent(type, symbol, apiFilter);

        public static List<string> GenerateNamedTypeModifiers(this INamedTypeSymbol symbol)
        {
            var modifiers  = new List<string>();
            var visibility = symbol.GetVisibility();

            modifiers.AddWhen(visibility != null, visibility);

            if (symbol.TypeKind == TypeKind.Class)
            {
                if (symbol.IsStatic)
                {
                    modifiers.Add("static");
                }
                else if (symbol.IsAbstract)
                {
                    modifiers.Add("abstract");
                }
                else if (symbol.IsSealed)
                {
                    modifiers.Add("sealed");
                }
            }

            var typeKind = symbol.TypeKind switch
            {
                TypeKind.Module    => "class",
                TypeKind.Class     => "class",
                TypeKind.Delegate  => "delegate",
                TypeKind.Enum      => "enum",
                TypeKind.Interface => "interface",
                TypeKind.Struct    => "struct",
                _                  => null
            };
            modifiers.AddWhen(typeKind != null, typeKind);

            return modifiers;
        }

        public static void GenerateField(this IFieldSymbol symbol, MetadataItem item)
        {
            var visibility = symbol.GetVisibility();

            item.Modifiers
                .AddWhen(visibility != null, visibility)
                .AddWhen(symbol.IsConst, "const")
                .AddWhen(symbol.IsStatic && !symbol.IsConst, "static")
                .AddWhen(symbol.IsReadOnly, "readonly")
                .AddWhen(symbol.IsVolatile, "volatile");
        }

        public static List<string> GeneratePropertyModifiers(this IPropertySymbol symbol)
        {
            var modifiers = symbol.GetMemberModifiersAsStrings();
            var propertyVisibility = symbol.GetVisibility();

            AddGetSet(symbol.GetMethod, "get");
            AddGetSet(symbol.SetMethod, "set");

            return modifiers;

            void AddGetSet(ISymbol methodSymbol, string method)
            {
                if (methodSymbol == null) return;

                var methodVisibility = methodSymbol.GetVisibility();
                if (propertyVisibility != null && methodVisibility == null) return;

                modifiers.Add(methodVisibility != propertyVisibility ? $"{methodVisibility} {method}" : method);
            }
        }

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

        internal static List<string> GetMemberModifiersAsStrings(this ISymbol symbol)
        {
            if (symbol.ContainingType.TypeKind == TypeKind.Interface)
            {
                return new List<string>();
            }

            var visibility = symbol.GetVisibility();

            return new List<string>()
                .AddWhen(visibility != null, visibility)
                .AddWhen(symbol.IsStatic, "static")
                .AddWhen(symbol.IsAbstract, "abstract")
                .AddWhen(symbol.IsOverride, "override")
                .AddWhen(symbol.IsVirtual && !symbol.IsSealed, "virtual")
                .AddWhen(symbol.IsSealed  && !symbol.IsVirtual, "sealed");
        }

        internal static void GenerateReference(this ISymbol symbol, ReferenceItem reference, bool asOverload = false)
            => symbol.Accept(new ReferenceItemVisitor(reference, asOverload));

        static string GetClassSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => RemoveBraces(
                ClassDeclaration(
                        symbol.GetAttributes(apiFilter),
                        TokenList(GetTypeModifiers(symbol)),
                        Identifier(symbol.Name),
                        symbol.GetTypeParameters(),
                        symbol.GetBaseTypeList(),
                        List(symbol.GetTypeParameterConstraints()),
                        new SyntaxList<MemberDeclarationSyntax>()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );

        static string GetEnumSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => RemoveBraces(
                EnumDeclaration(
                        GetAttributes(symbol, apiFilter),
                        TokenList(GetTypeModifiers(symbol)),
                        Identifier(symbol.Name),
                        GetEnumBaseTypeList(symbol),
                        new SeparatedSyntaxList<EnumMemberDeclarationSyntax>()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );

        static string GetInterfaceSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => RemoveBraces(
                InterfaceDeclaration(
                        GetAttributes(symbol, apiFilter),
                        TokenList(
                            GetTypeModifiers(symbol)
                        ),
                        Identifier(symbol.Name),
                        GetTypeParameters(symbol),
                        GetBaseTypeList(symbol),
                        List(
                            GetTypeParameterConstraints(symbol)
                        ),
                        new SyntaxList<MemberDeclarationSyntax>()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );

        static string GetStructSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => RemoveBraces(
                StructDeclaration(
                        GetAttributes(symbol, apiFilter),
                        TokenList(
                            GetTypeModifiers(symbol)
                        ),
                        Identifier(symbol.Name),
                        GetTypeParameters(symbol),
                        GetBaseTypeList(symbol),
                        List(
                            GetTypeParameterConstraints(symbol)
                        ),
                        new SyntaxList<MemberDeclarationSyntax>()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );

        static string GetDelegateSyntax(this INamedTypeSymbol symbol, IApiFilter apiFilter)
            => DelegateDeclaration(
                    GetAttributes(symbol, apiFilter),
                    TokenList(
                        GetTypeModifiers(symbol)
                    ),
                    GetTypeSyntax(symbol.DelegateInvokeMethod.ReturnType),
                    Identifier(symbol.Name),
                    GetTypeParameters(symbol),
                    ParameterList(
                        SeparatedList(
                            from p in symbol.DelegateInvokeMethod.Parameters
                            select GetParameter(p, apiFilter)
                        )
                    ),
                    List(
                        GetTypeParameterConstraints(symbol)
                    )
                )
                .NormalizeWhitespace()
                .ToString();

        static string GetMethodSyntax(this IMethodSymbol symbol, IApiFilter apiFilter)
        {
            ExplicitInterfaceSpecifierSyntax eii = null;

            if (symbol.ExplicitInterfaceImplementations.IsNotEmpty())
            {
                eii = ExplicitInterfaceSpecifier(
                    ParseName(GetEiiContainerTypeName(symbol.ExplicitInterfaceImplementations, apiFilter))
                );
            }

            return MethodDeclaration(
                    GetAttributes(symbol, apiFilter),
                    TokenList(GetMemberModifiers(symbol)),
                    GetTypeSyntax(symbol.ReturnType),
                    eii,
                    Identifier(GetMemberName(symbol, apiFilter)),
                    GetTypeParameters(symbol),
                    ParameterList(
                        SeparatedList(
                            symbol.Parameters.Select(
                                (p, i)
                                    => GetParameter(p, apiFilter, i == 0 && symbol.IsExtensionMethod)
                            )
                        )
                    ),
                    List(GetTypeParameterConstraints(symbol)),
                    null,
                    null
                )
                .NormalizeWhitespace()
                .ToString();
        }

        static string GetOperatorSyntax(this IMethodSymbol symbol, IApiFilter apiFilter)
        {
            var operatorToken = GetOperatorToken(symbol);

            if (operatorToken == null)
            {
                return "Not supported in C#";
            }

            if (operatorToken.Value.Kind() == SyntaxKind.ImplicitKeyword ||
                operatorToken.Value.Kind() == SyntaxKind.ExplicitKeyword)
            {
                return ConversionOperatorDeclaration(
                        GetAttributes(symbol, apiFilter),
                        TokenList(
                            GetMemberModifiers(symbol)
                        ),
                        operatorToken.Value,
                        GetTypeSyntax(symbol.ReturnType),
                        ParameterList(
                            SeparatedList(
                                from p in symbol.Parameters
                                select GetParameter(p, apiFilter)
                            )
                        ),
                        null,
                        null
                    )
                    .NormalizeWhitespace()
                    .ToString();
            }

            return OperatorDeclaration(
                    GetAttributes(symbol, apiFilter),
                    TokenList(
                        GetMemberModifiers(symbol)
                    ),
                    GetTypeSyntax(symbol.ReturnType),
                    operatorToken.Value,
                    ParameterList(
                        SeparatedList(
                            from p in symbol.Parameters
                            select GetParameter(p, apiFilter)
                        )
                    ),
                    null,
                    null
                )
                .NormalizeWhitespace()
                .ToString();
        }

        static string GetConstructorSyntax(this IMethodSymbol symbol, IApiFilter apiFilter)
            => ConstructorDeclaration(
                    GetAttributes(symbol, apiFilter),
                    TokenList(
                        GetMemberModifiers(symbol)
                    ),
                    Identifier(symbol.ContainingType.Name),
                    ParameterList(
                        SeparatedList(
                            from p in symbol.Parameters
                            select GetParameter(p, apiFilter)
                        )
                    ),
                    null,
                    (BlockSyntax) null
                )
                .NormalizeWhitespace()
                .ToString();

        static string GetFieldSyntax(this IFieldSymbol symbol, IApiFilter apiFilter)
        {
            if (symbol.ContainingType.TypeKind == TypeKind.Enum)
            {
                return EnumMemberDeclaration(
                        GetAttributes(symbol, apiFilter),
                        Identifier(symbol.Name),
                        GetDefaultValueClause(symbol)
                    )
                    .NormalizeWhitespace()
                    .ToString();
            }

            return FieldDeclaration(
                    GetAttributes(symbol, apiFilter),
                    TokenList(
                        GetMemberModifiers(symbol)
                    ),
                    VariableDeclaration(
                        GetTypeSyntax(symbol.Type),
                        SingletonSeparatedList(
                            VariableDeclarator(
                                Identifier(symbol.Name),
                                null,
                                GetDefaultValueClause(symbol)
                            )
                        )
                    )
                )
                .NormalizeWhitespace()
                .ToString()
                .TrimEnd(';');
        }

        static string GetEventSyntax(this IEventSymbol symbol, IApiFilter apiFilter)
        {
            ExplicitInterfaceSpecifierSyntax eii = null;

            if (symbol.ExplicitInterfaceImplementations.Length > 0)
            {
                eii = ExplicitInterfaceSpecifier(
                    ParseName(GetEiiContainerTypeName(symbol.ExplicitInterfaceImplementations, apiFilter))
                );
            }

            return RemoveBraces(
                EventDeclaration(
                        GetAttributes(symbol, apiFilter),
                        TokenList(GetMemberModifiers(symbol)),
                        Token(SyntaxKind.EventKeyword),
                        GetTypeSyntax(symbol.Type),
                        eii,
                        Identifier(GetMemberName(symbol, apiFilter)),
                        AccessorList()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );
        }

        static string GetPropertySyntax(this IPropertySymbol symbol, IApiFilter apiFilter)
        {
            string result;

            ExplicitInterfaceSpecifierSyntax eii = null;

            if (symbol.ExplicitInterfaceImplementations.Length > 0)
            {
                eii = ExplicitInterfaceSpecifier(
                    ParseName(GetEiiContainerTypeName(symbol.ExplicitInterfaceImplementations, apiFilter))
                );
            }

            if (symbol.IsIndexer)
            {
                result = IndexerDeclaration(
                        GetAttributes(symbol, apiFilter),
                        TokenList(GetMemberModifiers(symbol)),
                        GetTypeSyntax(symbol.Type),
                        eii,
                        BracketedParameterList(
                            SeparatedList(symbol.Parameters.Select(p => GetParameter(p, apiFilter)))
                        ),
                        AccessorList(List(GetPropertyAccessors(symbol, apiFilter)))
                    )
                    .NormalizeWhitespace()
                    .ToString();
            }
            else
            {
                result = PropertyDeclaration(
                        GetAttributes(symbol, apiFilter),
                        TokenList(GetMemberModifiers(symbol)),
                        GetTypeSyntax(symbol.Type),
                        eii,
                        Identifier(GetMemberName(symbol, apiFilter)),
                        AccessorList(List(GetPropertyAccessors(symbol, apiFilter)))
                    )
                    .NormalizeWhitespace()
                    .ToString();
            }

            result = Regex.Replace(result, @"\s*\{\s*get;\s*set;\s*}\s*$", " { get; set; }");
            result = Regex.Replace(result, @"\s*\{\s*get;\s*}\s*$", " { get; }");
            result = Regex.Replace(result, @"\s*\{\s*set;\s*}\s*$", " { set; }");
            result = Regex.Replace(result, @"\s*\{\s*get;\s*protected set;\s*}\s*$", " { get; protected set; }");
            result = Regex.Replace(result, @"\s*\{\s*protected get;\s*set;\s*}\s*$", " { protected get; set; }");
            return result;
        }

        static SyntaxList<AttributeListSyntax> GetAttributes(this ISymbol symbol, IApiFilter apiFilter, bool inOneLine = false)
        {
            var attributes = symbol.GetAttributes();
            if (attributes.Length <= 0) return new SyntaxList<AttributeListSyntax>();

            var attrList = attributes
                .Where(attr => !(attr.AttributeClass is IErrorTypeSymbol))
                .Where(attr => attr?.AttributeConstructor != null)
                .Where(attr => apiFilter.CanVisitAttribute(attr.AttributeConstructor))
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
            var attrTypeName = NameVisitorFactory.GetCSharp(NameOptions.None).GetName(attr.AttributeClass);

            if (attrTypeName.EndsWith(nameof(Attribute), StringComparison.Ordinal))
            {
                attrTypeName = attrTypeName.Remove(attrTypeName.Length - nameof(Attribute).Length);
            }

            if (attr.ConstructorArguments.Length == 0 && attr.NamedArguments.Length == 0)
            {
                return Attribute(ParseName(attrTypeName));
            }

            return Attribute(
                ParseName(attrTypeName),
                AttributeArgumentList(
                    SeparatedList(
                        attr.ConstructorArguments
                            .Select(GetLiteralExpression)
                            .Where(expr => expr != null)
                            .Select(AttributeArgument)
                            .Concat(
                                attr.NamedArguments
                                    .Select(item => new {item, expr = GetLiteralExpression(item.Value)})
                                    .Where(t => t.expr != null)
                                    .Select(
                                        t => AttributeArgument(
                                            NameEquals(IdentifierName(t.item.Key)), null,
                                            t.expr
                                        )
                                    )
                            )
                    )
                )
            );
        }

        static string GetSymbolName<T>(T symbol, Func<T, ImmutableArray<T>> getExplicitInterfaceImplementations, IApiFilter apiFilter)
            where T : ISymbol
            => getExplicitInterfaceImplementations(symbol)
                .FirstOrNone(x => apiFilter.CanVisitApi(x))
                .Match(x => x.Name, () => symbol.Name);

        static string GetMemberName(this IMethodSymbol symbol, IApiFilter apiFilter)
            => GetSymbolName(symbol, x => x.ExplicitInterfaceImplementations, apiFilter);

        static string GetMemberName(this IEventSymbol symbol, IApiFilter apiFilter)
            => GetSymbolName(symbol, x => x.ExplicitInterfaceImplementations, apiFilter);

        static string GetMemberName(this IPropertySymbol symbol, IApiFilter apiFilter)
            => GetSymbolName(symbol, x => x.ExplicitInterfaceImplementations, apiFilter);

        static string GetEiiContainerTypeName<T>(ImmutableArray<T> implementations, IApiFilter apiFilter)
            where T : ISymbol
            => implementations
                .Where(x => apiFilter.CanVisitApi(x))
                .Select(
                    t =>
                        NameVisitorFactory
                            .GetCSharp(NameOptions.UseAlias | NameOptions.WithGenericParameter)
                            .GetName(t.ContainingType)
                )
                .FirstOrDefault();

        static ParameterSyntax GetParameter(this IParameterSymbol p, IApiFilter apiFilter, bool isThisParameter = false)
            => Parameter(
                GetAttributes(p, apiFilter, true),
                TokenList(GetParameterModifiers(p, isThisParameter)),
                GetTypeSyntax(p.Type),
                Identifier(p.Name),
                GetDefaultValueClause(p)
            );

        static IEnumerable<SyntaxToken> GetParameterModifiers(this IParameterSymbol parameter, bool isThisParameter)
        {
            if (isThisParameter)
            {
                yield return Token(SyntaxKind.ThisKeyword);
            }

            switch (parameter.RefKind)
            {
                case RefKind.None:
                    break;
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

            if (parameter.IsParams)
            {
                yield return Token(SyntaxKind.ParamsKeyword);
            }
        }

        static EqualsValueClauseSyntax GetDefaultValueClause(this IParameterSymbol symbol)
            => symbol.HasExplicitDefaultValue
                ? GetDefaultValueClauseCore(symbol.ExplicitDefaultValue, symbol.Type)
                : null;

        static EqualsValueClauseSyntax GetDefaultValueClause(this IFieldSymbol symbol)
            => symbol.IsConst
                ? GetDefaultValueClauseCore(
                    symbol.ConstantValue,
                    symbol.ContainingType.TypeKind == TypeKind.Enum
                        ? ((INamedTypeSymbol) symbol.Type).EnumUnderlyingType
                        : symbol.Type
                )
                : null;

        static EqualsValueClauseSyntax GetDefaultValueClauseCore(object value, ITypeSymbol type)
        {
            var expr = GetLiteralExpression(value, type);
            return expr != null ? EqualsValueClause(expr) : null;
        }

        static ExpressionSyntax GetLiteralExpression(this TypedConstant constant)
        {
            if (constant.Type.TypeKind == TypeKind.Array)
            {
                if (constant.Values == null)
                {
                    return GetLiteralExpression(null, constant.Type);
                }

                var items = constant.Values.Select(GetLiteralExpression);

                if (items.All(x => x != null))
                {
                    return ArrayCreationExpression(
                        (ArrayTypeSyntax) GetTypeSyntax(constant.Type),
                        InitializerExpression(
                            SyntaxKind.ArrayInitializerExpression,
                            SeparatedList(
                                from value in constant.Values
                                select GetLiteralExpression(value)
                            )
                        )
                    );
                }

                return ArrayCreationExpression(
                    (ArrayTypeSyntax) GetTypeSyntax(constant.Type)
                );
            }

            var expr = GetLiteralExpression(constant.Value, constant.Type);

            if (expr == null)
            {
                return null;
            }

            return constant.Type.SpecialType switch
            {
                System_SByte  => CastExpression(PredefinedType(Token(SyntaxKind.SByteKeyword)), expr),
                System_Byte   => CastExpression(PredefinedType(Token(SyntaxKind.ByteKeyword)), expr),
                System_Int16  => CastExpression(PredefinedType(Token(SyntaxKind.ShortKeyword)), expr),
                System_UInt16 => CastExpression(PredefinedType(Token(SyntaxKind.UShortKeyword)), expr),
                _             => expr
            };
        }

        static ExpressionSyntax GetLiteralExpression(object value, ITypeSymbol type)
        {
            if (value == null)
            {
                return type.IsValueType
                    ? (ExpressionSyntax) DefaultExpression(GetTypeSyntax(type))
                    : LiteralExpression(
                        SyntaxKind.NullLiteralExpression,
                        Token(SyntaxKind.NullKeyword)
                    );
            }

            var result = GetLiteralExpressionCore(value, type);

            if (result != null)
            {
                return result;
            }

            if (type.TypeKind == TypeKind.Enum)
            {
                var namedType = (INamedTypeSymbol) type;
                var enumType  = GetTypeSyntax(namedType);

                var isFlags = namedType
                    .GetAttributes()
                    .Any(attr => attr.AttributeClass.GetDocumentationCommentId() == "T:System.FlagsAttribute");

                var pairs = namedType.GetMembers()
                    .OfType<IFieldSymbol>()
                    .Where(member => member.IsConst && member.HasConstantValue)
                    .Select(member => new {member.Name, member.ConstantValue});

                if (isFlags)
                {
                    var exprs = pairs
                        .Where(pair => HasFlag(namedType.EnumUnderlyingType, value, pair.ConstantValue))
                        .Select(
                            pair => MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression, enumType,
                                IdentifierName(pair.Name)
                            )
                        )
                        .ToList();

                    if (exprs.Count > 0)
                    {
                        return exprs.Aggregate<ExpressionSyntax>(
                            (x, y) =>
                                BinaryExpression(SyntaxKind.BitwiseOrExpression, x, y)
                        );
                    }
                }
                else
                {
                    var expr = pairs
                        .Where(pair => Equals(value, pair.ConstantValue))
                        .Select(
                            pair => MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression, enumType,
                                IdentifierName(pair.Name)
                            )
                        )
                        .FirstOrDefault();

                    if (expr != null)
                    {
                        return expr;
                    }
                }

                return CastExpression(
                    enumType,
                    GetLiteralExpressionCore(
                        value,
                        namedType.EnumUnderlyingType
                    )
                );
            }

            if (value is ITypeSymbol symbol)
            {
                return TypeOfExpression(GetTypeSyntax(symbol));
            }

            Debug.Fail("Unknown default value!");
            return null;
        }

        static bool HasFlag(ITypeSymbol type, object value, object constantValue)
        {
            switch (type.SpecialType)
            {
                case System_SByte:
                {
                    var v  = (sbyte) value;
                    var cv = (sbyte) constantValue;
                    return cv == 0 ? v == 0 : (v & cv) == cv;
                }
                case System_Byte:
                {
                    var v  = (byte) value;
                    var cv = (byte) constantValue;
                    return cv == 0 ? v == 0 : (v & cv) == cv;
                }
                case System_Int16:
                {
                    var v  = (short) value;
                    var cv = (short) constantValue;
                    return cv == 0 ? v == 0 : (v & cv) == cv;
                }
                case System_UInt16:
                {
                    var v  = (ushort) value;
                    var cv = (ushort) constantValue;
                    return cv == 0 ? v == 0 : (v & cv) == cv;
                }
                case System_Int32:
                {
                    var v  = (int) value;
                    var cv = (int) constantValue;
                    return cv == 0 ? v == 0 : (v & cv) == cv;
                }
                case System_UInt32:
                {
                    var v  = (uint) value;
                    var cv = (uint) constantValue;
                    return cv == 0 ? v == 0 : (v & cv) == cv;
                }
                case System_Int64:
                {
                    var v  = (long) value;
                    var cv = (long) constantValue;
                    return cv == 0 ? v == 0 : (v & cv) == cv;
                }
                case System_UInt64:
                {
                    var v  = (ulong) value;
                    var cv = (ulong) constantValue;
                    return cv == 0 ? v == 0 : (v & cv) == cv;
                }
                default:
                    return false;
            }
        }

        static ExpressionSyntax GetLiteralExpressionCore(object value, ITypeSymbol type)
        {
            return type.SpecialType switch
            {
                System_Boolean => LiteralExpression(
                    (bool) value
                        ? SyntaxKind.TrueLiteralExpression
                        : SyntaxKind.FalseLiteralExpression
                ),
                System_Char    => CharLiteralExpression(),
                System_SByte   => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((sbyte) value)),
                System_Byte    => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((byte) value)),
                System_Int16   => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((short) value)),
                System_UInt16  => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((ushort) value)),
                System_Int32   => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((int) value)),
                System_UInt32  => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((uint) value)),
                System_Int64   => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((long) value)),
                System_UInt64  => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((ulong) value)),
                System_Decimal => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((decimal) value)),
                System_Single  => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((float) value)),
                System_Double  => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal((double) value)),
                System_String  => LiteralExpression(SyntaxKind.StringLiteralExpression, Literal((string) value)),
                _              => null
            };

            LiteralExpressionSyntax CharLiteralExpression()
            {
                var ch       = (char) value;
                var category = char.GetUnicodeCategory(ch);

                return category switch
                {
                    System.Globalization.UnicodeCategory.Surrogate => LiteralExpression(
                        SyntaxKind.CharacterLiteralExpression,
                        Literal("'\\u" + ((int) ch).ToString("X4") + "'", ch)
                    ),
                    _ => LiteralExpression(
                        SyntaxKind.CharacterLiteralExpression,
                        Literal((char) value)
                    )
                };
            }
        }

        static IEnumerable<TypeParameterConstraintClauseSyntax> GetTypeParameterConstraints(this INamedTypeSymbol symbol)
            => symbol.TypeArguments.Cast<ITypeParameterSymbol>()
                .Where(
                    ta
                        => ta.HasConstructorConstraint || ta.HasReferenceTypeConstraint || ta.HasValueTypeConstraint ||
                        ta.ConstraintTypes.Length > 0
                )
                .Select(
                    ta => TypeParameterConstraintClause(
                        IdentifierName(ta.Name),
                        SeparatedList(GetTypeParameterConstraint(ta))
                    )
                );

        static IEnumerable<TypeParameterConstraintClauseSyntax> GetTypeParameterConstraints(this IMethodSymbol symbol)
            => symbol
                .TypeArguments
                .Cast<ITypeParameterSymbol>()
                .Where(
                    ta =>
                        ta.HasConstructorConstraint
                     || ta.HasReferenceTypeConstraint
                     || ta.HasValueTypeConstraint
                     || ta.ConstraintTypes.Length > 0
                )
                .Select(
                    ta => TypeParameterConstraintClause(
                        IdentifierName(ta.Name),
                        SeparatedList(GetTypeParameterConstraint(ta))
                    )
                );

        static IEnumerable<TypeParameterConstraintSyntax> GetTypeParameterConstraint(this ITypeParameterSymbol symbol)
        {
            if (symbol.HasReferenceTypeConstraint)
            {
                yield return ClassOrStructConstraint(SyntaxKind.ClassConstraint);
            }

            if (symbol.HasValueTypeConstraint)
            {
                yield return ClassOrStructConstraint(SyntaxKind.StructConstraint);
            }

            if (symbol.ConstraintTypes.Length > 0)
            {
                foreach (var t in symbol.ConstraintTypes)
                {
                    yield return TypeConstraint(GetTypeSyntax(t));
                }
            }

            if (symbol.HasConstructorConstraint)
            {
                yield return ConstructorConstraint();
            }
        }

        static BaseListSyntax GetBaseTypeList(this INamedTypeSymbol symbol)
        {
            var baseTypeList = symbol.TypeKind              != TypeKind.Class
             || symbol.BaseType                             == null ||
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
                        SimpleBaseType(
                            GetTypeSyntax(underlyingType)
                        )
                    )
                );
        }

        static TypeParameterListSyntax GetTypeParameters(this INamedTypeSymbol symbol)
            => symbol.TypeArguments.IsNotEmpty()
                ? null
                : TypeParameterList(
                    SeparatedList(
                        from ITypeParameterSymbol t in symbol.TypeArguments
                        select TypeParameter(
                            new SyntaxList<AttributeListSyntax>(),
                            GetVarianceToken(t),
                            Identifier(t.Name)
                        )
                    )
                );

        static TypeParameterListSyntax GetTypeParameters(this IMethodSymbol symbol)
            => symbol.TypeArguments.Length == 0
                ? null
                : TypeParameterList(
                    SeparatedList(
                        from ITypeParameterSymbol t in symbol.TypeArguments
                        select TypeParameter(
                            new SyntaxList<AttributeListSyntax>(),
                            GetVarianceToken(t),
                            Identifier(t.Name)
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

            if (symbol.TypeKind == TypeKind.Class)
            {
                if (symbol.IsStatic)
                {
                    yield return Token(SyntaxKind.StaticKeyword);
                }
                else
                {
                    if (symbol.IsAbstract)
                    {
                        yield return Token(SyntaxKind.AbstractKeyword);
                    }

                    if (symbol.IsSealed)
                    {
                        yield return Token(SyntaxKind.SealedKeyword);
                    }
                }
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

            if (symbol.IsStatic)
            {
                yield return Token(SyntaxKind.StaticKeyword);
            }

            if (symbol.IsAbstract && symbol.ContainingType.TypeKind != TypeKind.Interface)
            {
                yield return Token(SyntaxKind.AbstractKeyword);
            }

            if (symbol.IsVirtual)
            {
                yield return Token(SyntaxKind.VirtualKeyword);
            }

            if (symbol.IsOverride)
            {
                yield return Token(SyntaxKind.OverrideKeyword);
            }

            if (symbol.IsSealed)
            {
                yield return Token(SyntaxKind.SealedKeyword);
            }
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

        static IEnumerable<AccessorDeclarationSyntax> GetPropertyAccessors(this IPropertySymbol propertySymbol, IApiFilter apiFilter)
        {
            var getAccessor = GetPropertyAccessorCore(
                propertySymbol, propertySymbol.GetMethod,
                SyntaxKind.GetAccessorDeclaration, SyntaxKind.GetKeyword, apiFilter
            );

            if (getAccessor != null)
            {
                yield return getAccessor;
            }

            var setAccessor = GetPropertyAccessorCore(
                propertySymbol, propertySymbol.SetMethod,
                SyntaxKind.SetAccessorDeclaration, SyntaxKind.SetKeyword, apiFilter
            );

            if (setAccessor != null)
            {
                yield return setAccessor;
            }
        }

        static AccessorDeclarationSyntax GetPropertyAccessorCore(
            IPropertySymbol propertySymbol, IMethodSymbol methodSymbol,
            SyntaxKind kind, SyntaxKind keyword, IApiFilter apiFilter
        )
        {
            if (methodSymbol == null)
                return null;

            return methodSymbol.DeclaredAccessibility switch
            {
                Accessibility.Protected           => Protected(),
                Accessibility.ProtectedOrInternal => Protected(),
                Accessibility.Public              => Public(),
                _ => methodSymbol.ExplicitInterfaceImplementations.IsNotEmpty()
                    ? Public()
                    : null
            };

            AccessorDeclarationSyntax Protected()
                => propertySymbol.DeclaredAccessibility  == Accessibility.Protected ||
                    propertySymbol.DeclaredAccessibility == Accessibility.ProtectedOrInternal
                        ? Public()
                        : AccessorDeclaration(
                            kind, GetAttributes(methodSymbol, apiFilter),
                            TokenList(Token(SyntaxKind.ProtectedKeyword)), Token(keyword), (BlockSyntax) null,
                            Token(SyntaxKind.SemicolonToken)
                        );

            AccessorDeclarationSyntax Public()
                => AccessorDeclaration(
                    kind, GetAttributes(methodSymbol, apiFilter),
                    new SyntaxTokenList(), Token(keyword), (BlockSyntax) null, Token(SyntaxKind.SemicolonToken)
                );
        }

        static string RemoveBraces(string text) => BracesRegex.Replace(text, string.Empty);

        static TypeSyntax GetTypeSyntax(this ITypeSymbol type)
        {
            var name = NameVisitorFactory
                .GetCSharp(NameOptions.UseAlias | NameOptions.WithGenericParameter)
                .GetName(type);
            return ParseTypeName(name);
        }

        static string GetVisibility(this ISymbol symbol)
            => symbol.DeclaredAccessibility switch
            {
                Accessibility.Protected           => "protected",
                Accessibility.ProtectedOrInternal => "protected",
                Accessibility.Public              => "public",
                _                                 => null
            };
    }
}