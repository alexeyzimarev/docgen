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
using Ubiquitous.DocFx.Markdown.Extensions;
using Ubiquitous.DocFx.Markdown.Models;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static Microsoft.CodeAnalysis.SpecialType;
using Attribute = System.Attribute;

namespace Ubiquitous.DocFx.Markdown.Visitors
{
    public class CsMetadataGenerator
    {
        static readonly Regex BracesRegex = new Regex(@"\s*\{(;|\s)*\}\s*$", RegexOptions.Compiled);

        public static void DefaultVisit(ISymbol symbol, MetadataItem item)
        {
            item.DisplayName = NameVisitorFactory
                .GetCSharp(NameOptions.WithGenericParameter | NameOptions.WithParameter).GetName(symbol);
            item.DisplayNameWithType = NameVisitorFactory
                .GetCSharp(NameOptions.WithType | NameOptions.WithGenericParameter | NameOptions.WithParameter)
                .GetName(symbol);
            item.DisplayQualifiedName = NameVisitorFactory
                .GetCSharp(NameOptions.Qualified | NameOptions.WithGenericParameter | NameOptions.WithParameter)
                .GetName(symbol);
        }

        internal void GenerateSyntax(
            MemberType type, ISymbol symbol, SyntaxDetail syntax, MetadataSymbolVisitor adapter
        )
        {
            var syntaxStr = GetSyntaxContent(type, symbol, adapter);

            Debug.Assert(!string.IsNullOrEmpty(syntaxStr));
            if (string.IsNullOrEmpty(syntaxStr)) return;

            syntax.Content = syntaxStr;
        }

        public static void GenerateNamedType(INamedTypeSymbol symbol, MetadataItem item)
        {
            var modifiers = new List<string>();
            var visiblity = GetVisibility(symbol.DeclaredAccessibility);

            modifiers.AddWhen(visiblity != null, visiblity);

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

            item.Modifiers = modifiers;
        }

        public static void GenerateMethod(IMethodSymbol symbol, MetadataItem item)
            => AddMemberModifiers(item.Modifiers, symbol);

        public static void GenerateField(IFieldSymbol symbol, MetadataItem item)
        {
            var visibility = GetVisibility(symbol.DeclaredAccessibility);

            item.Modifiers
                .AddWhen(visibility != null, visibility)
                .AddWhen(symbol.IsConst, "const")
                .AddWhen(symbol.IsStatic && !symbol.IsConst, "static")
                .AddWhen(symbol.IsReadOnly, "readonly")
                .AddWhen(symbol.IsVolatile, "volatile");
        }

        public static void GenerateProperty(IPropertySymbol symbol, MetadataItem item)
        {
            var modifiers = new List<string>();

            var propertyVisibility = GetVisibility(symbol.DeclaredAccessibility);

            AddMemberModifiers(item.Modifiers, symbol);

            AddGetSet(symbol.GetMethod, "get");
            AddGetSet(symbol.SetMethod, "set");

            item.Modifiers = modifiers;

            void AddGetSet(ISymbol methodSymbol, string method)
            {
                if (methodSymbol == null) return;

                var methodVisibility = GetVisibility(methodSymbol.DeclaredAccessibility);
                if (propertyVisibility != null && methodVisibility == null) return;

                modifiers.Add(methodVisibility != propertyVisibility ? $"{methodVisibility} {method}" : method);
            }
        }

        public static void GenerateEvent(IEventSymbol symbol, MetadataItem item)
            => AddMemberModifiers(item.Modifiers, symbol);

        string GetSyntaxContent(MemberType typeKind, ISymbol symbol, MetadataSymbolVisitor adapter)
            => typeKind switch
            {
                MemberType.Class       => GetClassSyntax((INamedTypeSymbol) symbol, adapter.FilterVisitor),
                MemberType.Enum        => GetEnumSyntax((INamedTypeSymbol) symbol, adapter.FilterVisitor),
                MemberType.Interface   => GetInterfaceSyntax((INamedTypeSymbol) symbol, adapter.FilterVisitor),
                MemberType.Struct      => GetStructSyntax((INamedTypeSymbol) symbol, adapter.FilterVisitor),
                MemberType.Delegate    => GetDelegateSyntax((INamedTypeSymbol) symbol, adapter.FilterVisitor),
                MemberType.Method      => GetMethodSyntax((IMethodSymbol) symbol, adapter.FilterVisitor),
                MemberType.Operator    => GetOperatorSyntax((IMethodSymbol) symbol, adapter.FilterVisitor),
                MemberType.Constructor => GetConstructorSyntax((IMethodSymbol) symbol, adapter.FilterVisitor),
                MemberType.Field       => GetFieldSyntax((IFieldSymbol) symbol, adapter.FilterVisitor),
                MemberType.Event       => GetEventSyntax((IEventSymbol) symbol, adapter.FilterVisitor),
                MemberType.Property    => GetPropertySyntax((IPropertySymbol) symbol, adapter.FilterVisitor),
                _                      => null
            };

        static List<string> AddMemberModifiers(List<string> modifiers, ISymbol symbol)
        {
            if (symbol.ContainingType.TypeKind == TypeKind.Interface)
            {
                return modifiers;
            }

            var visibility = GetVisibility(symbol.DeclaredAccessibility);
            modifiers = new List<string>()
                .AddWhen(visibility != null, visibility)
                .AddWhen(symbol.IsStatic, "static")
                .AddWhen(symbol.IsAbstract, "abstract")
                .AddWhen(symbol.IsOverride, "override")
                .AddWhen(symbol.IsVirtual && !symbol.IsSealed, "virtual")
                .AddWhen(symbol.IsSealed  && !symbol.IsVirtual, "sealed");
            return modifiers;
        }

        static void GenerateReference(ISymbol symbol, ReferenceItem reference, bool asOverload = false)
            => symbol.Accept(new ReferenceItemVisitor(reference, asOverload));

        string GetClassSyntax(INamedTypeSymbol symbol, IFilterVisitor filterVisitor) =>
            RemoveBraces(
                ClassDeclaration(
                    GetAttributes(symbol, filterVisitor),
                    TokenList(GetTypeModifiers(symbol)),
                    Identifier(symbol.Name),
                    GetTypeParameters(symbol),
                    GetBaseTypeList(symbol),
                    List(GetTypeParameterConstraints(symbol)),
                    new SyntaxList<MemberDeclarationSyntax>()
                ).NormalizeWhitespace().ToString()
            );

        string GetEnumSyntax(INamedTypeSymbol symbol, IFilterVisitor filterVisitor) =>
            RemoveBraces(
                EnumDeclaration(
                        GetAttributes(symbol, filterVisitor),
                        TokenList(GetTypeModifiers(symbol)),
                        Identifier(symbol.Name),
                        GetEnumBaseTypeList(symbol),
                        new SeparatedSyntaxList<EnumMemberDeclarationSyntax>()
                    )
                    .NormalizeWhitespace()
                    .ToString()
            );

        string GetInterfaceSyntax(INamedTypeSymbol symbol, IFilterVisitor filterVisitor) =>
            RemoveBraces(
                InterfaceDeclaration(
                    GetAttributes(symbol, filterVisitor),
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
                ).NormalizeWhitespace().ToString()
            );

        string GetStructSyntax(INamedTypeSymbol symbol, IFilterVisitor filterVisitor) =>
            RemoveBraces(
                StructDeclaration(
                    GetAttributes(symbol, filterVisitor),
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
                ).NormalizeWhitespace().ToString()
            );

        static string GetDelegateSyntax(INamedTypeSymbol symbol, IFilterVisitor filterVisitor) =>
            DelegateDeclaration(
                GetAttributes(symbol, filterVisitor),
                TokenList(
                    GetTypeModifiers(symbol)
                ),
                GetTypeSyntax(symbol.DelegateInvokeMethod.ReturnType),
                Identifier(symbol.Name),
                GetTypeParameters(symbol),
                ParameterList(
                    SeparatedList(
                        from p in symbol.DelegateInvokeMethod.Parameters
                        select GetParameter(p, filterVisitor)
                    )
                ),
                List(
                    GetTypeParameterConstraints(symbol)
                )
            ).NormalizeWhitespace().ToString();

        static string GetMethodSyntax(IMethodSymbol symbol, IFilterVisitor filterVisitor)
        {
            ExplicitInterfaceSpecifierSyntax eii = null;
            if (symbol.ExplicitInterfaceImplementations.NotEmpty())
            {
                eii = ExplicitInterfaceSpecifier(
                    ParseName(GetEiiContainerTypeName(symbol.ExplicitInterfaceImplementations, filterVisitor)));
            }

            return MethodDeclaration(
                GetAttributes(symbol, filterVisitor),
                TokenList(GetMemberModifiers(symbol)),
                GetTypeSyntax(symbol.ReturnType),
                eii,
                Identifier(GetMemberName(symbol, filterVisitor)),
                GetTypeParameters(symbol),
                ParameterList(
                    SeparatedList(
                        symbol.Parameters.Select((p, i)
                            => GetParameter(p, filterVisitor, i == 0 && symbol.IsExtensionMethod))
                    )
                ),
                List(GetTypeParameterConstraints(symbol)),
                null,
                null
            ).NormalizeWhitespace().ToString();
        }

        static string GetOperatorSyntax(IMethodSymbol symbol, IFilterVisitor filterVisitor)
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
                    GetAttributes(symbol, filterVisitor),
                    TokenList(
                        GetMemberModifiers(symbol)
                    ),
                    operatorToken.Value,
                    GetTypeSyntax(symbol.ReturnType),
                    ParameterList(
                        SeparatedList(
                            from p in symbol.Parameters
                            select GetParameter(p, filterVisitor)
                        )
                    ),
                    null,
                    null
                ).NormalizeWhitespace().ToString();
            }

            return OperatorDeclaration(
                GetAttributes(symbol, filterVisitor),
                TokenList(
                    GetMemberModifiers(symbol)
                ),
                GetTypeSyntax(symbol.ReturnType),
                operatorToken.Value,
                ParameterList(
                    SeparatedList(
                        from p in symbol.Parameters
                        select GetParameter(p, filterVisitor)
                    )
                ),
                null,
                null
            ).NormalizeWhitespace().ToString();
        }

        static string GetConstructorSyntax(IMethodSymbol symbol, IFilterVisitor filterVisitor) =>
            ConstructorDeclaration(
                GetAttributes(symbol, filterVisitor),
                TokenList(
                    GetMemberModifiers(symbol)
                ),
                Identifier(symbol.ContainingType.Name),
                ParameterList(
                    SeparatedList(
                        from p in symbol.Parameters
                        select GetParameter(p, filterVisitor)
                    )
                ),
                null,
                (BlockSyntax) null
            ).NormalizeWhitespace().ToString();

        string GetFieldSyntax(IFieldSymbol symbol, IFilterVisitor filterVisitor)
        {
            if (symbol.ContainingType.TypeKind == TypeKind.Enum)
            {
                return EnumMemberDeclaration(
                    GetAttributes(symbol, filterVisitor),
                    Identifier(symbol.Name),
                    GetDefaultValueClause(symbol)
                ).NormalizeWhitespace().ToString();
            }

            return FieldDeclaration(
                GetAttributes(symbol, filterVisitor),
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
            ).NormalizeWhitespace().ToString().TrimEnd(';');
        }

        static string GetEventSyntax(IEventSymbol symbol, IFilterVisitor filterVisitor)
        {
            ExplicitInterfaceSpecifierSyntax eii = null;
            if (symbol.ExplicitInterfaceImplementations.Length > 0)
            {
                eii = ExplicitInterfaceSpecifier(
                    ParseName(GetEiiContainerTypeName(symbol.ExplicitInterfaceImplementations, filterVisitor)));
            }

            return RemoveBraces(
                EventDeclaration(
                    GetAttributes(symbol, filterVisitor),
                    TokenList(GetMemberModifiers(symbol)),
                    Token(SyntaxKind.EventKeyword),
                    GetTypeSyntax(symbol.Type),
                    eii,
                    Identifier(GetMemberName(symbol, filterVisitor)),
                    AccessorList()
                ).NormalizeWhitespace().ToString()
            );
        }

        static string GetPropertySyntax(IPropertySymbol symbol, IFilterVisitor filterVisitor)
        {
            string result;

            ExplicitInterfaceSpecifierSyntax eii = null;
            if (symbol.ExplicitInterfaceImplementations.Length > 0)
            {
                eii = ExplicitInterfaceSpecifier(
                    ParseName(GetEiiContainerTypeName(symbol.ExplicitInterfaceImplementations, filterVisitor)));
            }

            if (symbol.IsIndexer)
            {
                result = IndexerDeclaration(
                        GetAttributes(symbol, filterVisitor),
                        TokenList(GetMemberModifiers(symbol)),
                        GetTypeSyntax(symbol.Type),
                        eii,
                        BracketedParameterList(
                            SeparatedList(symbol.Parameters.Select(p => GetParameter(p, filterVisitor)))),
                        AccessorList(List(GetPropertyAccessors(symbol, filterVisitor))))
                    .NormalizeWhitespace()
                    .ToString();
            }
            else
            {
                result = PropertyDeclaration(
                        GetAttributes(symbol, filterVisitor),
                        TokenList(GetMemberModifiers(symbol)),
                        GetTypeSyntax(symbol.Type),
                        eii,
                        Identifier(GetMemberName(symbol, filterVisitor)),
                        AccessorList(List(GetPropertyAccessors(symbol, filterVisitor))))
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

        static SyntaxList<AttributeListSyntax> GetAttributes(
            ISymbol symbol, IFilterVisitor filterVisitor, bool inOneLine = false
        )
        {
            var attributes = symbol.GetAttributes();
            if (attributes.Length <= 0) return new SyntaxList<AttributeListSyntax>();

            var attrList = attributes
                .Where(attr => !(attr.AttributeClass is IErrorTypeSymbol))
                .Where(attr => attr?.AttributeConstructor != null)
                .Where(attr => filterVisitor.CanVisitAttribute(attr.AttributeConstructor))
                .Select(GetAttributeSyntax).ToList();

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
                            .Concat(attr.NamedArguments
                                .Select(item => new {item, expr = GetLiteralExpression(item.Value)})
                                .Where(t => t.expr != null)
                                .Select(t => AttributeArgument(NameEquals(IdentifierName(t.item.Key)), null,
                                    t.expr)))
                    )
                )
            );
        }

        static string GetSymbolName<T>(
            T symbol, Func<T, ImmutableArray<T>> getExplicitInterfaceImplementations, IFilterVisitor filterVisitor
        ) where T : ISymbol
            => getExplicitInterfaceImplementations(symbol)
                .FirstOrNone(x => filterVisitor.CanVisitApi(x))
                .Match(x => x.Name, () => symbol.Name);

        static string GetMemberName(IMethodSymbol symbol, IFilterVisitor filterVisitor)
            => GetSymbolName(symbol, x => x.ExplicitInterfaceImplementations, filterVisitor);

        static string GetMemberName(IEventSymbol symbol, IFilterVisitor filterVisitor)
            => GetSymbolName(symbol, x => x.ExplicitInterfaceImplementations, filterVisitor);

        static string GetMemberName(IPropertySymbol symbol, IFilterVisitor filterVisitor)
            => GetSymbolName(symbol, x => x.ExplicitInterfaceImplementations, filterVisitor);

        static string GetEiiContainerTypeName<T>(ImmutableArray<T> implementations, IFilterVisitor filterVisitor)
            where T : ISymbol
            => implementations
                .Where(x => filterVisitor.CanVisitApi(x))
                .Select(t =>
                    NameVisitorFactory
                        .GetCSharp(NameOptions.UseAlias | NameOptions.WithGenericParameter)
                        .GetName(t.ContainingType))
                .FirstOrDefault();

        static ParameterSyntax GetParameter(
            IParameterSymbol p, IFilterVisitor filterVisitor, bool isThisParameter = false
        )
            => Parameter(
                GetAttributes(p, filterVisitor, true),
                TokenList(GetParameterModifiers(p, isThisParameter)),
                GetTypeSyntax(p.Type),
                Identifier(p.Name),
                GetDefaultValueClause(p));

        static IEnumerable<SyntaxToken> GetParameterModifiers(IParameterSymbol parameter, bool isThisParameter)
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

        static EqualsValueClauseSyntax GetDefaultValueClause(IParameterSymbol symbol)
            => symbol.HasExplicitDefaultValue
                ? GetDefaultValueClauseCore(symbol.ExplicitDefaultValue, symbol.Type)
                : null;

        static EqualsValueClauseSyntax GetDefaultValueClause(IFieldSymbol symbol)
            => symbol.IsConst
                ? GetDefaultValueClauseCore(symbol.ConstantValue,
                    symbol.ContainingType.TypeKind == TypeKind.Enum
                        ? ((INamedTypeSymbol) symbol.Type).EnumUnderlyingType
                        : symbol.Type)
                : null;

        static EqualsValueClauseSyntax GetDefaultValueClauseCore(object value, ITypeSymbol type)
        {
            var expr = GetLiteralExpression(value, type);
            return expr != null ? EqualsValueClause(expr) : null;
        }

        static ExpressionSyntax GetLiteralExpression(TypedConstant constant)
        {
            if (constant.Type.TypeKind == TypeKind.Array)
            {
                if (constant.Values == null)
                {
                    return GetLiteralExpression(null, constant.Type);
                }

                var items = (from value in constant.Values
                    select GetLiteralExpression(value)).ToList();
                if (items.TrueForAll(x => x != null))
                {
                    return ArrayCreationExpression(
                        (ArrayTypeSyntax) GetTypeSyntax(constant.Type),
                        InitializerExpression(
                            SyntaxKind.ArrayInitializerExpression,
                            SeparatedList(
                                from value in constant.Values
                                select GetLiteralExpression(value))));
                }

                return ArrayCreationExpression(
                    (ArrayTypeSyntax) GetTypeSyntax(constant.Type));
            }

            var expr = GetLiteralExpression(constant.Value, constant.Type);
            if (expr == null)
            {
                return null;
            }

            return constant.Type.SpecialType switch
            {
                System_SByte => CastExpression(
                    PredefinedType(Token(SyntaxKind.SByteKeyword)), expr),
                System_Byte => CastExpression(
                    PredefinedType(Token(SyntaxKind.ByteKeyword)), expr),
                System_Int16 => CastExpression(
                    PredefinedType(Token(SyntaxKind.ShortKeyword)), expr),
                System_UInt16 => CastExpression(
                    PredefinedType(Token(SyntaxKind.UShortKeyword)), expr),
                _ => expr
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
                        Token(SyntaxKind.NullKeyword));
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
                var isFlags = namedType.GetAttributes().Any(attr
                    => attr.AttributeClass.GetDocumentationCommentId() == "T:System.FlagsAttribute");

                var pairs = namedType.GetMembers()
                    .OfType<IFieldSymbol>()
                    .Where(member => member.IsConst && member.HasConstantValue)
                    .Select(member => new {member.Name, member.ConstantValue});

                if (isFlags)
                {
                    var exprs = pairs
                        .Where(pair => HasFlag(namedType.EnumUnderlyingType, value, pair.ConstantValue))
                        .Select(pair => MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, enumType,
                            IdentifierName(pair.Name)))
                        .ToList();
                    if (exprs.Count > 0)
                    {
                        return exprs.Aggregate<ExpressionSyntax>((x, y) =>
                            BinaryExpression(SyntaxKind.BitwiseOrExpression, x, y));
                    }
                }
                else
                {
                    var expr = pairs
                        .Where(pair => Equals(value, pair.ConstantValue))
                        .Select(pair => MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, enumType,
                            IdentifierName(pair.Name)))
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
                        namedType.EnumUnderlyingType));
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
                System_Boolean => LiteralExpression((bool) value
                    ? SyntaxKind.TrueLiteralExpression
                    : SyntaxKind.FalseLiteralExpression),
                System_Char => CharLiteralExpression(),
                System_SByte => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((sbyte) value)),
                System_Byte => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((byte) value)),
                System_Int16 => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((short) value)),
                System_UInt16 => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((ushort) value)),
                System_Int32 => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression, Literal((int) value)),
                System_UInt32 => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((uint) value)),
                System_Int64 => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((long) value)),
                System_UInt64 => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((ulong) value)),
                System_Decimal => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((decimal) value)),
                System_Single => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((float) value)),
                System_Double => LiteralExpression(SyntaxKind.NumericLiteralExpression,
                    Literal((double) value)),
                System_String => LiteralExpression(SyntaxKind.StringLiteralExpression,
                    Literal((string) value)),
                _ => null
            };

            LiteralExpressionSyntax CharLiteralExpression()
            {
                var ch       = (char) value;
                var category = char.GetUnicodeCategory(ch);
                return category switch
                {
                    System.Globalization.UnicodeCategory.Surrogate => LiteralExpression(
                        SyntaxKind.CharacterLiteralExpression,
                        Literal("'\\u" + ((int) ch).ToString("X4") + "'", ch)),
                    _ => LiteralExpression(SyntaxKind.CharacterLiteralExpression,
                        Literal((char) value))
                };
            }
        }

        static IEnumerable<TypeParameterConstraintClauseSyntax> GetTypeParameterConstraints(
            INamedTypeSymbol symbol
        )
        {
            if (symbol.TypeArguments.Length == 0)
            {
                yield break;
            }

            foreach (var ta in symbol.TypeArguments.Cast<ITypeParameterSymbol>().Where(ta
                => ta.HasConstructorConstraint || ta.HasReferenceTypeConstraint || ta.HasValueTypeConstraint ||
                ta.ConstraintTypes.Length > 0))
            {
                yield return TypeParameterConstraintClause(IdentifierName(ta.Name),
                    SeparatedList(GetTypeParameterConstraint(ta)));
            }
        }

        static IEnumerable<TypeParameterConstraintClauseSyntax> GetTypeParameterConstraints(IMethodSymbol symbol)
        {
            if (symbol.TypeArguments.Length == 0)
            {
                yield break;
            }

            foreach (var ta in symbol
                .TypeArguments
                .Cast<ITypeParameterSymbol>()
                .Where(ta =>
                    ta.HasConstructorConstraint
                    || ta.HasReferenceTypeConstraint
                    || ta.HasValueTypeConstraint
                    || ta.ConstraintTypes.Length > 0)
            )
            {
                yield return TypeParameterConstraintClause(IdentifierName(ta.Name),
                    SeparatedList(GetTypeParameterConstraint(ta)));
            }
        }

        static IEnumerable<TypeParameterConstraintSyntax> GetTypeParameterConstraint(
            ITypeParameterSymbol symbol
        )
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

        private BaseListSyntax GetBaseTypeList(INamedTypeSymbol symbol)
        {
            IReadOnlyList<INamedTypeSymbol> baseTypeList;
            if (symbol.TypeKind                             != TypeKind.Class || symbol.BaseType == null ||
                symbol.BaseType.GetDocumentationCommentId() == "T:System.Object")
            {
                baseTypeList = symbol.AllInterfaces;
            }
            else
            {
                baseTypeList = new[] {symbol.BaseType}.Concat(symbol.AllInterfaces).ToList();
            }

            if (baseTypeList.Count == 0)
            {
                return null;
            }

            return BaseList(
                SeparatedList<BaseTypeSyntax>(
                    from t in baseTypeList
                    select SimpleBaseType(GetTypeSyntax(t))));
        }

        BaseListSyntax GetEnumBaseTypeList(INamedTypeSymbol symbol)
        {
            var underlyingType = symbol.EnumUnderlyingType;
            if (underlyingType.GetDocumentationCommentId() == "T:System.Int32")
            {
                return null;
            }

            return BaseList(
                SingletonSeparatedList<BaseTypeSyntax>(
                    SimpleBaseType(
                        GetTypeSyntax(underlyingType))));
        }

        static TypeParameterListSyntax GetTypeParameters(INamedTypeSymbol symbol)
        {
            if (symbol.TypeArguments.NotEmpty())
            {
                return null;
            }

            return TypeParameterList(
                SeparatedList(
                    from ITypeParameterSymbol t in symbol.TypeArguments
                    select TypeParameter(
                        new SyntaxList<AttributeListSyntax>(),
                        GetVarianceToken(t),
                        Identifier(t.Name))));
        }

        private static TypeParameterListSyntax GetTypeParameters(IMethodSymbol symbol)
        {
            if (symbol.TypeArguments.Length == 0)
            {
                return null;
            }

            return TypeParameterList(
                SeparatedList(
                    from ITypeParameterSymbol t in symbol.TypeArguments
                    select TypeParameter(
                        new SyntaxList<AttributeListSyntax>(),
                        GetVarianceToken(t),
                        Identifier(t.Name))));
        }

        private static SyntaxToken GetVarianceToken(ITypeParameterSymbol t)
            => t.Variance switch
            {
                VarianceKind.In  => Token(SyntaxKind.InKeyword),
                VarianceKind.Out => Token(SyntaxKind.OutKeyword),
                _                => new SyntaxToken()
            };

        static IEnumerable<SyntaxToken> GetTypeModifiers(INamedTypeSymbol symbol)
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

        static IEnumerable<SyntaxToken> GetMemberModifiers(IMethodSymbol symbol)
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

        static IEnumerable<SyntaxToken> GetMemberModifiers(IEventSymbol symbol)
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

        private static IEnumerable<SyntaxToken> GetMemberModifiers(IPropertySymbol symbol)
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

        static IEnumerable<SyntaxToken> GetMemberModifiers(IFieldSymbol symbol)
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

            if (symbol.IsConst)
            {
                yield return Token(SyntaxKind.ConstKeyword);
            }
            else
            {
                if (symbol.IsStatic)
                {
                    yield return Token(SyntaxKind.StaticKeyword);
                }

                if (symbol.IsReadOnly)
                {
                    yield return Token(SyntaxKind.ReadOnlyKeyword);
                }

                if (symbol.IsVolatile)
                {
                    yield return Token(SyntaxKind.VolatileKeyword);
                }
            }
        }

        static SyntaxToken? GetOperatorToken(IMethodSymbol symbol)
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
            IPropertySymbol propertySymbol, IFilterVisitor filterVisitor
        )
        {
            var getAccessor = GetPropertyAccessorCore(propertySymbol, propertySymbol.GetMethod,
                SyntaxKind.GetAccessorDeclaration, SyntaxKind.GetKeyword, filterVisitor);
            if (getAccessor != null)
            {
                yield return getAccessor;
            }

            var setAccessor = GetPropertyAccessorCore(propertySymbol, propertySymbol.SetMethod,
                SyntaxKind.SetAccessorDeclaration, SyntaxKind.SetKeyword, filterVisitor);
            if (setAccessor != null)
            {
                yield return setAccessor;
            }
        }

        static AccessorDeclarationSyntax GetPropertyAccessorCore(
            IPropertySymbol propertySymbol, IMethodSymbol methodSymbol,
            SyntaxKind kind, SyntaxKind keyword, IFilterVisitor filterVisitor
        )
        {
            if (methodSymbol == null)
            {
                return null;
            }

            switch (methodSymbol.DeclaredAccessibility)
            {
                case Accessibility.Protected:
                case Accessibility.ProtectedOrInternal:
                    if (propertySymbol.DeclaredAccessibility == Accessibility.Protected ||
                        propertySymbol.DeclaredAccessibility == Accessibility.ProtectedOrInternal)
                    {
                        return AccessorDeclaration(kind,
                            GetAttributes(methodSymbol, filterVisitor),
                            new SyntaxTokenList(),
                            Token(keyword),
                            (BlockSyntax) null,
                            Token(SyntaxKind.SemicolonToken));
                    }
                    else
                    {
                        return AccessorDeclaration(
                            kind,
                            GetAttributes(methodSymbol, filterVisitor),
                            TokenList(Token(SyntaxKind.ProtectedKeyword)),
                            Token(keyword),
                            (BlockSyntax) null,
                            Token(SyntaxKind.SemicolonToken));
                    }
                case Accessibility.Public:
                    return AccessorDeclaration(kind,
                        GetAttributes(methodSymbol, filterVisitor),
                        new SyntaxTokenList(),
                        Token(keyword),
                        (BlockSyntax) null,
                        Token(SyntaxKind.SemicolonToken));
                default:
                    if (methodSymbol.ExplicitInterfaceImplementations.Length > 0)
                    {
                        return AccessorDeclaration(kind,
                            GetAttributes(methodSymbol, filterVisitor),
                            new SyntaxTokenList(),
                            Token(keyword),
                            (BlockSyntax) null,
                            Token(SyntaxKind.SemicolonToken));
                    }

                    return null;
            }
        }

        static string RemoveBraces(string text) => BracesRegex.Replace(text, string.Empty);

        static TypeSyntax GetTypeSyntax(ITypeSymbol type)
        {
            var name = NameVisitorFactory.GetCSharp(NameOptions.UseAlias | NameOptions.WithGenericParameter)
                .GetName(type);
            return ParseTypeName(name);
        }

        static string GetVisibility(Accessibility accessibility)
            => accessibility switch
            {
                Accessibility.Protected           => "protected",
                Accessibility.ProtectedOrInternal => "protected",
                Accessibility.Public              => "public",
                _                                 => null
            };

        internal string AddReference(ISymbol symbol, Dictionary<string, ReferenceItem> references)
        {
            var id = symbol.GetId();

            var reference = new ReferenceItem
            {
                Parts        = new List<LinkItem>(),
                IsDefinition = symbol.IsDefinition,
                CommentId    = symbol.GetCommentId()
            };
            GenerateReference(symbol, reference);

            if (!references.ContainsKey(id))
            {
                references[id] = reference;
            }
            else
            {
                references[id].Merge(reference);
            }

            return id;
        }

        internal static void AddReference(string id, string commentId, Dictionary<string, ReferenceItem> references)
        {
            if (!references.ContainsKey(id))
            {
                // Add id to reference dictionary
                references[id] = new ReferenceItem {CommentId = commentId};
            }
        }

        internal static string AddOverloadReference(ISymbol symbol, Dictionary<string, ReferenceItem> references)
        {
            var uidBody = symbol.GetOverloadIdBody();

            var reference = new ReferenceItem
            {
                Parts        = new List<LinkItem>(),
                IsDefinition = true,
                CommentId    = "Overload:" + uidBody
            };
            GenerateReference(symbol, reference, true);

            var uid = uidBody + "*";
            if (!references.ContainsKey(uid))
            {
                references[uid] = reference;
            }
            else
            {
                references[uid].Merge(reference);
            }

            return uid;
        }

        internal string AddSpecReference(
            ISymbol symbol,
            IReadOnlyList<string> typeGenericParameters,
            IReadOnlyList<string> methodGenericParameters,
            Dictionary<string, ReferenceItem> references,
            MetadataSymbolVisitor adapter
        )
        {
            var rawId = symbol.GetId();
            var id    = symbol.GetSpecId(typeGenericParameters, methodGenericParameters);
            if (string.IsNullOrEmpty(id))
            {
                throw new InvalidDataException(
                    $"Fail to parse id for symbol {symbol.MetadataName} in namespace {symbol.ContainingSymbol?.MetadataName}.");
            }

            var reference = new ReferenceItem {Parts = new List<LinkItem>()};

            GenerateReference(symbol, reference);
            var reducedFrom    = (symbol as IMethodSymbol)?.ReducedFrom;
            var originalSymbol = reducedFrom ?? symbol;

            reference.IsDefinition = originalSymbol.Equals(symbol) && id == rawId &&
                (symbol.IsDefinition || symbol.OriginalDefinition.GetId() == rawId);

            if (!reference.IsDefinition.Value && rawId != null)
            {
                reference.Definition = AddReference(originalSymbol.OriginalDefinition, references);
            }

            reference.Parent = GetReferenceParent(originalSymbol, typeGenericParameters, methodGenericParameters,
                references, adapter);
            reference.CommentId = originalSymbol.GetCommentId();

            if (!references.ContainsKey(id))
            {
                references[id] = reference;
            }
            else
            {
                references[id].Merge(reference);
            }

            return id;
        }

        string GetReferenceParent(
            ISymbol symbol,
            IReadOnlyList<string> typeGenericParameters,
            IReadOnlyList<string> methodGenericParameters,
            Dictionary<string, ReferenceItem> references,
            MetadataSymbolVisitor adapter
        )
        {
            switch (symbol.Kind)
            {
                case SymbolKind.Event:
                case SymbolKind.Field:
                case SymbolKind.Method:
                case SymbolKind.NamedType:
                case SymbolKind.Property:
                {
                    var parentSymbol = symbol;
                    do
                    {
                        parentSymbol = parentSymbol.ContainingSymbol;
                    } while (parentSymbol.Kind == symbol.Kind); // the parent of nested type is namespace.

                    return IsGlobalNamespace(parentSymbol)
                        ? null
                        : AddSpecReference(parentSymbol, typeGenericParameters, methodGenericParameters, references,
                            adapter);
                }
                default:
                    return null;
            }

            static bool IsGlobalNamespace(ISymbol symbol) => (symbol as INamespaceSymbol)?.IsGlobalNamespace == true;
        }
    }
}