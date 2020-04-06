using System.Diagnostics;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static Microsoft.CodeAnalysis.SpecialType;

namespace Ubiquitous.DocGen.Metadata.CodeAnalysis.Syntax
{
    static class DefaultValue
    {
        internal static EqualsValueClauseSyntax GetDefaultValueClause(this IParameterSymbol symbol)
            => symbol.HasExplicitDefaultValue
                ? symbol.Type.GetDefaultValueClauseCore(symbol.ExplicitDefaultValue)
                : null;

        internal static EqualsValueClauseSyntax GetDefaultValueClause(this IFieldSymbol symbol)
            => symbol.IsConst
                ? GetDefaultValueClauseCore(
                    symbol.ContainingType.TypeKind == TypeKind.Enum
                        ? ((INamedTypeSymbol) symbol.Type).EnumUnderlyingType
                        : symbol.Type,
                    symbol.ConstantValue
                )
                : null;

        static EqualsValueClauseSyntax GetDefaultValueClauseCore(this ITypeSymbol type, object value)
        {
            var expr = type.GetLiteralExpression(value);
            return expr != null ? EqualsValueClause(expr) : null;
        }

        internal static ExpressionSyntax GetLiteralExpression(this TypedConstant constant)
        {
            if (constant.Type.TypeKind == TypeKind.Array)
            {
                if (constant.Values == null) return GetLiteralExpression(null, constant.Type);

                var items = constant.Values.Select(GetLiteralExpression);

                if (items.All(x => x != null))
                {
                    return ArrayCreationExpression(
                        (ArrayTypeSyntax) constant.Type.GetTypeSyntax(),
                        InitializerExpression(
                            SyntaxKind.ArrayInitializerExpression,
                            SeparatedList(
                                from value in constant.Values
                                select GetLiteralExpression(value)
                            )
                        )
                    );
                }

                return ArrayCreationExpression((ArrayTypeSyntax) constant.Type.GetTypeSyntax());
            }

            var expr = constant.Type.GetLiteralExpression(constant.Type);

            if (expr == null) return null;

            return constant.Type.SpecialType switch
            {
                System_SByte => CastExpression(
                    PredefinedType(Token(SyntaxKind.SByteKeyword)),
                    expr
                ),
                System_Byte => CastExpression(PredefinedType(Token(SyntaxKind.ByteKeyword)), expr),
                System_Int16 => CastExpression(
                    PredefinedType(Token(SyntaxKind.ShortKeyword)),
                    expr
                ),
                System_UInt16 => CastExpression(
                    PredefinedType(Token(SyntaxKind.UShortKeyword)),
                    expr
                ),
                _ => expr
            };
        }

        static ExpressionSyntax GetLiteralExpression(this ITypeSymbol type, object value)
        {
            if (value == null)
                return type.IsValueType
                    ? (ExpressionSyntax) DefaultExpression(type.GetTypeSyntax())
                    : LiteralExpression(
                        SyntaxKind.NullLiteralExpression,
                        Token(SyntaxKind.NullKeyword)
                    );

            var result = type.GetLiteralExpressionCore(value);

            if (result != null) return result;

            if (type.TypeKind == TypeKind.Enum)
            {
                var namedType = (INamedTypeSymbol) type;
                var enumType  = namedType.GetTypeSyntax();

                var isFlags = namedType.GetAttributes()
                    .Any(
                        attr => attr.AttributeClass.GetDocumentationCommentId() ==
                            "T:System.FlagsAttribute"
                    );

                var pairs = namedType.GetMembers()
                    .OfType<IFieldSymbol>()
                    .Where(member => member.IsConst && member.HasConstantValue)
                    .Select(member => new {member.Name, member.ConstantValue});

                if (isFlags)
                {
                    var exprs = pairs
                        .Where(
                            pair => HasFlag(namedType.EnumUnderlyingType, value, pair.ConstantValue)
                        )
                        .Select(
                            pair => MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                enumType,
                                IdentifierName(pair.Name)
                            )
                        )
                        .ToList();

                    if (exprs.Count > 0)
                        return exprs.Aggregate<ExpressionSyntax>(
                            (x, y) => BinaryExpression(SyntaxKind.BitwiseOrExpression, x, y)
                        );
                }
                else
                {
                    var expr = pairs
                        .Where(pair => Equals(value, pair.ConstantValue))
                        .Select(
                            pair => MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                enumType,
                                IdentifierName(pair.Name)
                            )
                        )
                        .FirstOrDefault();

                    if (expr != null) return expr;
                }

                return CastExpression(
                    enumType,
                    namedType.EnumUnderlyingType.GetLiteralExpressionCore(value)
                );
            }

            if (value is ITypeSymbol symbol) return TypeOfExpression(symbol.GetTypeSyntax());

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
                default: return false;
            }
        }

        static ExpressionSyntax GetLiteralExpressionCore(this ITypeSymbol type, object value)
        {
            return type.SpecialType switch
            {
                System_Boolean => LiteralExpression(
                    (bool) value
                        ? SyntaxKind.TrueLiteralExpression
                        : SyntaxKind.FalseLiteralExpression
                ),
                System_Char => CharLiteralExpression(),
                System_SByte => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((sbyte) value)
                ),
                System_Byte => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((byte) value)
                ),
                System_Int16 => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((short) value)
                ),
                System_UInt16 => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((ushort) value)
                ),
                System_Int32 => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((int) value)
                ),
                System_UInt32 => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((uint) value)
                ),
                System_Int64 => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((long) value)
                ),
                System_UInt64 => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((ulong) value)
                ),
                System_Decimal => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((decimal) value)
                ),
                System_Single => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((float) value)
                ),
                System_Double => LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal((double) value)
                ),
                System_String => LiteralExpression(
                    SyntaxKind.StringLiteralExpression,
                    Literal((string) value)
                ),
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
                        Literal("'\\u" + ((int) ch).ToString("X4") + "'", ch)
                    ),
                    _ => LiteralExpression(
                        SyntaxKind.CharacterLiteralExpression,
                        Literal((char) value)
                    )
                };
            }
        }

        
    }
}
