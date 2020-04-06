using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Ubiquitous.DocGen.Metadata.Extensions;

namespace Ubiquitous.DocGen.Metadata.CodeAnalysis
{
    public static class NameVisitorFactory
    {
        static readonly Dictionary<NameOptions, Func<NameVisitor>> NameVisitors =
            new Dictionary<NameOptions, Func<NameVisitor>>();

        public static string GetName(this Func<NameVisitor> getVisitor, ISymbol symbol)
        {
            var visitor = getVisitor();
            symbol.Accept(visitor);
            return visitor.GetTypeName();
        }

        public static Func<NameVisitor> GetNameVisitor(NameOptions option)
        {
            if (option < NameOptions.None || option > NameOptions.All)
                throw new ArgumentOutOfRangeException(nameof(option));

            if (!NameVisitors.ContainsKey(option))
                NameVisitors[option] = () => new NameVisitor(option);
            
            return NameVisitors[option];
        }
    }

    [Flags]
    public enum NameOptions
    {
        None                       = 0,
        UseAlias                   = 1,
        WithNamespace              = 2,
        WithTypeGenericParameter   = 4,
        WithParameter              = 8,
        WithType                   = 16,
        WithMethodGenericParameter = 32,
        WithGenericParameter       = WithTypeGenericParameter | WithMethodGenericParameter,
        Qualified                  = WithNamespace | WithType,

        All = UseAlias |
            WithNamespace |
            WithTypeGenericParameter |
            WithParameter |
            WithType |
            WithMethodGenericParameter
    }

    public sealed class NameVisitor : SymbolVisitor
    {
        readonly NameOptions   _options;
        readonly StringBuilder _sb = new StringBuilder();

        void Append(string text) => _sb.Append(text);

        internal string GetTypeName() => _sb.ToString();

        public NameVisitor(NameOptions options) => _options = options;

        public override void VisitNamedType(INamedTypeSymbol symbol)
        {
            if (OptionsMatch(NameOptions.UseAlias) && TrySpecialType(symbol)) return;

            if (symbol.ContainingType != null)
            {
                symbol.ContainingType.Accept(this);
                Append(".");
            }
            else if (OptionsMatch(NameOptions.WithNamespace))
            {
                if (!InGlobalNamespace(symbol))
                {
                    symbol.ContainingNamespace.Accept(this);
                    Append(".");
                }
            }

            if (symbol.IsTupleType)
            {
                if (OptionsMatch(NameOptions.Qualified))
                {
                    Append("ValueTuple");
                    symbol = symbol.TupleUnderlyingType;
                }
                else
                {
                    AppendParameters(
                        symbol.TupleElements,
                        "(",
                        ")",
                        x => x.Type.Accept(this),
                        x => Append(" " + x.Name)
                    );
                }
            }
            else
            {
                Append(symbol.Name);
            }

            if (OptionsMatch(NameOptions.WithTypeGenericParameter) &&
                symbol.TypeParameters.IsNotEmpty())
            {
                if (symbol.TypeArguments != null && symbol.TypeArguments.IsNotEmpty())
                {
                    if (symbol.IsUnboundGenericType)
                        AppendParameters(symbol.TypeArguments.Length, "<", ">");
                    else
                        AppendParameters(symbol.TypeArguments, "<", ">");
                }
                else
                {
                    AppendParameters(symbol.TypeParameters, "<", ">");
                }
            }

            static bool InGlobalNamespace(ISymbol symbol)
                => symbol.ContainingNamespace == null ||
                    symbol.ContainingNamespace.IsGlobalNamespace;
        }

        bool OptionsMatch(NameOptions options) => (_options & options) == options;

        public override void VisitNamespace(INamespaceSymbol symbol)
        {
            if (symbol.IsGlobalNamespace) return;

            if (!symbol.ContainingNamespace.IsGlobalNamespace)
            {
                symbol.ContainingNamespace.Accept(this);
                Append(".");
            }

            Append(symbol.Name);
        }

        public override void VisitArrayType(IArrayTypeSymbol symbol)
        {
            symbol.ElementType.Accept(this);
            Append(symbol.GetSuffix());
        }

        public override void VisitPointerType(IPointerTypeSymbol symbol)
        {
            symbol.PointedAtType.Accept(this);
            Append("*");
        }

        public override void VisitTypeParameter(ITypeParameterSymbol symbol) => Append(symbol.Name);

        public override void VisitMethod(IMethodSymbol symbol)
        {
            if ((_options & NameOptions.WithType) == NameOptions.WithType)
            {
                symbol.ContainingType.Accept(this);
                Append(".");
            }

            switch (symbol.MethodKind)
            {
                case MethodKind.Constructor:
                    Append(symbol.ContainingType.Name);
                    break;
                case MethodKind.Conversion:
                    switch (symbol.Name)
                    {
                        case "op_Explicit":
                            Append("Explicit");
                            break;
                        case "op_Implicit":
                            Append("Implicit");
                            break;
                        default:
                            Append(symbol.Name);
                            break;
                    }

                    break;
                case MethodKind.UserDefinedOperator:
                    Append(
                        symbol.Name.StartsWith("op_", StringComparison.Ordinal)
                            ? symbol.Name.Substring(3)
                            : symbol.Name
                    );
                    break;
                default:
                    if (symbol.ExplicitInterfaceImplementations.Length == 0)
                    {
                        Append(symbol.Name);
                    }
                    else
                    {
                        var interfaceMethod = symbol.ExplicitInterfaceImplementations[0];

                        if ((_options & NameOptions.WithType) == NameOptions.None)
                        {
                            interfaceMethod.ContainingType.Accept(this);
                            Append(".");
                        }

                        interfaceMethod.Accept(this);
                        return;
                    }

                    break;
            }

            if (symbol.IsGenericMethod && OptionsMatch(NameOptions.WithMethodGenericParameter))
            {
                var typeParams = symbol.TypeArguments.Length > 0
                    ? symbol.TypeArguments.CastArray<ISymbol>()
                    : symbol.TypeParameters.CastArray<ISymbol>();

                AppendParameters(typeParams, "<", ">");
            }

            if (OptionsMatch(NameOptions.WithParameter))
            {
                AppendParameters(
                    symbol.Parameters,
                    "(",
                    ")",
                    inBetween: x =>
                    {
                        if (symbol.MethodKind != MethodKind.Conversion || symbol.ReturnsVoid)
                            return;

                        Append(" to ");
                        symbol.ReturnType.Accept(this);
                    }
                );
            }
        }

        public override void VisitProperty(IPropertySymbol symbol)
        {
            if (OptionsMatch(NameOptions.WithType))
            {
                symbol.ContainingType.Accept(this);
                Append(".");
            }

            if (symbol.ExplicitInterfaceImplementations.IsNotEmpty())
            {
                var interfaceProperty = symbol.ExplicitInterfaceImplementations[0];

                if (!OptionsMatch(NameOptions.WithType))
                {
                    interfaceProperty.ContainingType.Accept(this);
                    Append(".");
                }

                interfaceProperty.Accept(this);
                return;
            }

            if (symbol.Parameters.IsNotEmpty())
            {
                Append(OptionsMatch(NameOptions.UseAlias) ? "this" : symbol.MetadataName);

                if (OptionsMatch(NameOptions.WithParameter))
                    AppendParameters(symbol.Parameters, "[", "]");
            }
            else
            {
                Append(symbol.Name);
            }
        }

        public override void VisitEvent(IEventSymbol symbol)
        {
            if (OptionsMatch(NameOptions.WithType))
            {
                symbol.ContainingType.Accept(this);
                Append(".");
            }

            if (symbol.ExplicitInterfaceImplementations.Length > 0)
            {
                var interfaceEvent = symbol.ExplicitInterfaceImplementations[0];

                if (!OptionsMatch(NameOptions.WithType))
                {
                    interfaceEvent.ContainingType.Accept(this);
                    Append(".");
                }

                interfaceEvent.Accept(this);
            }
            else
            {
                Append(symbol.Name);
            }
        }

        public override void VisitField(IFieldSymbol symbol)
        {
            if (OptionsMatch(NameOptions.WithType))
            {
                symbol.ContainingType.Accept(this);
                Append(".");
            }

            Append(symbol.Name);
        }

        public override void VisitParameter(IParameterSymbol symbol)
        {
            switch (symbol.RefKind)
            {
                case RefKind.Ref:
                    Append("ref ");
                    break;
                case RefKind.Out:
                    Append("out ");
                    break;
            }

            symbol.Type.Accept(this);
        }

        public override void VisitDynamicType(IDynamicTypeSymbol symbol)
        {
            if (OptionsMatch(NameOptions.UseAlias))
                Append("dynamic");
            else if (OptionsMatch(NameOptions.WithNamespace))
                Append(typeof(object).FullName);
            else
                Append(nameof(Object));
        }

        static readonly Dictionary<SpecialType, string> SpecialTypeMap =
            new Dictionary<SpecialType, string>
            {
                {SpecialType.System_Object, "object"},
                {SpecialType.System_Void, "void"},
                {SpecialType.System_Boolean, "bool"},
                {SpecialType.System_Char, "char"},
                {SpecialType.System_SByte, "sbyte"},
                {SpecialType.System_Byte, "byte"},
                {SpecialType.System_Int16, "short"},
                {SpecialType.System_UInt16, "ushort"},
                {SpecialType.System_Int32, "int"},
                {SpecialType.System_UInt32, "uint"},
                {SpecialType.System_Int64, "long"},
                {SpecialType.System_UInt64, "ulong"},
                {SpecialType.System_Decimal, "decimal"},
                {SpecialType.System_Single, "float"},
                {SpecialType.System_Double, "double"},
                {SpecialType.System_String, "string"}
            };

        bool TrySpecialType(INamedTypeSymbol symbol)
        {
            if (SpecialTypeMap.TryGetValue(symbol.SpecialType, out var typeName))
            {
                Append(typeName);
                return true;
            }

            if (!symbol.IsGenericType ||
                symbol.IsDefinition ||
                symbol.OriginalDefinition.SpecialType != SpecialType.System_Nullable_T)
                return false;

            symbol.TypeArguments[0].Accept(this);
            Append("?");
            return true;
        }

        void AppendParameters<T>(
            ImmutableArray<T> parameters,
            string start,
            string end,
            Action<T> accept = null,
            Action<T> inBetween = null
        ) where T : ISymbol
        {
            accept ??= x => x.Accept(this);

            Append(start);

            for (var i = 0; i < parameters.Length; i++)
            {
                if (i > 0) Append(", ");
                accept(parameters[i]);
                inBetween?.Invoke(parameters[i]);
            }

            Append(end);
        }

        void AppendParameters(int typeParameterCount, string start, string end)
            => Append($"{start}{new string(',', typeParameterCount)}{end}");
    }
}
