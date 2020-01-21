using System.Collections.Immutable;
using Microsoft.CodeAnalysis;
using Ubiquitous.DocFx.Markdown.Extensions;
using Ubiquitous.DocFx.Markdown.Models;

namespace Ubiquitous.DocFx.Markdown.Visitors
{
    public class ReferenceItemVisitor : SymbolVisitor
    {
        ReferenceItem ReferenceItem { get; }

        readonly bool _asOverload;

        public ReferenceItemVisitor(ReferenceItem referenceItem, bool asOverload)
        {
            ReferenceItem = referenceItem;
            _asOverload   = asOverload;
        }

        public override void VisitNamedType(INamedTypeSymbol symbol)
        {
            if (symbol.IsTupleType)
            {
                symbol = symbol.TupleUnderlyingType;
            }

            if (symbol.IsGenericType)
            {
                if (symbol.IsUnboundGenericType)
                {
                    AddLinkItems(symbol, true);
                }
                else
                {
                    AddLinkItems(symbol.OriginalDefinition, false);
                    AddArguments(symbol.TypeParameters, "<", ">");
                }
            }
            else
            {
                AddLinkItems(symbol, true);
            }
        }

        public override void VisitNamespace(INamespaceSymbol symbol) => AddPart(symbol, null, NameOptions.None,
            NameOptions.WithType, NameOptions.Qualified);

        public override void VisitTypeParameter(ITypeParameterSymbol symbol) => AddIdenticalNamePart(symbol.Name);

        public override void VisitArrayType(IArrayTypeSymbol symbol)
        {
            symbol.ElementType.Accept(this);

            var name = symbol.Rank == 1 ? "[]" : "[" + new string(',', symbol.Rank - 1) + "]";
            AddIdenticalNamePart(name);
        }

        public override void VisitPointerType(IPointerTypeSymbol symbol)
        {
            symbol.PointedAtType.Accept(this);
            AddIdenticalNamePart("*");
        }

        public override void VisitMethod(IMethodSymbol symbol)
        {
            var id = _asOverload
                ? symbol.OriginalDefinition.GetOverloadId()
                : symbol.OriginalDefinition.GetId();

            var baseOptions = _asOverload ? NameOptions.WithTypeGenericParameter : NameOptions.WithGenericParameter;

            AddPart(symbol, id, baseOptions, NameOptions.WithType | baseOptions, NameOptions.Qualified | baseOptions);

            if (!_asOverload) AddArguments(symbol.Parameters, "(", ")");
        }

        public override void VisitProperty(IPropertySymbol symbol)
        {
            var id = _asOverload
                ? symbol.OriginalDefinition.GetOverloadId()
                : symbol.OriginalDefinition.GetId();
            AddPart(symbol, id, NameOptions.WithTypeGenericParameter,
                NameOptions.WithType  | NameOptions.WithTypeGenericParameter,
                NameOptions.Qualified | NameOptions.WithTypeGenericParameter);

            if (symbol.Parameters.Length > 0 && !_asOverload) AddArguments(symbol.Parameters, "[", "]");
        }

        public override void VisitEvent(IEventSymbol symbol)
        {
            var id = symbol.OriginalDefinition.GetId();
            AddPart(symbol, id, NameOptions.WithTypeGenericParameter,
                NameOptions.WithType  | NameOptions.WithTypeGenericParameter,
                NameOptions.Qualified | NameOptions.WithTypeGenericParameter);
        }

        public override void VisitField(IFieldSymbol symbol)
        {
            var id = symbol.OriginalDefinition.GetId();
            AddPart(symbol, id, NameOptions.WithTypeGenericParameter,
                NameOptions.WithType  | NameOptions.WithTypeGenericParameter,
                NameOptions.Qualified | NameOptions.WithTypeGenericParameter);
        }

        public override void VisitDynamicType(IDynamicTypeSymbol symbol)
        {
            var id = symbol.OriginalDefinition.GetId();
            AddPart(symbol, id, NameOptions.None, NameOptions.WithType, NameOptions.Qualified);
        }

        void AddLinkItems(ISymbol symbol, bool withGenericParameter)
        {
            var id = symbol.GetId();
            if (withGenericParameter)
            {
                AddPart(symbol, id, NameOptions.WithGenericParameter,
                    NameOptions.WithType  | NameOptions.WithGenericParameter,
                    NameOptions.Qualified | NameOptions.WithGenericParameter);
            }
            else
            {
                AddPart(symbol, id, NameOptions.None, NameOptions.WithType, NameOptions.Qualified);
            }
        }

        void AddIdenticalNamePart(string name)
            => ReferenceItem.Parts.Add(new LinkItem
            {
                DisplayName          = name,
                DisplayNameWithType  = name,
                DisplayQualifiedName = name
            });

        void AddPart(
            ISymbol symbol, string id,
            NameOptions displayNameOptions,
            NameOptions displayNameWithTypeOptions,
            NameOptions displayQualifiedNameOptions
        )
        {
            ReferenceItem.Parts.Add(new LinkItem
            {
                DisplayName          = NameVisitorFactory.GetCSharp(displayNameOptions).GetName(symbol),
                DisplayNameWithType  = NameVisitorFactory.GetCSharp(displayNameWithTypeOptions).GetName(symbol),
                DisplayQualifiedName = NameVisitorFactory.GetCSharp(displayQualifiedNameOptions).GetName(symbol),
                Name                 = id,
                IsExternalPath       = symbol.IsExtern || symbol.DeclaringSyntaxReferences.Length == 0,
            });
        }

        void AddArguments<T>(ImmutableArray<T> arguments, string start, string end) where T : ISymbol
        {
            AddIdenticalNamePart(start);

            for (var i = 0; i < arguments.Length; i++)
            {
                if (i > 0) AddIdenticalNamePart(", ");
                arguments[i].Accept(this);
            }

            AddIdenticalNamePart(end);
        }
    }
}