using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using DocGen.Metadata.Comments;
using DocGen.Metadata.Models;
using DocGen.Metadata.Roslyn;
using Microsoft.CodeAnalysis;
using Optional;
using Optional.Collections;
using DocGen.Metadata.CodeAnalysis.Syntax;
using DocGen.Metadata.Extensions;

namespace DocGen.Metadata.CodeAnalysis
{
    public class MetadataSymbolVisitor : SymbolVisitor<MetadataItem>
    {
        static readonly IReadOnlyList<string> EmptyListOfString = new string[0];

        readonly IReadOnlyDictionary<Compilation, IEnumerable<IMethodSymbol>> _extensionMethods;

        readonly Compilation          _currentCompilation;
        readonly CompilationReference _currentCompilationRef;
        readonly string?              _codeSourceBasePath;

        public MetadataSymbolVisitor(Compilation compilation, ExtractMetadataOptions? options)
        {
            _currentCompilation    = compilation;
            _currentCompilationRef = compilation.ToMetadataReference();
            ApiFilter              = new DefaultApiFilter();

            _extensionMethods = options?.RoslynExtensionMethods != null
                ? options.RoslynExtensionMethods.ToDictionary(
                    p => p.Key,
                    p => p.Value.Where(e => ApiFilter.CanVisitApi(e))
                )
                : new Dictionary<Compilation, IEnumerable<IMethodSymbol>>();
            _codeSourceBasePath = options?.CodeSourceBasePath;
        }

        IApiFilter ApiFilter { get; }

        [return: MaybeNull]
        public override MetadataItem DefaultVisit(ISymbol symbol)
        {
            if (!ApiFilter.CanVisitApi(symbol)) return null;

            var item = new MetadataItem
            {
                Name       = symbol.GetRawId(),
                CommentId  = symbol.GetCommentId(),
                RawComment = symbol.GetDocumentationCommentXml(),
                Source     = symbol.GetSourceDetail()
            };

            var assemblyName = symbol.ContainingAssembly?.Name;

            item.AssemblyNameList = string.IsNullOrEmpty(assemblyName)
                ? null
                : new List<string> {assemblyName};

            if (!(symbol is INamespaceSymbol))
            {
                var namespaceName = symbol.ContainingNamespace.GetRawId();
                item.NamespaceName = namespaceName;
            }

            item.AddComments(GetTripleSlashCommentParserContext(item));

            item.DisplayName = NameVisitorFactory
                .GetNameVisitor(NameOptions.WithGenericParameter | NameOptions.WithParameter)
                .GetName(symbol);

            return item;
        }

        public override MetadataItem VisitAssembly(IAssemblySymbol symbol)
        {
            var item = new MetadataItem
            {
                Name        = symbol.GetRawId(),
                RawComment  = symbol.GetDocumentationCommentXml(),
                DisplayName = symbol.MetadataName,
                Type        = MemberType.Assembly,
            };

            var namespaces = symbol.GlobalNamespace.GetNamespaceMembers();

            item.Items = VisitDescendants(
                namespaces,
                ns => ns.GetMembers().OfType<INamespaceSymbol>(),
                ns => ns.GetMembers().OfType<INamedTypeSymbol>().Any(t => ApiFilter.CanVisitApi(t))
            );
            return item;
        }

        [return: MaybeNull]
        public override MetadataItem VisitNamespace(INamespaceSymbol symbol)
        {
            var item = DefaultVisit(symbol);

            if (item == null) return null;

            item.Type = MemberType.Namespace;

            item.Items = VisitDescendants(
                symbol.GetMembers().OfType<ITypeSymbol>(),
                t => t.GetMembers().OfType<ITypeSymbol>(),
                t => true
            );
            return item;
        }

        [return: MaybeNull]
        public override MetadataItem VisitNamedType(INamedTypeSymbol symbol)
        {
            var item = DefaultVisit(symbol);

            if (item == null) return null;

            GenerateInheritance(symbol, item, ApiFilter);

            if (!symbol.IsStatic) item.ExtensionMethods = GenerateExtensionMethods(symbol);

            item.Type = symbol.GetMemberTypeFromTypeKind();
            item.EnsureSyntax();

            item.Syntax.Content = symbol.GenerateSyntax(item.Type, ApiFilter);

            item.Syntax.TypeParameters = symbol.TypeParameters
                .Select(x => ApiParameter.Create(x, item))
                .ToList();

            if (symbol.TypeKind == TypeKind.Delegate)
                AddMethodSyntax(symbol.DelegateInvokeMethod!, item);

            item.Modifiers = symbol.GenerateNamedTypeModifiers();

            item.Items = symbol.GetMembers()
                .Where(s => !(s is INamedTypeSymbol))
                .Select(x => x.Accept(this).SomeNotNull())
                .Values()
                .ToList();

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            return item;
        }

        [return: MaybeNull]
        public override MetadataItem VisitMethod(IMethodSymbol symbol)
        {
            var item = GetMetadataItem(symbol);

            if (item == null) return null;

            item.EnsureSyntax();

            if (symbol.TypeParameters.IsNotEmpty())
                item.Syntax.TypeParameters = symbol.TypeParameters
                    .Select(p => ApiParameter.Create(p, item))
                    .ToList();

            AddMethodSyntax(symbol, item);

            var syntaxContent = symbol.GenerateSyntax(item.Type, ApiFilter);

            if (!string.IsNullOrWhiteSpace(syntaxContent)) item.Syntax.Content = syntaxContent;

            item.Modifiers = symbol.GetMemberModifiersAsStrings();

            item.Implements = symbol.GetMethodImplements(ApiFilter);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            item.IsExplicitInterfaceImplementation =
                !symbol.ExplicitInterfaceImplementations.IsEmpty;
            item.IsExtensionMethod = symbol.IsExtensionMethod;

            return item;
        }

        [return: MaybeNull]
        public override MetadataItem VisitField(IFieldSymbol symbol)
        {
            var item = GetMetadataItem(symbol);

            if (item == null) return null;

            item.EnsureSyntax();

            item.Syntax.Content = symbol.GenerateSyntax(item.Type, ApiFilter);
            item.Modifiers      = symbol.GetFieldModifiers();
            item.Syntax.Return  = CreateParameter(symbol, item, true);
            item.Attributes     = GetAttributeInfo(symbol.GetAttributes());

            return item;
        }

        [return: MaybeNull]
        public override MetadataItem VisitEvent(IEventSymbol symbol)
        {
            var item = GetMetadataItem(symbol);

            if (item == null) return null;

            item.EnsureSyntax();

            item.Syntax.Content = symbol.GenerateSyntax(item.Type, ApiFilter);
            item.Modifiers      = symbol.GetMemberModifiersAsStrings();

            item.Syntax.Return = CreateParameter(symbol, item, true);

            item.Implements = symbol.GetMemberImplements(ApiFilter);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            item.IsExplicitInterfaceImplementation =
                !symbol.ExplicitInterfaceImplementations.IsEmpty;

            return item;
        }

        [return: MaybeNull]
        public override MetadataItem VisitProperty(IPropertySymbol symbol)
        {
            var item = GetMetadataItem(symbol);

            if (item == null) return null;

            item.EnsureSyntax();

            item.Syntax.Content = symbol.GenerateSyntax(item.Type, ApiFilter);

            item.Syntax.Parameters = symbol.Parameters
                .Select(x => CreateParameter(x, item))
                .ToList();

            item.Syntax.Return = CreateParameter(symbol, item, true);

            item.Modifiers = symbol.GeneratePropertyModifiers();

            item.Implements = symbol.GetMemberImplements(ApiFilter);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            item.IsExplicitInterfaceImplementation =
                !symbol.ExplicitInterfaceImplementations.IsEmpty;

            return item;
        }

        static ApiParameter CreateParameter(
            ISymbol symbol,
            MetadataItem item,
            bool isReturn = false
        )
        {
            var type = symbol switch
            {
                IParameterSymbol s => s.Type,
                IEventSymbol s     => s.Type,
                IPropertySymbol s  => s.Type,
                IFieldSymbol s     => s.Type,
                _ => throw new ArgumentException(
                    $"Symbol {symbol.Kind} is not supported",
                    nameof(symbol)
                )
            };
            return ApiParameter.Create(symbol, item, type.ToDisplayString(), isReturn);
        }

        MetadataItem? GetMetadataItem(ISymbol symbol)
        {
            var item = DefaultVisit(symbol);
            if (item == null) return null;

            item.Type = symbol.GetMemberTypeFromSymbol();
            return item.Type == MemberType.Default ? null : item;
        }

        List<MetadataItem> VisitDescendants<T>(
            IEnumerable<T> children,
            Func<T, IEnumerable<T>> getChildren,
            Func<T, bool> filter
        ) where T : ISymbol
        {
            var result = new List<MetadataItem>();
            var stack  = new Stack<T>(children.Reverse());

            while (stack.Count > 0)
            {
                var child = stack.Pop();

                if (filter(child)) result.AddNotNull(child.Accept(this));

                foreach (var m in getChildren(child).Reverse()) stack.Push(m);
            }

            return result;
        }

        static void GenerateInheritance(
            INamedTypeSymbol symbol,
            MetadataItem item,
            IApiFilter apiFilter
        )
        {
            var dict = new Dictionary<string, string?>();

            switch (symbol.TypeKind)
            {
                case TypeKind.Class:
                case TypeKind.Struct:
                {
                    var type        = symbol;
                    var inheritance = new List<string>();

                    var typeParameterNames = symbol.IsGenericType
                        ? symbol.Accept(TypeGenericParameterNameVisitor.Instance)
                        : EmptyListOfString;

                    while (type != null)
                    {
                        inheritance.AddWhen(type.Kind == SymbolKind.ErrorType, "System.Object");
                        inheritance.AddWhen(!type.Equals(symbol), type.ToDisplayString());

                        AddInheritedMembers(symbol, type, dict, typeParameterNames, apiFilter);
                        type = type.BaseType;
                    }

                    if (symbol.TypeKind == TypeKind.Class)
                    {
                        inheritance.Reverse();
                        item.Inheritance = inheritance;
                    }

                    item.Implements = symbol.AllInterfaces
                        .Where(t => apiFilter.CanVisitApi(t))
                        .Select(x => x.ToDisplayString())
                        .ToList();

                    break;
                }
                case TypeKind.Interface:
                {
                    var typeParameterNames = symbol.IsGenericType
                        ? symbol.Accept(TypeGenericParameterNameVisitor.Instance)
                        : EmptyListOfString;
                    AddInheritedMembers(symbol, symbol, dict, typeParameterNames, apiFilter);

                    foreach (var t in symbol.AllInterfaces)
                        AddInheritedMembers(symbol, t, dict, typeParameterNames, apiFilter);

                    break;
                }
            }

            item.InheritedMembers = dict.Values.Where(r => r != null).ToList()!;
        }

        List<string> GenerateExtensionMethods(INamedTypeSymbol symbol)
        {
            var extensions = new List<string>();

            foreach (var (comp, symbols) in _extensionMethods)
            {
                var retargetedSymbol = symbol;

                // get retargeted symbol for cross-assembly case.
                if (comp != _currentCompilation)
                {
                    var compilation =
                        comp.References.Any(r => r.Display == _currentCompilationRef.Display)
                            ? comp
                            : comp.AddReferences(_currentCompilationRef);
                    retargetedSymbol = compilation.FindSymbol(symbol);
                }

                if (retargetedSymbol == null) continue;

                extensions.AddRange(
                    symbols.Select(e => e.ReduceExtensionMethod(retargetedSymbol))
                        .Where(reduced => reduced != null)
                        .Select(x => x!.ReducedFrom!.ToDisplayString())
                );
            }

            return extensions;
        }

        static void AddInheritedMembers(
            ITypeSymbol symbol,
            INamespaceOrTypeSymbol type,
            Dictionary<string, string?> dict,
            IReadOnlyList<string> typeParameterNames,
            IApiFilter apiFilter
        )
        {
            foreach (var m in type.GetMembers()
                .Where(m => !(m is INamedTypeSymbol))
                .Where(
                    m => apiFilter.CanVisitApi(
                        m,
                        symbol.Equals(type) ||
                        !symbol.IsSealed ||
                        symbol.TypeKind != TypeKind.Struct
                    )
                )
                .Where(x => x.IsInheritable()))
            {
                var id = symbol.ToString()?.Trim();
                if (id != null) dict.TryAdd(id, type.Equals(symbol) ? null : m.ToDisplayString());
            }
        }

        void AddMethodSyntax(IMethodSymbol symbol, MetadataItem item)
        {
            if (!symbol.ReturnsVoid)
                item.Syntax.Return = CreateApiParameter(symbol, symbol.ReturnType, true);

            item.Syntax.Parameters = symbol.Parameters
                .Select(p => CreateApiParameter(p, p.Type, false))
                .ToList();

            ApiParameter CreateApiParameter(ISymbol p, ITypeSymbol type, bool isReturn)
            {
                var param = ApiParameter.Create(p, item, type.ToDisplayString(), isReturn);
                param.Attributes = GetAttributeInfo(p.GetAttributes());
                return param;
            }
        }

        TripleSlashCommentParserContext GetTripleSlashCommentParserContext(MetadataItem item)
            => new TripleSlashCommentParserContext
            {
                AddReferenceDelegate = Console.WriteLine,
                Source               = item.Source,
                CodeSourceBasePath   = _codeSourceBasePath
            };

        List<AttributeInfo> GetAttributeInfo(ImmutableArray<AttributeData> attributes)
            => attributes
                .Where(
                    attr => !(attr.AttributeClass is IErrorTypeSymbol) &&
                        attr.AttributeConstructor != null
                )
                .Where(attr => ApiFilter.CanVisitAttribute(attr.AttributeConstructor))
                .Select(
                    attr => new AttributeInfo
                    {
                        Type           = attr.AttributeClass.ToDisplayString(),
                        Constructor    = attr.AttributeConstructor.ToDisplayString(),
                        Arguments      = GetArguments(attr),
                        NamedArguments = GetNamedArguments(attr)
                    }
                )
                .Where(attr => attr.Arguments != null)
                .ToList();

        List<ArgumentInfo> GetArguments(AttributeData attr)
            => attr.ConstructorArguments.Select(GetArgumentInfo).Values().ToList();

        Option<ArgumentInfo> GetArgumentInfo(TypedConstant arg)
            => GetArgumentInfoBase(arg).Map(x => new ArgumentInfo {Type = x.Type, Value = x.Value});

        List<NamedArgumentInfo> GetNamedArguments(AttributeData attr)
            => attr.NamedArguments.Select(GetNamedArgumentInfo).Values().ToList();

        Option<NamedArgumentInfo> GetNamedArgumentInfo(KeyValuePair<string, TypedConstant> pair)
        {
            var (key, arg) = pair;
            var argumentInfo = GetArgumentInfoBase(arg);

            return argumentInfo.Map(
                x => new NamedArgumentInfo {Name = key, Value = x.Value, Type = x.Type}
            );
        }

        Option<(string Type, object? Value)> GetArgumentInfoBase(TypedConstant arg)
        {
            if (arg.Type.TypeKind == TypeKind.Array)
                return Option.None<(string Type, object? Value)>();

            object? value;

            switch (arg.Value)
            {
                case null: return (arg.Type.ToDisplayString(), (object?) null).Some();
                case ITypeSymbol type when !ApiFilter.CanVisitApi(type):
                    return Option.None<(string Type, object? Value)>();
                case ITypeSymbol type:
                    value = type.ToDisplayString();
                    break;
                default:
                    value = Convert.GetTypeCode(arg.Value) switch
                    {
                        TypeCode.UInt32 => arg.Value.ToString(),
                        TypeCode.Int64  => arg.Value.ToString(),
                        TypeCode.UInt64 => arg.Value.ToString(),
                        _               => arg.Value
                    };
                    break;
            }

            return (arg.Type.ToDisplayString(), value).Some();
        }
    }
}
