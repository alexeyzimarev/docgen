using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Optional;
using Optional.Collections;
using Ubiquitous.DocGen.Metadata.Comments;
using Ubiquitous.DocGen.Metadata.Extensions;
using Ubiquitous.DocGen.Metadata.Models;
using Ubiquitous.DocGen.Metadata.Roslyn;
using Ubiquitous.DocGen.Metadata.Visitors;

namespace Ubiquitous.DocGen.Metadata.CodeAnalysis
{
    public class MetadataSymbolVisitor
        : SymbolVisitor<MetadataItem>
    {
        static readonly Regex MemberSigRegex =
            new Regex(@"^([\w\{\}`]+\.)+", RegexOptions.Compiled);

        static readonly IReadOnlyList<string> EmptyListOfString = new string[0];

        readonly IReadOnlyDictionary<Compilation, IEnumerable<IMethodSymbol>> _extensionMethods;

        readonly Compilation          _currentCompilation;
        readonly CompilationReference _currentCompilationRef;
        readonly string               _codeSourceBasePath;
        References                    _references;

        public MetadataSymbolVisitor(Compilation compilation, ExtractMetadataOptions options)
        {
            _currentCompilation    = compilation;
            _currentCompilationRef = compilation.ToMetadataReference();
            ApiFilter              = new DefaultApiFilter();

            _extensionMethods = options.RoslynExtensionMethods != null
                ? options.RoslynExtensionMethods.ToDictionary(
                    p => p.Key,
                    p => p.Value.Where(e => ApiFilter.CanVisitApi(e))
                )
                : new Dictionary<Compilation, IEnumerable<IMethodSymbol>>();
            _codeSourceBasePath = options.CodeSourceBasePath;
        }

        public IApiFilter ApiFilter { get; }

        public override MetadataItem DefaultVisit(ISymbol symbol)
        {
            if (!ApiFilter.CanVisitApi(symbol))
                return null;

            var item = new MetadataItem
            {
                Name       = symbol.GetRawId(),
                CommentId  = symbol.GetCommentId(),
                RawComment = symbol.GetDocumentationCommentXml(),
                Source     = symbol.GetSourceDetail()
            };

            var assemblyName = symbol.ContainingAssembly?.Name;
            item.AssemblyNameList = string.IsNullOrEmpty(assemblyName) ? null : new List<string> {assemblyName};

            if (!(symbol is INamespaceSymbol))
            {
                var namespaceName = symbol.ContainingNamespace.GetRawId();
                item.NamespaceName = namespaceName;
            }

            item.AddComments(GetTripleSlashCommentParserContext(item));

            _references.AddExceptions(item.Exceptions);
            _references.AddLinks(item.Sees?.Where(x => x.LinkType     == LinkType.CRef));
            _references.AddLinks(item.SeeAlsos?.Where(x => x.LinkType == LinkType.CRef));

            item.DisplayName = NameVisitorFactory
                .GetCSharp(NameOptions.WithGenericParameter | NameOptions.WithParameter)
                .GetName(symbol);

            item.DisplayNameWithType = NameVisitorFactory
                .GetCSharp(NameOptions.WithType | NameOptions.WithGenericParameter | NameOptions.WithParameter)
                .GetName(symbol);

            item.DisplayQualifiedName = NameVisitorFactory
                .GetCSharp(NameOptions.Qualified | NameOptions.WithGenericParameter | NameOptions.WithParameter)
                .GetName(symbol);

            return item;
        }

        public override MetadataItem VisitAssembly(IAssemblySymbol symbol)
        {
            var item = new MetadataItem
            {
                Name                 = symbol.GetRawId(),
                RawComment           = symbol.GetDocumentationCommentXml(),
                DisplayName          = symbol.MetadataName,
                DisplayQualifiedName = symbol.MetadataName,
                Type                 = MemberType.Assembly,
            };

            _references = new References();

            var namespaces = symbol.GlobalNamespace.GetNamespaceMembers();

            item.Items = VisitDescendants(
                namespaces,
                ns => ns.GetMembers().OfType<INamespaceSymbol>(),
                ns => ns.GetMembers().OfType<INamedTypeSymbol>().Any(t => ApiFilter.CanVisitApi(t))
            );
            item.References = _references;
            return item;
        }

        public override MetadataItem VisitNamespace(INamespaceSymbol symbol)
        {
            var item = DefaultVisit(symbol);

            if (item == null)
                return null;

            item.Type = MemberType.Namespace;

            item.Items = VisitDescendants(
                symbol.GetMembers().OfType<ITypeSymbol>(),
                t => t.GetMembers().OfType<ITypeSymbol>(),
                t => true
            );
            AddReference(symbol);
            return item;
        }

        public override MetadataItem VisitNamedType(INamedTypeSymbol symbol)
        {
            var item = DefaultVisit(symbol);

            if (item == null)
                return null;

            GenerateInheritance(symbol, item);

            if (!symbol.IsStatic)
                item.ExtensionMethods = GenerateExtensionMethods(symbol);

            item.Type = symbol.GetMemberTypeFromTypeKind();
            item.EnsureSyntax();

            item.Syntax.Content = symbol.GenerateSyntax(item.Type, ApiFilter);

            item.Syntax.TypeParameters =
                symbol.TypeParameters.Select(x => ApiParameter.Create(x, item)).ToList();

            if (symbol.TypeKind == TypeKind.Delegate)
                AddMethodSyntax(symbol.DelegateInvokeMethod, item);

            item.Modifiers = symbol.GenerateNamedTypeModifiers();

            item.Items = symbol.GetMembers()
                .Where(s => !(s is INamedTypeSymbol))
                .Select(x => x.Accept(this).SomeNotNull())
                .Values()
                .ToList();

            AddReference(symbol);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            return item;
        }

        public override MetadataItem VisitMethod(IMethodSymbol symbol)
        {
            var item = GetMetadataItem(symbol);

            if (item == null)
                return null;

            item.EnsureSyntax();

            if (symbol.TypeParameters.IsNotEmpty())
                item.Syntax.TypeParameters = symbol.TypeParameters.Select(p => ApiParameter.Create(p, item)).ToList();

            AddMethodSyntax(symbol, item);

            var syntaxContent = symbol.GenerateSyntax(item.Type, ApiFilter);

            if (!string.IsNullOrWhiteSpace(syntaxContent))
                item.Syntax.Content = syntaxContent;

            item.Modifiers = symbol.GetMemberModifiersAsStrings();

            if (symbol.IsOverride && symbol.OverriddenMethod != null)
                item.Overridden = AddSpecReference(symbol.OverriddenMethod);

            item.Overload = AddOverloadReference(symbol.OriginalDefinition);

            AddMemberImplements(symbol, item);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            item.IsExplicitInterfaceImplementation = !symbol.ExplicitInterfaceImplementations.IsEmpty;
            item.IsExtensionMethod                 = symbol.IsExtensionMethod;

            return item;
        }

        public override MetadataItem VisitField(IFieldSymbol symbol)
        {
            var item = GetMetadataItem(symbol);

            if (item == null)
                return null;

            item.EnsureSyntax();

            item.Syntax.Content = symbol.GenerateSyntax(item.Type, ApiFilter);
            symbol.GenerateField(item);

            item.Syntax.Return = CreateParameter(symbol, item, true);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            return item;
        }

        public override MetadataItem VisitEvent(IEventSymbol symbol)
        {
            var item = GetMetadataItem(symbol);

            if (item == null)
                return null;

            item.EnsureSyntax();

            item.Syntax.Content = symbol.GenerateSyntax(item.Type, ApiFilter);
            item.Modifiers      = symbol.GetMemberModifiersAsStrings();

            if (symbol.IsOverride && symbol.OverriddenEvent != null)
                item.Overridden = AddSpecReference(symbol.OverriddenEvent);

            item.Syntax.Return = CreateParameter(symbol, item, true);

            AddMemberImplements(symbol, item);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            item.IsExplicitInterfaceImplementation = !symbol.ExplicitInterfaceImplementations.IsEmpty;

            return item;
        }

        public override MetadataItem VisitProperty(IPropertySymbol symbol)
        {
            var item = GetMetadataItem(symbol);

            if (item == null)
                return null;

            item.EnsureSyntax();

            item.Syntax.Content = symbol.GenerateSyntax(item.Type, ApiFilter);

            item.Syntax.Parameters = symbol.Parameters
                .Select(x => CreateParameter(x, item))
                .ToList();

            item.Syntax.Return = CreateParameter(symbol, item, true);

            if (symbol.IsOverride && symbol.OverriddenProperty != null)
                item.Overridden = AddSpecReference(symbol.OverriddenProperty);

            item.Overload = AddOverloadReference(symbol.OriginalDefinition);

            item.Modifiers = symbol.GeneratePropertyModifiers();

            AddMemberImplements(symbol, item);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            item.IsExplicitInterfaceImplementation = !symbol.ExplicitInterfaceImplementations.IsEmpty;

            return item;
        }

        ApiParameter CreateParameter(ISymbol symbol, MetadataItem item, bool isReturn = false)
        {
            var type = symbol switch
            {
                IParameterSymbol s => s.Type,
                IEventSymbol s     => s.Type,
                IPropertySymbol s  => s.Type,
                IFieldSymbol s     => s.Type,
                _                  => throw new ArgumentException($"Symbol {symbol.Kind} is not supported", nameof(symbol))
            };
            return ApiParameter.Create(symbol, item, AddSpecReference(type), isReturn);
        }

        void AddReference(ISymbol symbol)
        {
            var memberType = symbol.GetMemberTypeFromSymbol();

            if (memberType == MemberType.Default)
                throw new InvalidOperationException("Unexpected member type");

            _references.AddReference(symbol);
        }

        void AddReference(string id, string commentId) => _references.AddCommentReference(id, commentId);

        string AddOverloadReference(ISymbol symbol)
        {
            var memberType = symbol.GetMemberTypeFromSymbol();

            switch (memberType)
            {
                case MemberType.Property:
                case MemberType.Constructor:
                case MemberType.Method:
                case MemberType.Operator:
                    return _references.AddOverloadReference(symbol);
                default:
                    Debug.Fail("Unexpected membertype.");
                    throw new InvalidOperationException("Unexpected membertype.");
            }
        }

        string AddSpecReference(ISymbol symbol)
        {
            try
            {
                return _references.AddSpecReference(symbol);
            }
            catch (Exception ex)
            {
                throw new Exception($"Unable to generate spec reference for {symbol.GetCommentId()}", ex);
            }
        }

        MetadataItem GetMetadataItem(ISymbol symbol)
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
        )
            where T : ISymbol
        {
            var result = new List<MetadataItem>();
            var stack  = new Stack<T>(children.Reverse());

            while (stack.Count > 0)
            {
                var child = stack.Pop();

                if (filter(child))
                {
                    result.AddNotNull(child.Accept(this));
                }

                foreach (var m in getChildren(child).Reverse())
                {
                    stack.Push(m);
                }
            }

            return result;
        }

        void GenerateInheritance(INamedTypeSymbol symbol, MetadataItem item)
        {
            Dictionary<string, string> dict = null;

            switch (symbol.TypeKind)
            {
                case TypeKind.Class:
                case TypeKind.Struct:
                {
                    var type        = symbol;
                    var inheritance = new List<string>();
                    dict = new Dictionary<string, string>();

                    var typeParameterNames = symbol.IsGenericType
                        ? symbol.Accept(TypeGenericParameterNameVisitor.Instance)
                        : EmptyListOfString;

                    while (type != null)
                    {
                        inheritance.AddWhen(type.Kind == SymbolKind.ErrorType, "System.Object");
                        inheritance.AddWhen(!type.Equals(symbol), AddSpecReference(type));

                        AddInheritedMembers(symbol, type, dict, typeParameterNames);
                        type = type.BaseType;
                    }

                    if (symbol.TypeKind == TypeKind.Class)
                    {
                        inheritance.Reverse();
                        item.Inheritance = inheritance;
                    }

                    item.Implements = symbol.AllInterfaces
                        .Where(t => ApiFilter.CanVisitApi(t))
                        .Select(AddSpecReference)
                        .ToList();

                    break;
                }
                case TypeKind.Interface:
                {
                    dict = new Dictionary<string, string>();

                    var typeParameterNames = symbol.IsGenericType
                        ? symbol.Accept(TypeGenericParameterNameVisitor.Instance)
                        : EmptyListOfString;
                    AddInheritedMembers(symbol, symbol, dict, typeParameterNames);

                    foreach (var t in symbol.AllInterfaces)
                    {
                        AddInheritedMembers(symbol, t, dict, typeParameterNames);
                    }

                    break;
                }
            }

            item.InheritedMembers = dict?.Values.Where(r => r != null).ToList();
        }

        void AddMemberImplements(ISymbol symbol, MetadataItem item)
            => item.Implements = symbol.ContainingType.AllInterfaces
                .Where(type => ApiFilter.CanVisitApi(type))
                .SelectMany(type => type.GetMembers(), (type, member) => new {type, member})
                .Where(x => ApiFilter.CanVisitApi(x.member))
                .Where(x => symbol.Equals(symbol.ContainingType.FindImplementationForInterfaceMember(x.member)))
                .Select(x => AddSpecReference(x.member))
                .ToList();

        void AddMemberImplements(IMethodSymbol symbol, MetadataItem item)
            => item.Implements = symbol.ContainingType.AllInterfaces
                .Where(type => ApiFilter.CanVisitApi(type))
                .SelectMany(type => type.GetMembers(), (type, member) => new {type, member})
                .Where(x => ApiFilter.CanVisitApi(x.member))
                .Where(x => symbol.Equals(symbol.ContainingType.FindImplementationForInterfaceMember(x.member)))
                .Select(
                    x => AddSpecReference(
                        symbol.TypeParameters.Length == 0
                            ? x.member
                            : ((IMethodSymbol) x.member).Construct(symbol.TypeParameters.ToArray<ITypeSymbol>())
                    )
                )
                .ToList();

        List<string> GenerateExtensionMethods(INamedTypeSymbol symbol)
        {
            var extensions = new List<string>();

            foreach (var (comp, symbols) in _extensionMethods.Where(p => p.Key.Language == symbol.Language))
            {
                ITypeSymbol retargetedSymbol = symbol;

                // get retargeted symbol for cross-assembly case.
                if (comp != _currentCompilation)
                {
                    var compilation = comp.References.Any(r => r.Display == _currentCompilationRef.Display)
                        ? comp
                        : comp.AddReferences(_currentCompilationRef);
                    retargetedSymbol = compilation.FindSymbol(symbol);
                }

                if (retargetedSymbol == null)
                {
                    continue;
                }

                extensions.AddRange(
                    symbols
                        .Select(e => e.ReduceExtensionMethod(retargetedSymbol))
                        .Where(reduced => reduced != null)
                        .Select(AddSpecReference)
                );
            }

            return extensions;
        }

        void AddInheritedMembers(
            ITypeSymbol symbol, INamespaceOrTypeSymbol type, Dictionary<string, string> dict,
            IReadOnlyList<string> typeParameterNames
        )
        {
            foreach (var m in type.GetMembers()
                .Where(m => !(m is INamedTypeSymbol))
                .Where(m => ApiFilter.CanVisitApi(m, symbol.Equals(type) || !symbol.IsSealed || symbol.TypeKind != TypeKind.Struct))
                .Where(x => x.IsInheritable()))
            {
                var id = symbol.ToString()?.Trim();
                var sig = id; //MemberSigRegex.Replace(m.GetSpecId(), string.Empty);
                dict.TryAdd(sig, type.Equals(symbol) ? null : AddSpecReference(m));
            }
        }

        void AddMethodSyntax(IMethodSymbol symbol, MetadataItem item)
        {
            if (!symbol.ReturnsVoid)
                item.Syntax.Return = CreateApiParameter(symbol, symbol.ReturnType, true);

            item.Syntax.Parameters =
                symbol.Parameters.Select(p => CreateApiParameter(p, p.Type, false)).ToList();

            ApiParameter CreateApiParameter(ISymbol p, ITypeSymbol type, bool isReturn)
            {
                var id    = AddSpecReference(type);
                var param = ApiParameter.Create(p, item, id, isReturn);
                param.Attributes = GetAttributeInfo(p.GetAttributes());
                return param;
            }
        }

        TripleSlashCommentParserContext GetTripleSlashCommentParserContext(MetadataItem item)
        {
            return new TripleSlashCommentParserContext
            {
                AddReferenceDelegate = GetAddReferenceDelegate,
                Source               = item.Source,
                CodeSourceBasePath   = _codeSourceBasePath
            };

            void GetAddReferenceDelegate(string id, string commentId)
            {
                AddReference(id, commentId);

                if (item.References == null)
                    item.References = new References();

                // only record the id now, the value would be fed at later phase after merge
                item.References.ClearReference(id);
            }
        }

        List<AttributeInfo> GetAttributeInfo(ImmutableArray<AttributeData> attributes)
            => attributes
                .Where(attr => !(attr.AttributeClass is IErrorTypeSymbol) && attr.AttributeConstructor != null)
                .Where(attr => ApiFilter.CanVisitAttribute(attr.AttributeConstructor))
                .Select(
                    attr => new AttributeInfo
                    {
                        Type           = AddSpecReference(attr.AttributeClass),
                        Constructor    = AddSpecReference(attr.AttributeConstructor),
                        Arguments      = GetArguments(attr),
                        NamedArguments = GetNamedArguments(attr)
                    }
                )
                .Where(attr => attr.Arguments != null)
                .ToList();

        List<ArgumentInfo> GetArguments(AttributeData attr) => attr.ConstructorArguments.Select(GetArgumentInfo).Values().ToList();

        Option<ArgumentInfo> GetArgumentInfo(TypedConstant arg)
            => GetArgumentInfoBase(arg)
                .Map(
                    x => new ArgumentInfo
                    {
                        Type  = x.Type,
                        Value = x.Value
                    }
                );

        List<NamedArgumentInfo> GetNamedArguments(AttributeData attr) => attr.NamedArguments.Select(GetNamedArgumentInfo).Values().ToList();

        Option<NamedArgumentInfo> GetNamedArgumentInfo(KeyValuePair<string, TypedConstant> pair)
        {
            var (key, arg) = pair;
            var argumentInfo = GetArgumentInfoBase(arg);

            return argumentInfo.Map(
                x => new NamedArgumentInfo
                {
                    Name  = key,
                    Value = x.Value,
                    Type  = x.Type
                }
            );
        }

        Option<(string Type, object Value)> GetArgumentInfoBase(TypedConstant arg)
        {
            if (arg.Type.TypeKind == TypeKind.Array)
                return Option.None<(string Type, object Value)>();

            object value;

            switch (arg.Value)
            {
                case null:
                    return (AddSpecReference(arg.Type), (object) null).Some();
                case ITypeSymbol type when !ApiFilter.CanVisitApi(type):
                    return Option.None<(string Type, object Value)>();
                case ITypeSymbol type:
                    value = AddSpecReference(type);
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

            return (AddSpecReference(arg.Type), value).Some();
        }
    }
}