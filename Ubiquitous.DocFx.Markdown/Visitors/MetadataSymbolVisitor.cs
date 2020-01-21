using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.CodeAnalysis;
using Optional;
using Optional.Collections;
using Ubiquitous.DocFx.Markdown.Extensions;
using Ubiquitous.DocFx.Markdown.Models;
using Ubiquitous.DocFx.Markdown.Parsers;

namespace Ubiquitous.DocFx.Markdown.Visitors
{
    public class MetadataSymbolVisitor
        : SymbolVisitor<MetadataItem>
    {
        static readonly Regex MemberSigRegex =
            new Regex(@"^([\w\{\}`]+\.)+", RegexOptions.Compiled);

        static readonly IReadOnlyList<string> EmptyListOfString = new string[0];

        readonly CsMetadataGenerator                                          _generator;
        Dictionary<string, ReferenceItem>                                     _references;
        readonly IReadOnlyDictionary<Compilation, IEnumerable<IMethodSymbol>> _extensionMethods;
        readonly Compilation                                                  _currentCompilation;
        readonly CompilationReference                                         _currentCompilationRef;
        readonly string                                                       _codeSourceBasePath;

        public MetadataSymbolVisitor(
            CsMetadataGenerator generator, Compilation compilation,
            ExtractMetadataOptions options
        )
        {
            _generator             = generator;
            _currentCompilation    = compilation;
            _currentCompilationRef = compilation.ToMetadataReference();
            FilterVisitor          = new DefaultFilterVisitor();
            _extensionMethods = options.RoslynExtensionMethods != null
                ? options.RoslynExtensionMethods.ToDictionary(p => p.Key,
                    p => p.Value.Where(e => FilterVisitor.CanVisitApi(e)))
                : new Dictionary<Compilation, IEnumerable<IMethodSymbol>>();
            _codeSourceBasePath = options.CodeSourceBasePath;
        }

        public IFilterVisitor FilterVisitor { get; }

        public override MetadataItem DefaultVisit(ISymbol symbol)
        {
            if (!FilterVisitor.CanVisitApi(symbol))
                return null;

            var item = new MetadataItem
            {
                Name       = symbol.GetId(),
                CommentId  = symbol.GetCommentId(),
                RawComment = symbol.GetDocumentationCommentXml(),
                Source     = symbol.GetSourceDetail()
            };

            var assemblyName = symbol.ContainingAssembly?.Name;
            item.AssemblyNameList = string.IsNullOrEmpty(assemblyName) ? null : new List<string> {assemblyName};
            if (!(symbol is INamespaceSymbol))
            {
                var namespaceName = symbol.ContainingNamespace.GetId();
                item.NamespaceName = string.IsNullOrEmpty(namespaceName) ? null : namespaceName;
            }

            item.AddComments(GetTripleSlashCommentParserContext(item));
            if (item.Exceptions != null)
            {
                foreach (var exceptions in item.Exceptions)
                {
                    AddReference(exceptions.Type, exceptions.CommentId);
                }
            }

            if (item.Sees != null)
            {
                foreach (var i in item.Sees.Where(l => l.LinkType == LinkType.CRef))
                {
                    AddReference(i.LinkId, i.CommentId);
                }
            }

            if (item.SeeAlsos != null)
            {
                foreach (var i in item.SeeAlsos.Where(l => l.LinkType == LinkType.CRef))
                {
                    AddReference(i.LinkId, i.CommentId);
                }
            }

            CsMetadataGenerator.DefaultVisit(symbol, item);
            return item;
        }

        public override MetadataItem VisitAssembly(IAssemblySymbol symbol)
        {
            var item = new MetadataItem
            {
                Name                 = symbol.GetId(),
                RawComment           = symbol.GetDocumentationCommentXml(),
                DisplayName          = symbol.MetadataName,
                DisplayQualifiedName = symbol.MetadataName,
                Type                 = MemberType.Assembly,
            };

            _references = new Dictionary<string, ReferenceItem>();

            var namespaces = //!string.IsNullOrEmpty(VisitorHelper.GlobalNamespaceId)
                // ? Enumerable.Repeat(symbol.GlobalNamespace, 1)
                //:
                symbol.GlobalNamespace.GetNamespaceMembers();

            item.Items = VisitDescendants(
                namespaces,
                ns => ns.GetMembers().OfType<INamespaceSymbol>(),
                ns => ns.GetMembers().OfType<INamedTypeSymbol>().Any(t => FilterVisitor.CanVisitApi(t)));
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
                t => true);
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
            {
                GenerateExtensionMethods(symbol, item);
            }

            item.Type = GetMemberTypeFromTypeKind(symbol.TypeKind);
            item.EnsureSyntax();

            _generator.GenerateSyntax(item.Type, symbol, item.Syntax, this);

            if (symbol.TypeParameters.Length > 0)
            {
                item.Syntax.TypeParameters
                    .CreateIfNull()
                    .AddRange(symbol.TypeParameters.Select(x => ApiParameter.Create(x, item)));
            }

            if (symbol.TypeKind == TypeKind.Delegate)
            {
                var typeGenericParameters = symbol.IsGenericType
                    ? symbol.Accept(TypeGenericParameterNameVisitor.Instance)
                    : EmptyListOfString;
                AddMethodSyntax(symbol.DelegateInvokeMethod, item, typeGenericParameters, EmptyListOfString);
            }

            CsMetadataGenerator.GenerateNamedType(symbol, item);

            item.Items = symbol.GetMembers()
                .Where(s => !(s is INamedTypeSymbol))
                .Select(x => x.Accept(this).SomeNotNull())
                .Values().ToList();

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

            if (symbol.TypeParameters.Length > 0)
            {
                item.Syntax.TypeParameters
                    .CreateIfNull()
                    .AddRange(symbol.TypeParameters.Select(p => ApiParameter.Create(p, item)));
            }

            var typeGenericParameters = symbol.ContainingType.IsGenericType
                ? symbol.ContainingType.Accept(TypeGenericParameterNameVisitor.Instance)
                : EmptyListOfString;

            var methodGenericParameters = symbol.IsGenericMethod
                ? (from p in symbol.TypeParameters select p.Name).ToList()
                : EmptyListOfString;

            AddMethodSyntax(symbol, item, typeGenericParameters, methodGenericParameters);

            _generator.GenerateSyntax(item.Type, symbol, item.Syntax, this);

            CsMetadataGenerator.GenerateMethod(symbol, item);

            if (symbol.IsOverride && symbol.OverriddenMethod != null)
            {
                item.Overridden = AddSpecReference(symbol.OverriddenMethod, typeGenericParameters,
                    methodGenericParameters);
            }

            item.Overload = AddOverloadReference(symbol.OriginalDefinition);

            AddMemberImplements(symbol, item, typeGenericParameters, methodGenericParameters);

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

            _generator.GenerateSyntax(item.Type, symbol, item.Syntax, this);
            CsMetadataGenerator.GenerateField(symbol, item);

            var typeGenericParameters = symbol.ContainingType.IsGenericType
                ? symbol.ContainingType.Accept(TypeGenericParameterNameVisitor.Instance)
                : EmptyListOfString;

            var id = AddSpecReference(symbol.Type, typeGenericParameters);
            item.Syntax.Return = ApiParameter.Create(symbol, item, id, true);
            Debug.Assert(item.Syntax.Return.Type != null);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            return item;
        }

        public override MetadataItem VisitEvent(IEventSymbol symbol)
        {
            var item = GetMetadataItem(symbol);
            if (item == null)
                return null;

            item.EnsureSyntax();

            _generator.GenerateSyntax(item.Type, symbol, item.Syntax, this);
            CsMetadataGenerator.GenerateEvent(symbol, item);

            var typeGenericParameters = symbol.ContainingType.IsGenericType
                ? symbol.ContainingType.Accept(TypeGenericParameterNameVisitor.Instance)
                : EmptyListOfString;

            if (symbol.IsOverride && symbol.OverriddenEvent != null)
            {
                item.Overridden = AddSpecReference(symbol.OverriddenEvent, typeGenericParameters);
            }

            var id = AddSpecReference(symbol.Type, typeGenericParameters);
            item.Syntax.Return = ApiParameter.Create(symbol, item, id, true);
            Debug.Assert(item.Syntax.Return.Type != null);

            AddMemberImplements(symbol, item, typeGenericParameters);

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

            _generator.GenerateSyntax(item.Type, symbol, item.Syntax, this);

            var typeGenericParameters = symbol.ContainingType.IsGenericType
                ? symbol.ContainingType.Accept(TypeGenericParameterNameVisitor.Instance)
                : EmptyListOfString;

            item.Syntax.Parameters = symbol.Parameters
                .Select(x => ApiParameter.Create(x, item,
                    AddSpecReference(x.Type, typeGenericParameters), false))
                .ToList();

            var id = AddSpecReference(symbol.Type, typeGenericParameters);
            item.Syntax.Return = ApiParameter.Create(symbol, item, id, true);
            Debug.Assert(item.Syntax.Return.Type != null);

            if (symbol.IsOverride && symbol.OverriddenProperty != null)
            {
                item.Overridden = AddSpecReference(symbol.OverriddenProperty, typeGenericParameters);
            }

            item.Overload = AddOverloadReference(symbol.OriginalDefinition);

            CsMetadataGenerator.GenerateProperty(symbol, item);

            AddMemberImplements(symbol, item, typeGenericParameters);

            item.Attributes = GetAttributeInfo(symbol.GetAttributes());

            item.IsExplicitInterfaceImplementation = !symbol.ExplicitInterfaceImplementations.IsEmpty;

            return item;
        }

        void AddReference(ISymbol symbol)
        {
            var memberType = GetMemberTypeFromSymbol(symbol);
            if (memberType == MemberType.Default)
            {
                Debug.Fail("Unexpected membertype.");
                throw new InvalidOperationException("Unexpected membertype.");
            }

            _generator.AddReference(symbol, _references);
        }

        void AddReference(string id, string commentId) => CsMetadataGenerator.AddReference(id, commentId, _references);

        string AddOverloadReference(ISymbol symbol)
        {
            var memberType = GetMemberTypeFromSymbol(symbol);
            switch (memberType)
            {
                case MemberType.Property:
                case MemberType.Constructor:
                case MemberType.Method:
                case MemberType.Operator:
                    return CsMetadataGenerator.AddOverloadReference(symbol, _references);
                default:
                    Debug.Fail("Unexpected membertype.");
                    throw new InvalidOperationException("Unexpected membertype.");
            }
        }

        string AddSpecReference(
            ISymbol symbol,
            IReadOnlyList<string> typeGenericParameters = null,
            IReadOnlyList<string> methodGenericParameters = null
        )
        {
            try
            {
                return _generator.AddSpecReference(symbol, typeGenericParameters, methodGenericParameters, _references,
                    this);
            }
            catch (Exception ex)
            {
                throw new Exception($"Unable to generate spec reference for {symbol.GetCommentId()}", ex);
            }
        }

        static MemberType GetMemberTypeFromSymbol(ISymbol symbol)
            => symbol.Kind switch
            {
                SymbolKind.Namespace => MemberType.Namespace,
                SymbolKind.NamedType => (symbol is INamedTypeSymbol nameTypeSymbol
                    ? GetMemberTypeFromTypeKind(nameTypeSymbol.TypeKind)
                    : MemberType.Default),
                SymbolKind.Event    => MemberType.Event,
                SymbolKind.Field    => MemberType.Field,
                SymbolKind.Property => MemberType.Property,
                SymbolKind.Method   => GetMethodMemberType(symbol),
                _                   => MemberType.Default
            };

        static MemberType GetMethodMemberType(ISymbol symbol)
        {
            if (!(symbol is IMethodSymbol methodSymbol)) return MemberType.Default;

            return methodSymbol.MethodKind switch
            {
                MethodKind.AnonymousFunction               => MemberType.Method,
                MethodKind.DelegateInvoke                  => MemberType.Method,
                MethodKind.Destructor                      => MemberType.Method,
                MethodKind.ExplicitInterfaceImplementation => MemberType.Method,
                MethodKind.Ordinary                        => MemberType.Method,
                MethodKind.ReducedExtension                => MemberType.Method,
                MethodKind.DeclareMethod                   => MemberType.Method,
                MethodKind.BuiltinOperator                 => MemberType.Operator,
                MethodKind.UserDefinedOperator             => MemberType.Operator,
                MethodKind.Conversion                      => MemberType.Operator,
                MethodKind.Constructor                     => MemberType.Constructor,
                MethodKind.StaticConstructor               => MemberType.Constructor,
                MethodKind.PropertyGet                     => MemberType.Default,
                MethodKind.PropertySet                     => MemberType.Default,
                MethodKind.EventAdd                        => MemberType.Default,
                MethodKind.EventRemove                     => MemberType.Default,
                MethodKind.EventRaise                      => MemberType.Default,
                _                                          => MemberType.Default
            };
        }

        MetadataItem GetMetadataItem(ISymbol symbol)
        {
            var item = DefaultVisit(symbol);
            if (item == null) return null;
            item.Type = GetMemberTypeFromSymbol(symbol);
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

        static bool IsInheritable(ISymbol memberSymbol)
            => (memberSymbol as IMethodSymbol)?.MethodKind switch
            {
                null                                       => true,
                MethodKind.ExplicitInterfaceImplementation => true,
                MethodKind.DeclareMethod                   => true,
                MethodKind.Ordinary                        => true,
                _                                          => false
            };

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
                        inheritance.AddWhen(!type.Equals(symbol), AddSpecReference(type, typeParameterNames));

                        AddInheritedMembers(symbol, type, dict, typeParameterNames);
                        type = type.BaseType;
                    }

                    if (symbol.TypeKind == TypeKind.Class)
                    {
                        inheritance.Reverse();
                        item.Inheritance = inheritance;
                    }

                    item.Implements = symbol.AllInterfaces
                        .Where(t => FilterVisitor.CanVisitApi(t))
                        .Select(t => AddSpecReference(t, typeParameterNames))
                        .ToList();

                    break;
                }
                case TypeKind.Interface:
                {
                    dict = new Dictionary<string, string>();
                    var typeParamterNames = symbol.IsGenericType
                        ? symbol.Accept(TypeGenericParameterNameVisitor.Instance)
                        : EmptyListOfString;
                    AddInheritedMembers(symbol, symbol, dict, typeParamterNames);
                    foreach (var t in symbol.AllInterfaces)
                    {
                        AddInheritedMembers(symbol, t, dict, typeParamterNames);
                    }

                    break;
                }
            }

            item.InheritedMembers = dict?.Values.Where(r => r != null).ToList();
        }

        void AddMemberImplements(
            ISymbol symbol, MetadataItem item, IReadOnlyList<string> typeGenericParameters = null
        )
        {
            item.Implements = symbol.ContainingType.AllInterfaces
                .Where(type => FilterVisitor.CanVisitApi(type))
                .SelectMany(type => type.GetMembers(), (type, member) => new {type, member})
                .Where(x => FilterVisitor.CanVisitApi(x.member))
                .Where(x => symbol.Equals(symbol.ContainingType.FindImplementationForInterfaceMember(x.member)))
                .Select(x => AddSpecReference(x.member, typeGenericParameters))
                .ToList();
        }

        void AddMemberImplements(
            IMethodSymbol symbol, MetadataItem item, IReadOnlyList<string> typeGenericParameters = null,
            IReadOnlyList<string> methodGenericParameters = null
        )
        {
            item.Implements = symbol.ContainingType.AllInterfaces
                .Where(type => FilterVisitor.CanVisitApi(type))
                .SelectMany(type => type.GetMembers(), (type, member) => new {type, member})
                .Where(x => FilterVisitor.CanVisitApi(x.member))
                .Where(x => symbol.Equals(symbol.ContainingType.FindImplementationForInterfaceMember(x.member)))
                .Select(x => AddSpecReference(
                    symbol.TypeParameters.Length == 0
                        ? x.member
                        : ((IMethodSymbol) x.member).Construct(symbol.TypeParameters.ToArray<ITypeSymbol>()),
                    typeGenericParameters, methodGenericParameters))
                .ToList();
        }

        void GenerateExtensionMethods(INamedTypeSymbol symbol, MetadataItem item)
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
                        .Select(reduced => new
                        {
                            Reduced = reduced,
                            TypeParameterNames = symbol.IsGenericType
                                ? symbol.Accept(TypeGenericParameterNameVisitor.Instance)
                                : EmptyListOfString
                        })
                        .Select(x => new
                        {
                            Element = x,
                            MethodGenericParameters = x.Reduced.IsGenericMethod
                                ? x.Reduced.TypeParameters.Select(p => p.Name).ToList()
                                : EmptyListOfString
                        })
                        .Select(x =>
                            AddSpecReference(x.Element.Reduced, x.Element.TypeParameterNames,
                                x.MethodGenericParameters)
                        )
                );
            }

            item.ExtensionMethods = extensions;
        }

        void AddInheritedMembers(
            ITypeSymbol symbol, INamespaceOrTypeSymbol type, Dictionary<string, string> dict,
            IReadOnlyList<string> typeParameterNames
        )
        {
            foreach (var m in type.GetMembers()
                .Where(m => !(m is INamedTypeSymbol))
                .Where(m => FilterVisitor.CanVisitApi(m,
                    symbol.Equals(type) || !symbol.IsSealed || symbol.TypeKind != TypeKind.Struct))
                .Where(IsInheritable))
            {
                var sig = MemberSigRegex.Replace(m.GetSpecId(typeParameterNames), string.Empty);
                dict.TryAdd(sig, type.Equals(symbol) ? null : AddSpecReference(m, typeParameterNames));
            }
        }

        void AddMethodSyntax(
            IMethodSymbol symbol, MetadataItem item, IReadOnlyList<string> typeGenericParameters,
            IReadOnlyList<string> methodGenericParameters
        )
        {
            if (!symbol.ReturnsVoid)
            {
                item.Syntax.Return = CreateApiParameter(symbol, symbol.ReturnType, true);
            }

            item.Syntax.Parameters
                .CreateIfNull()
                .AddRange(symbol.Parameters.Select(p => CreateApiParameter(p, p.Type, false)));

            ApiParameter CreateApiParameter(ISymbol p, ITypeSymbol type, bool isReturn)
            {
                var id    = AddSpecReference(type, typeGenericParameters, methodGenericParameters);
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
                {
                    item.References = new Dictionary<string, ReferenceItem>();
                }

                // only record the id now, the value would be fed at later phase after merge
                item.References[id] = null;
            }
        }

        List<AttributeInfo> GetAttributeInfo(ImmutableArray<AttributeData> attributes)
            => attributes
                .Where(attr => !(attr.AttributeClass is IErrorTypeSymbol) && attr.AttributeConstructor != null)
                .Where(attr => FilterVisitor.CanVisitAttribute(attr.AttributeConstructor))
                .Select(attr => new AttributeInfo
                {
                    Type           = AddSpecReference(attr.AttributeClass),
                    Constructor    = AddSpecReference(attr.AttributeConstructor),
                    Arguments      = GetArguments(attr),
                    NamedArguments = GetNamedArguments(attr)
                })
                .Where(attr => attr.Arguments != null)
                .ToList();

        List<ArgumentInfo> GetArguments(AttributeData attr)
            => attr.ConstructorArguments.Select(GetArgumentInfo).Values().ToList();

        Option<ArgumentInfo> GetArgumentInfo(TypedConstant arg) => GetArgumentInfoBase(arg)
            .Map(x => new ArgumentInfo
            {
                Type  = x.Type,
                Value = x.Value
            });

        List<NamedArgumentInfo> GetNamedArguments(AttributeData attr)
            => attr.NamedArguments.Select(GetNamedArgumentInfo).Values().ToList();

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

            object value = null;
            switch (arg.Value)
            {
                case null:
                    return (AddSpecReference(arg.Type), value).Some();
                case ITypeSymbol type when !FilterVisitor.CanVisitApi(type):
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

        static MemberType GetMemberTypeFromTypeKind(TypeKind typeKind)
            => typeKind switch
            {
                TypeKind.Module    => MemberType.Class,
                TypeKind.Class     => MemberType.Class,
                TypeKind.Enum      => MemberType.Enum,
                TypeKind.Interface => MemberType.Interface,
                TypeKind.Struct    => MemberType.Struct,
                TypeKind.Delegate  => MemberType.Delegate,
                _                  => MemberType.Default
            };
    }
}