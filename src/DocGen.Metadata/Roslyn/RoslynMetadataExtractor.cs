using System.Collections.Generic;
using System.Linq;
using DocGen.Metadata.CodeAnalysis;
using DocGen.Metadata.Models;
using Microsoft.CodeAnalysis;

namespace DocGen.Metadata.Roslyn
{
    public static class RoslynMetadataExtractor
    {
        public static MetadataItem ExtractMetadata(
            this Compilation compilation,
            IAssemblySymbol? assembly = null,
            ExtractMetadataOptions? options = null
        )
        {
            var visitor = new MetadataSymbolVisitor(compilation, options);
            var target  = assembly ?? compilation.Assembly;

            return target.Accept(visitor);
        }

        public static IEnumerable<IMethodSymbol> GetExtensionMethods(this Compilation compilation)
            => !compilation.Assembly.MightContainExtensionMethods
                ? Enumerable.Empty<IMethodSymbol>()
                : GetAllNamespaceMembers(compilation.Assembly)
                    .Distinct()
                    .SelectMany(GetExtensionMethodPerNamespace);

        static IEnumerable<INamespaceSymbol> GetAllNamespaceMembers(IAssemblySymbol assembly)
        {
            var queue = new Queue<INamespaceSymbol>();
            queue.Enqueue(assembly.GlobalNamespace);

            while (queue.Count > 0)
            {
                var space = queue.Dequeue();
                yield return space;

                foreach (var child in space.GetNamespaceMembers())
                {
                    queue.Enqueue(child);
                }
            }
        }

        static IEnumerable<IMethodSymbol> GetExtensionMethodPerNamespace(INamespaceSymbol space)
        {
            var typesWithExtensionMethods = space
                .GetTypeMembers()
                .Where(t => t.MightContainExtensionMethods);

            foreach (var type in typesWithExtensionMethods)
            {
                var members = type.GetMembers();

                foreach (var method in members
                    .Where(member => member.Kind == SymbolKind.Method)
                    .Select(member => (IMethodSymbol) member)
                    .Where(method => method.IsExtensionMethod))
                {
                    yield return method;
                }
            }
        }
    }
}
