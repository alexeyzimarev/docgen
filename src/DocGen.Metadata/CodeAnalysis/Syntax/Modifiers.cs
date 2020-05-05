using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using DocGen.Metadata.Extensions;

namespace DocGen.Metadata.CodeAnalysis.Syntax
{
    public static class Modifiers
    {
        public static List<string> GeneratePropertyModifiers(this IPropertySymbol symbol)
        {
            var modifiers          = symbol.GetMemberModifiersAsStrings();
            var propertyVisibility = symbol.GetVisibility();

            AddGetSet(symbol.GetMethod, "get");
            AddGetSet(symbol.SetMethod, "set");

            return modifiers;

            void AddGetSet(ISymbol? methodSymbol, string method)
            {
                if (methodSymbol == null) return;

                var methodVisibility = methodSymbol.GetVisibility();
                if (propertyVisibility != null && methodVisibility == null) return;

                modifiers!.Add(
                    methodVisibility != propertyVisibility ? $"{methodVisibility} {method}" : method
                );
            }
        }

        public static List<string> GenerateNamedTypeModifiers(this INamedTypeSymbol symbol)
        {
            var modifiers = new List<string>()
                .AddNotEmpty(symbol.GetVisibility());

            if (symbol.TypeKind == TypeKind.Class)
            {
                if (symbol.IsStatic)
                    modifiers.Add("static");
                else if (symbol.IsAbstract)
                    modifiers.Add("abstract");
                else if (symbol.IsSealed) modifiers.Add("sealed");
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
            modifiers!.AddWhen(typeKind != null, typeKind);

            return modifiers;
        }

        internal static List<string> GetMemberModifiersAsStrings(this ISymbol symbol)
        {
            if (symbol.ContainingType.TypeKind == TypeKind.Interface) return new List<string>();

            return new List<string>()
                .AddNotEmpty(symbol.GetVisibility())
                .AddWhen(symbol.IsStatic, "static")
                .AddWhen(symbol.IsAbstract, "abstract")
                .AddWhen(symbol.IsOverride, "override")
                .AddWhen(symbol.IsVirtual && !symbol.IsSealed, "virtual")
                .AddWhen(symbol.IsSealed && !symbol.IsVirtual, "sealed");
        }

        public static List<string> GetFieldModifiers(this IFieldSymbol symbol)
            => new List<string>()
                .AddNotEmpty(symbol.GetVisibility())
                .AddWhen(symbol.IsConst, "const")
                .AddWhen(symbol.IsStatic && !symbol.IsConst, "static")
                .AddWhen(symbol.IsReadOnly, "readonly")
                .AddWhen(symbol.IsVolatile, "volatile");

        static string GetVisibility(this ISymbol symbol)
            => symbol.DeclaredAccessibility switch
            {
                Accessibility.Protected           => "protected",
                Accessibility.ProtectedOrInternal => "protected",
                Accessibility.Public              => "public",
                _                                 => string.Empty
            };
    }
}
