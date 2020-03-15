using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;

namespace Ubiquitous.DocGen.Metadata.Models
{
    public class ApiParameter
    {
        public string Name { get; private set; }

        public string Type { get; private set; }

        public string Description { get; private set; }

        public List<AttributeInfo> Attributes { get; set; }

        public void CopyInheritedData(ApiParameter src)
        {
            if (src == null)
                throw new ArgumentNullException(nameof(src));

            if (Description == null)
                Description = src.Description;
        }

        public static ApiParameter Create(
            ISymbol symbol, MetadataItem item, string id, bool isReturn
        )
            => new ApiParameter
            {
                Name        = isReturn ? null : symbol.Name,
                Type        = id,
                Description = isReturn ? item.CommentModel?.Returns : item.CommentModel?.GetParameter(symbol.Name)
            };

        public static ApiParameter Create(
            ITypeParameterSymbol symbol, MetadataItem item
        )
            => new ApiParameter
            {
                Name        = symbol.Name,
                Description = item.CommentModel?.GetTypeParameter(symbol.Name),
            };
    }
}