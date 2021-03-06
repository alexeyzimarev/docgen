﻿using System.Collections.Generic;
using Microsoft.CodeAnalysis;

namespace DocGen.Metadata.Roslyn
{
    public class ExtractMetadataOptions
    {
        public string? CodeSourceBasePath { get; set; }

        public IReadOnlyDictionary<Compilation, IEnumerable<IMethodSymbol>>? RoslynExtensionMethods { get; set; }
    }
}
