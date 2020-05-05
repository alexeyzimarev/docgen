using System.Collections.Generic;
using System.Collections.Immutable;

namespace DocGen.Metadata.Extensions
{
    public static class ListExtensions
    {
        public static List<T> AddWhen<T>(this List<T> self, bool predicate, T element)
        {
            if (predicate) self.Add(element);
            return self;
        }

        public static List<string> AddNotEmpty(this List<string> self, string element)
        {
            if (!string.IsNullOrWhiteSpace(element)) self.Add(element);
            return self;
        }

        public static List<T> AddNotNull<T>(this List<T> self, T element)
        {
            if (element != null) self.Add(element);
            return self;
        }

        public static bool IsNotEmpty<T>(this IList<T> array) => array != null && array.Count > 0;
        
        public static bool IsEmpty<T>(this IList<T> array) => array == null || array.Count == 0;
        
        public static bool IsEmpty<T>(this ImmutableArray<T> array) => array == null || array.Length == 0;
        
        public static bool IsNotEmpty<T>(this ImmutableArray<T> array) => array != null && array.Length > 0;
    }
}