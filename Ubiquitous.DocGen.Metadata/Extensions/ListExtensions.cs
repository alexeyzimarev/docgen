using System.Collections.Generic;

namespace Ubiquitous.DocGen.Metadata.Extensions
{
    public static class ListExtensions
    {
        public static List<T> AddWhen<T>(this List<T> self, bool predicate, T element)
        {
            if (predicate) self.Add(element);
            return self;
        }

        public static List<T> AddNotNull<T>(this List<T> self, T element)
        {
            if (element != null) self.Add(element);
            return self;
        }

        public static bool IsNotEmpty<T>(this IList<T> array) => array != null && array.Count > 0;
        
        public static bool IsEmpty<T>(this IList<T> array) => array == null || array.Count == 0;
    }
}