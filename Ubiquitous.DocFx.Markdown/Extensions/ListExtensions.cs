using System.Collections.Generic;

namespace Ubiquitous.DocFx.Markdown.Extensions
{
    public static class ListExtensions
    {
        public static List<T> AddWhen<T>(this List<T> self, bool predicate, T element)
        {
            if (predicate) self.Add(element);
            return self;
        }

        public static List<T> CreateIfNull<T>(this List<T> self) => self ?? new List<T>();

        public static List<T> AddNotNull<T>(this List<T> self, T element)
        {
            if (element != null) self.Add(element);
            return self;
        }

        public static bool NotEmpty<T>(this IList<T> array) => array.Count > 0;
        
        public static bool Empty<T>(this IList<T> array) => array.Count == 0;
    }
}