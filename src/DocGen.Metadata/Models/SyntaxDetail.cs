using System.Collections.Generic;

namespace DocGen.Metadata.Models
{
    public class SyntaxDetail
    {
        public string? Content { get; set; }

        public List<ApiParameter> Parameters { get; set; }

        public List<ApiParameter> TypeParameters { get; set; }

        public ApiParameter Return { get; set; }
    }
}
