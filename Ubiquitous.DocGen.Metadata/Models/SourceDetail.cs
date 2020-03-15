namespace Ubiquitous.DocGen.Metadata.Models
{
    public class SourceDetail
    {
        public string BasePath { get; set; }

        public string Name { get; set; }

        /// <summary>
        /// The url path for current source, should be resolved at some late stage
        /// </summary>
        public string Href { get; set; }

        /// <summary>
        /// The local path for current source, should be resolved to be relative path at some late stage
        /// </summary>
        public string Path { get; set; }

        public int StartLine { get; set; }

        public int EndLine { get; set; }

        public string Content { get; set; }

        /// <summary>
        /// The external path for current source if it is not locally available
        /// </summary>
        public bool IsExternalPath { get; set; }
    }
}
