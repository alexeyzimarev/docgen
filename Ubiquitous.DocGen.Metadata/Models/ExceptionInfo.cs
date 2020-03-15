namespace Ubiquitous.DocGen.Metadata.Models
{
    public class ExceptionInfo
    {
        public string Type { get; set; }

        public string CommentId { get; set; }

        public string Description { get; set; }

        public ExceptionInfo Clone() => (ExceptionInfo)MemberwiseClone();
    }
}
