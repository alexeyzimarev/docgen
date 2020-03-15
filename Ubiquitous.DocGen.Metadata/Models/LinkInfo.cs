namespace Ubiquitous.DocGen.Metadata.Models
{
    public class LinkInfo
    {
        public LinkType LinkType { get; set; }

        public string LinkId { get; set; }

        public string CommentId { get; set; }

        public string AltText { get; set; }

        public LinkInfo Clone() => (LinkInfo) MemberwiseClone();
    }

    public enum LinkType
    {
        CRef,
        HRef,
    }
}