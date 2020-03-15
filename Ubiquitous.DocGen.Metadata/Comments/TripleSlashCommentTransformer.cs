using System.IO;
using System.Net;
using System.Xml;
using System.Xml.Linq;
using System.Xml.XPath;
using System.Xml.Xsl;
using Ubiquitous.DocGen.Metadata.Models;

namespace Ubiquitous.DocGen.Metadata.Comments
{
    public static class TripleSlashCommentTransformer
    {
        static readonly XslCompiledTransform XslTransform;

        static TripleSlashCommentTransformer()
        {
            var assembly     = typeof(TripleSlashCommentTransformer).Assembly;
            var xsltFilePath = $"{assembly.GetName().Name}.Comments.Transform.TripleSlashCommentTransform.xsl";

            using var stream = assembly.GetManifestResourceStream(xsltFilePath);
            using var reader = XmlReader.Create(stream);

            var xsltSettings = new XsltSettings(true, true);
            XslTransform = new XslCompiledTransform();
            XslTransform.Load(reader, xsltSettings, new XmlUrlResolver());
        }

        public static XDocument Transform(string xml, SyntaxLanguage language)
        {
            using var ms     = new MemoryStream();
            using var writer = new XHtmlWriter(new StreamWriter(ms));

            var doc  = XDocument.Parse(xml, LoadOptions.PreserveWhitespace);
            var args = new XsltArgumentList();
            args.AddParam("language", "urn:input-variables", WebUtility.HtmlEncode(language.ToString().ToLower()));
            XslTransform.Transform(doc.CreateNavigator(), args, writer);
            ms.Seek(0, SeekOrigin.Begin);
            return XDocument.Load(ms, LoadOptions.PreserveWhitespace | LoadOptions.SetLineInfo);
        }
    }
}