using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml;
using System.Xml.Linq;
using System.Xml.XPath;
using Microsoft.Extensions.Logging;
using Ubiquitous.DocGen.Metadata.Models;

namespace Ubiquitous.DocGen.Metadata.Comments
{
    public class TripleSlashCommentModel
    {
        static readonly ILogger<TripleSlashCommentModel> Logger = Logging.GetLogger<TripleSlashCommentModel>();

        const string IdSelector = @"((?![0-9])[\w_])+[\w\(\)\.\{\}\[\]\|\*\^~#@!`,_<>:]*";

        static readonly Regex CommentIdRegex =
            new Regex(@"^(?<type>N|T|M|P|F|E|Overload):(?<id>" + IdSelector + ")$", RegexOptions.Compiled);

        static readonly Regex LineBreakRegex = new Regex(@"\r?\n", RegexOptions.Compiled);

        static readonly Regex CodeElementRegex =
            new Regex(@"<code[^>]*>([\s\S]*?)</code>", RegexOptions.Compiled);

        static readonly Regex RegionRegex       = new Regex(@"^\s*#region\s*(.*)$");
        static readonly Regex XmlRegionRegex    = new Regex(@"^\s*<!--\s*<([^/\s].*)>\s*-->$");
        static readonly Regex EndRegionRegex    = new Regex(@"^\s*#endregion\s*.*$");
        static readonly Regex XmlEndRegionRegex = new Regex(@"^\s*<!--\s*</(.*)>\s*-->$");

        readonly TripleSlashCommentParserContext _context;
        readonly XPathNavigator                  _nav;

        public string Summary { get; private set; }

        public string Remarks { get; private set; }

        public string Returns { get; private set; }

        public List<ExceptionInfo> Exceptions { get; private set; }

        public List<LinkInfo> Sees { get; private set; }

        public List<LinkInfo> SeeAlsos { get; private set; }

        public List<string> Examples { get; private set; }

        public Dictionary<string, string> Parameters { get; private set; }

        public Dictionary<string, string> TypeParameters { get; private set; }

        public bool IsInheritDoc { get; }

        TripleSlashCommentModel(string xml, SyntaxLanguage language, TripleSlashCommentParserContext context)
        {
            // Transform triple slash comment
            var doc = TripleSlashCommentTransformer.Transform(xml, language);

            _context = context;

            ResolveSeeCref(doc);
            ResolveSeeAlsoCref(doc);
            ResolveExceptionCref(doc);

            ResolveCodeSource(doc);
            _nav    = doc.CreateNavigator();
            Summary = GetSummary();
            Remarks = GetRemarks();
            Returns = GetReturns();

            Exceptions     = GetExceptions();
            Sees           = GetSees();
            SeeAlsos       = GetSeeAlsos();
            Examples       = GetExamples();
            Parameters     = GetParameters();
            TypeParameters = GetTypeParameters();
            IsInheritDoc   = GetIsInheritDoc();
        }

        public static TripleSlashCommentModel CreateModel(string xml, TripleSlashCommentParserContext context)
        {
            if (context == null)
            {
                throw new ArgumentNullException(nameof(context));
            }

            if (string.IsNullOrEmpty(xml)) return null;

            // Quick turnaround for badly formed XML comment
            if (xml.StartsWith("<!-- Badly formed XML comment ignored for member ", StringComparison.Ordinal))
            {
                Logger.LogWarning($"Invalid triple slash comment is ignored: {xml}");
                return null;
            }

            try
            {
                var model = new TripleSlashCommentModel(xml, SyntaxLanguage.CSharp, context);
                return model;
            }
            catch (XmlException)
            {
                return null;
            }
        }

        public void CopyInheritedData(TripleSlashCommentModel src)
        {
            if (src == null)
            {
                throw new ArgumentNullException(nameof(src));
            }

            Summary ??= src.Summary;
            Remarks ??= src.Remarks;
            Returns ??= src.Returns;

            if (Exceptions == null && src.Exceptions != null)
            {
                Exceptions = src.Exceptions.Select(e => e.Clone()).ToList();
            }

            if (Sees == null && src.Sees != null)
            {
                Sees = src.Sees.Select(s => s.Clone()).ToList();
            }

            if (SeeAlsos == null && src.SeeAlsos != null)
            {
                SeeAlsos = src.SeeAlsos.Select(s => s.Clone()).ToList();
            }

            if (Examples == null && src.Examples != null)
            {
                Examples = new List<string>(src.Examples);
            }

            if (Parameters == null && src.Parameters != null)
            {
                Parameters = new Dictionary<string, string>(src.Parameters);
            }

            if (TypeParameters == null && src.TypeParameters != null)
            {
                TypeParameters = new Dictionary<string, string>(src.TypeParameters);
            }
        }

        public string GetParameter(string name) => string.IsNullOrEmpty(name) ? null : GetValue(name, Parameters);

        public string GetTypeParameter(string name) => string.IsNullOrEmpty(name) ? null : GetValue(name, TypeParameters);

        static string GetValue(string name, Dictionary<string, string> dictionary)
            => dictionary == null
                ? null
                : dictionary.TryGetValue(name, out string description)
                    ? description
                    : null;

        string GetSummary() => GetSingleNodeValue(_nav, "/member/summary");

        string GetRemarks() => GetSingleNodeValue(_nav, "/member/remarks");

        string GetReturns() => GetSingleNodeValue(_nav, "/member/returns");

        List<ExceptionInfo> GetExceptions()
        {
            var result = GetMulitpleCrefInfo(_nav, "/member/exception").ToList();
            return result.Count == 0 ? null : result;
        }

        List<LinkInfo> GetSees() => GetMultipleLinkInfo(_nav, "/member/see").ToList();

        List<LinkInfo> GetSeeAlsos() => GetMultipleLinkInfo(_nav, "/member/seealso").ToList();

        List<string> GetExamples() => GetMultipleExampleNodes(_nav, "/member/example").ToList();

        bool GetIsInheritDoc()
        {
            var node = _nav.SelectSingleNode("/member/inheritdoc");

            if (node == null)
            {
                return false;
            }

            if (node.HasAttributes)
            {
                Logger.LogWarning(
                    "Attributes on <inheritdoc /> elements are not supported; inheritdoc element will be ignored."
                );
                return false;
            }

            return true;
        }

        void ResolveCodeSource(XNode doc)
        {
            foreach (var node in doc.XPathSelectElements("//code"))
            {
                var source = node.Attribute("source");

                Logger.LogInformation("Trying to get source from {node} {source}", node, source);

                if (source == null || string.IsNullOrEmpty(source.Value))
                {
                    continue;
                }

                if (_context.Source == null || string.IsNullOrEmpty(_context.Source.Path))
                {
                    Logger.LogWarning($"Unable to get source file path for {node}");
                    return;
                }

                var region = node.Attribute("region");

                var path = source.Value;

                if (!Path.IsPathRooted(path))
                {
                    var basePath = !string.IsNullOrEmpty(_context.CodeSourceBasePath)
                        ? _context.CodeSourceBasePath
                        : Path.GetDirectoryName(_context.Source.Path);

                    path = Path.Combine(basePath, path);
                }

                ResolveCodeSource(node, path, region?.Value);
            }
        }

        static void ResolveCodeSource(XElement element, string source, string region)
        {
            if (!File.Exists(source))
            {
                Logger.LogWarning($"Source file '{source}' not found.");
                return;
            }

            var (regionRegex, endRegionRegex) = GetRegionRegex(source);

            var builder     = new StringBuilder();
            var regionCount = 0;

            foreach (var line in File.ReadLines(source))
            {
                if (!string.IsNullOrEmpty(region))
                {
                    var match = regionRegex.Match(line);

                    if (match.Success)
                    {
                        var name = match.Groups[1].Value.Trim();

                        if (name == region)
                        {
                            ++regionCount;
                            continue;
                        }

                        if (regionCount > 0)
                        {
                            ++regionCount;
                        }
                    }
                    else if (regionCount > 0 && endRegionRegex.IsMatch(line))
                    {
                        --regionCount;

                        if (regionCount == 0)
                        {
                            break;
                        }
                    }

                    if (regionCount > 0)
                    {
                        builder.AppendLine(line);
                    }
                }
                else
                {
                    builder.AppendLine(line);
                }
            }

            element.SetValue(builder.ToString());

            Logger.LogInformation("Resolved source for {path} and {region}", source, region);
        }

        static Dictionary<string, string> GetListContent(XPathNavigator navigator, string xpath, string contentType)
        {
            var iterator = navigator.Select(xpath);
            var result   = new Dictionary<string, string>();

            if (iterator == null)
            {
                return result;
            }

            foreach (XPathNavigator nav in iterator)
            {
                var name        = nav.GetAttribute("name", string.Empty);
                var description = GetXmlValue(nav);
                if (string.IsNullOrEmpty(name)) continue;

                if (result.ContainsKey(name))
                {
                    Logger.LogWarning($"Duplicate {contentType} '{name}' found in comments, the latter one is ignored.");
                }
                else
                {
                    result.Add(name, description);
                }
            }

            return result;
        }

        Dictionary<string, string> GetParameters() => GetListContent(_nav, "/member/param", "parameter");

        static (Regex, Regex) GetRegionRegex(string source)
        {
            var ext = Path.GetExtension(source);

            return ext?.ToUpper() switch
            {
                ".XML"    => (XmlRegionRegex, XmlEndRegionRegex),
                ".XAML"   => (XmlRegionRegex, XmlEndRegionRegex),
                ".HTML"   => (XmlRegionRegex, XmlEndRegionRegex),
                ".CSHTML" => (XmlRegionRegex, XmlEndRegionRegex),
                ".VBHTML" => (XmlRegionRegex, XmlEndRegionRegex),
                _         => (RegionRegex, EndRegionRegex)
            };
        }

        Dictionary<string, string> GetTypeParameters() => GetListContent(_nav, "/member/typeparam", "type parameter");

        void ResolveSeeAlsoCref(XNode node) => ResolveCrefLink(node, "//seealso[@cref]");

        void ResolveSeeCref(XNode node) => ResolveCrefLink(node, "//see[@cref]");

        void ResolveExceptionCref(XNode node) => ResolveCrefLink(node, "//exception[@cref]");

        void ResolveCrefLink(XNode node, string nodeSelector)
        {
            if (node == null || string.IsNullOrEmpty(nodeSelector))
            {
                return;
            }

            var nodes = node.XPathSelectElements(nodeSelector + "[@cref]").ToList();

            foreach (var item in nodes)
            {
                var cref = item.Attribute("cref")?.Value;
                if (cref == null) continue;

                var success = false;

                // Strict check is needed as value could be an invalid href,
                // e.g. !:Dictionary&lt;TKey, string&gt; when user manually changed the intellisensed generic type
                var match = CommentIdRegex.Match(cref);

                if (match.Success)
                {
                    var id   = match.Groups["id"].Value;
                    var type = match.Groups["type"].Value;

                    if (type == "Overload")
                    {
                        id += '*';
                    }

                    // When see and seealso are top level nodes in triple slash comments, do not convert it into xref node
                    if (item.Parent?.Parent != null)
                    {
                        var replacement = XElement.Parse($"~~~{id}~~~");
                        item.ReplaceWith(replacement);
                    }

                    _context?.AddReferenceDelegate?.Invoke(id, cref);
                    success = true;
                }

                if (success) continue;

                var detailedInfo = new StringBuilder();

                if (_context?.Source != null)
                {
                    if (!string.IsNullOrEmpty(_context.Source.Name))
                    {
                        detailedInfo.Append(" for ");
                        detailedInfo.Append(_context.Source.Name);
                    }

                    if (!string.IsNullOrEmpty(_context.Source.Path))
                    {
                        detailedInfo.Append(" defined in ");
                        detailedInfo.Append(_context.Source.Path);
                        detailedInfo.Append(" Line ");
                        detailedInfo.Append(_context.Source.StartLine);
                    }
                }

                Logger.LogWarning(
                    $"Invalid cref value \"{cref}\" found in triple-slash-comments{detailedInfo}, ignored."
                );
            }
        }

        static IEnumerable<string> GetMultipleExampleNodes(XPathNavigator navigator, string selector)
        {
            var iterator = navigator.Select(selector);

            if (iterator == null)
            {
                yield break;
            }

            foreach (XPathNavigator nav in iterator)
            {
                var description = GetXmlValue(nav);
                yield return description;
            }
        }

        static IEnumerable<ExceptionInfo> GetMulitpleCrefInfo(XPathNavigator navigator, string selector)
        {
            var iterator = navigator.Clone().Select(selector);

            if (iterator == null)
            {
                yield break;
            }

            foreach (XPathNavigator nav in iterator)
            {
                var description = GetXmlValue(nav);

                if (string.IsNullOrEmpty(description))
                {
                    description = null;
                }

                var commentId = nav.GetAttribute("cref", string.Empty);
                var refId     = nav.GetAttribute("refId", string.Empty);

                if (!string.IsNullOrEmpty(refId))
                {
                    yield return new ExceptionInfo
                    {
                        Description = description,
                        Type        = refId,
                        CommentId   = commentId
                    };
                }
                else if (!string.IsNullOrEmpty(commentId))
                {
                    // Check if exception type is valid and trim prefix
                    var match = CommentIdRegex.Match(commentId);
                    if (!match.Success) continue;

                    var id   = match.Groups["id"].Value;
                    var type = match.Groups["type"].Value;

                    if (type == "T")
                    {
                        yield return new ExceptionInfo
                        {
                            Description = description,
                            Type        = id,
                            CommentId   = commentId
                        };
                    }
                }
            }
        }

        static IEnumerable<LinkInfo> GetMultipleLinkInfo(XPathNavigator navigator, string selector)
        {
            var iterator = navigator.Clone().Select(selector);

            if (iterator == null)
            {
                yield break;
            }

            foreach (XPathNavigator nav in iterator)
            {
                var altText = GetXmlValue(nav);

                if (string.IsNullOrEmpty(altText)) altText = null;

                var commentId = nav.GetAttribute("cref", string.Empty);
                var url       = nav.GetAttribute("href", string.Empty);
                var refId     = nav.GetAttribute("refId", string.Empty);

                if (!string.IsNullOrEmpty(refId))
                {
                    yield return new LinkInfo
                    {
                        AltText   = altText,
                        LinkId    = refId,
                        CommentId = commentId,
                        LinkType  = LinkType.CRef
                    };
                }
                else if (!string.IsNullOrEmpty(commentId))
                {
                    // Check if cref type is valid and trim prefix
                    var match = CommentIdRegex.Match(commentId);
                    if (!match.Success) continue;

                    var id   = match.Groups["id"].Value;
                    var type = match.Groups["type"].Value;

                    if (type == "Overload")
                    {
                        id += '*';
                    }

                    yield return new LinkInfo
                    {
                        AltText   = altText,
                        LinkId    = id,
                        CommentId = commentId,
                        LinkType  = LinkType.CRef
                    };
                }
                else if (!string.IsNullOrEmpty(url))
                {
                    yield return new LinkInfo
                    {
                        AltText  = altText ?? url,
                        LinkId   = url,
                        LinkType = LinkType.HRef
                    };
                }
            }
        }

        static string GetSingleNodeValue(XPathNavigator nav, string selector)
        {
            var node = nav.Clone().SelectSingleNode(selector);
            return node == null ? null : GetXmlValue(node);
        }

        static string GetXmlValue(XPathNavigator node)
        {
            var lineInfo = node as IXmlLineInfo;
            var column   = lineInfo?.HasLineInfo() == true ? lineInfo.LinePosition - 2 : 0;

            return NormalizeXml(RemoveLeadingSpaces(GetInnerXml(node)), column);
        }

        static string RemoveLeadingSpaces(string xml)
        {
            var lines = LineBreakRegex.Split(xml);

            var preIndex = 0;

            var leadingSpaces = lines
                .Where(line => !string.IsNullOrWhiteSpace(line))
                .Select(line => line.TakeWhile(char.IsWhiteSpace).Count())
                .ToList();

            if (leadingSpaces.Any())
            {
                preIndex = leadingSpaces.Min();
            }

            if (preIndex == 0)
            {
                return xml;
            }

            var normalized = lines
                .Select(line => string.IsNullOrWhiteSpace(line) ? string.Empty : line.Substring(preIndex))
                .ToList();

            return string.Join("\n", normalized);
        }

        static string NormalizeXml(string xml, int parentIndex)
        {
            var lines      = LineBreakRegex.Split(xml);
            var normalized = new List<string>();

            foreach (var line in lines)
            {
                if (string.IsNullOrWhiteSpace(line))
                {
                    normalized.Add(string.Empty);
                }
                else
                {
                    var index = line.TakeWhile(char.IsWhiteSpace).Count();

                    if (line[index] == '<')
                    {
                        parentIndex = index;
                    }

                    normalized.Add(line.Substring(Math.Min(parentIndex, index)));
                }
            }

            // trim newline character for code element
            return CodeElementRegex.Replace(
                string.Join("\n", normalized),
                m =>
                {
                    var group = m.Groups[1];

                    return group.Length == 0
                        ? m.Value
                        : m.Value.Replace(group.ToString(), group.ToString().Trim('\n'));
                }
            );
        }

        static string GetInnerXml(XPathNavigator node)
        {
            using var sw = new StringWriter(CultureInfo.InvariantCulture);

            using (var tw = new XmlWriterWithGtDecoded(sw))
            {
                if (!node.MoveToFirstChild()) return sw.ToString();

                do
                {
                    tw.WriteNode(node, true);
                } while (node.MoveToNext());

                node.MoveToParent();
            }

            return sw.ToString();
        }

        sealed class XmlWriterWithGtDecoded : XmlTextWriter
        {
            public XmlWriterWithGtDecoded(TextWriter tw) : base(tw) { }

            public override void WriteString(string text)
            {
                var encoded = text.Replace("&", "&amp;")
                    .Replace("<", "&lt;")
                    .Replace("'", "&apos;")
                    .Replace("\"", "&quot;");
                WriteRaw(encoded);
            }
        }
    }
}