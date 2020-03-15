﻿using System.Collections.Generic;
using System.IO;
using System.Xml;

namespace Ubiquitous.DocGen.Metadata.Comments
{
    public class XHtmlWriter : XmlWriter
    {
        static HashSet<string> voidElements;
        string                 _currentElement;
        readonly XmlWriter     _writer;

        public override WriteState WriteState { get; }

        public XHtmlWriter(TextWriter writer)
        {
            _writer = Create(writer);
            // void element (ref: http://www.w3.org/TR/html-markup/syntax.html)
            voidElements = new HashSet<string>
            {
                "area", "base", "br", "col", "command", "embed", "hr", "img", "input", "keygen", "link", "meta",
                "param", "source", "track", "wbr"
            };
        }

        public override void WriteEndElement()
        {
            if (voidElements.Contains(_currentElement))
            {
                _writer.WriteEndElement();
            }
            else
            {
                _writer.WriteFullEndElement();
            }
        }

        public override void WriteFullEndElement() => _writer.WriteFullEndElement();

        public override void WriteStartAttribute(string prefix, string localName, string ns)
            => _writer.WriteStartAttribute(prefix, localName, ns);

        public override void WriteEndAttribute() => _writer.WriteEndAttribute();

        public override void WriteCData(string text) => _writer.WriteCData(text);

        public override void WriteComment(string text) => _writer.WriteComment(text);

        public override void WriteProcessingInstruction(string name, string text)
            => _writer.WriteProcessingInstruction(name, text);

        public override void WriteEntityRef(string name) => _writer.WriteEntityRef(name);

        public override void WriteCharEntity(char ch) => _writer.WriteCharEntity(ch);

        public override void WriteWhitespace(string ws) => _writer.WriteWhitespace(ws);

        public override void WriteString(string text) => _writer.WriteString(text);

        public override void WriteSurrogateCharEntity(char lowChar, char highChar)
            => _writer.WriteSurrogateCharEntity(lowChar, highChar);

        public override void WriteChars(char[] buffer, int index, int count)
            => _writer.WriteChars(buffer, index, count);

        public override void WriteRaw(char[] buffer, int index, int count) => _writer.WriteRaw(buffer, index, count);

        public override void WriteRaw(string data) => _writer.WriteRaw(data);

        public override void WriteBase64(byte[] buffer, int index, int count)
            => _writer.WriteBase64(buffer, index, count);

        public override void Flush() => _writer.Flush();

        public override string LookupPrefix(string ns) => _writer.LookupPrefix(ns);

        public override void WriteStartDocument() => _writer.WriteStartDocument();

        public override void WriteStartDocument(bool standalone) => _writer.WriteStartDocument(standalone);

        public override void WriteEndDocument() => _writer.WriteEndDocument();

        public override void WriteDocType(string name, string pubid, string sysid, string subset)
            => _writer.WriteDocType(name, pubid, sysid, subset);

        public override void WriteStartElement(string prefix, string localName, string ns)
        {
            _writer.WriteStartElement(prefix, localName, ns);
            _currentElement = localName;
        }
    }
}