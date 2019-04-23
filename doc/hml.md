[header_comment]: # (This files is written in Markdown; it may be compiled to PDF using, for example, pandoc file.md --pdf-engine=pdflatex -o file.pdf; however it should be readable in text form)

HML
===

HML is a human markup language, designed for humans to use rather than
XML. It is as expressive as XML, but replaces the tag descriptors.

Tags and attributes
-------------------

An XML tag of <fred attr='value'/> is handled in HML with

  #fred attr=value

If value has whitespace, then quotes should be used (either ' or ")

  #fred attr='value with spaces'

Child nodes
-----------

An XML tag with child nodes such as

    <fred><jim attr='value'/></fred>

is handled in HML with '#' indentation:

    #fred ##jim attr='value'

There is no closing tag in HML - it is implied by the level of '#'
indentation. Hence the following XML:

    <fred><jim><bob/></jim></fred>

would be in HML:

    #fred ##jim ###bob

A tag fred enclosing two tags jim and bob in XML would be:

    <fred><jim/><bob/></fred>

and in HML would be:

    #fred ##jim ##bob

Hash indentation and braces
---------------------------

Clearly the depth of '#' indentation can get large and unwieldy. To
help with this one can use enclosing tags like XML with a first and last tag
matching, the first with '{' and the last with '}' appended.
Hence the XML

    <fred><jim><bob id='bob1'/><bob id='bob2'><bobs_child
     id='child_of_bob2'/></bob><bob id='bob3'/></jim></fred>

would be:

    #fred ##jim{
      #bob id=bob1
      #bob id=bob2 ##bobs_child id=child_of_bob2
      #bob id=bob3
    ##jim}

Here tag fred has a single child jim; jim has three children with tag
bob, one of which has a child.

Whitespace
-----------

HML expects whitespace between tags and attributes and cdata
quotations. Whitespaces is a tab, a space, or newline, etc. There is
no requirement, then, for HML to have tags on separate lines, or to
have them on the same line, and so on.

HML should be written for readability - generally one would use one
tag per line if it has attributes, with whitespace indentation to
match the depth of the actual tag. Where braced tags are used the
opening and closing tags should have the same indentation.

CDATA
-----

HML supports 'CDATA', as does XML. In XML the CDATA is
anything not within a tag, and that includes whitespace - making XML
quite whitespace-sensitive. HML does it differently - CDATA must be
quoted. In XML:

    <fred id="banana">cdata></fred>

and in HML:

    #fred id=banana "cdata"

The quotation marks for the cdata are either single quotes or double
quotes. Furthermore, as in Python, the quotation marks can be tripled,
in which case the content can include newline characters - a single
quoted item cannot include newlines. This helps with debugging your
HML files...

Multiple quoted cdata elements can be included; they may be handled by
the application concatenation - this is the normal approach. Hence

    #fred id=banana "cdata1" "cdata2" "cdata3"

is equivalent to

    <fred id='banana'>cdata1cdata2cdata3</fred>

In XML one may have:

    <preformatted>This is my text:
    it has newlines in it
    and I want them
    </preformatted>

In HML one could do either:

    #prefomatted """This is my text:
    it has newlines in it
    and I want them
    """

or

    #prefomatted "This is my text:\nit has newlines in it\nand I want them"

Doctype, namespaces, etc
========================

An XML file expects a header with an encoding and so on. HML files are
all UTF-8 encoded (unless explicitly changed by the enclosing
library), and there is single HML format (no version number required).

XML also provides the DocType mechanism to provide the DTD for the XML
file; HML relies on the application to provde the decoding, and it is
not expected that HML files are munged with (for example) XSLT or
analyzed for correctness with arbitrary DTD specification.

An HML file will also usually not include the toplevel document tag -
this is again assumed by the application.

More complex examples
=====================

An SVG file in XML is:

    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
    <svg version='1.2' xmlns='http://www.w3.org/2000/svg' viewBox='0 0
    29 117.1' preserveAspectRatio='xMidYMid' fill-rule='evenodd' stroke-width='1.' stroke-linejoin='round' >
    <defs>
    <marker id='arr' viewBox='0 0 15 10' orient='auto' refX='10' refY='5' markerWidth='4' markerHeight='3' >
    <path d=' M 0 10 L 10 5 L 0 0 L 5 5z' fill='black' />
    <path d=' M 5 10 L 15 5 L 5 0 L 10 5z' fill='black' />
    </marker>
    </defs>
    <g transform=' ' id='toplevel' >
    <g transform=' ' id='group' >
    <g transform=' ' id='fetch' >
    <path stroke='none' fill='rgb(255,0,0)' stroke-width='0.5' d='M
      5.75 0.75 L 23.25 0.75 Q 28.25 0.75 28.25 5.75 L 28.25 6.15 Q
      28.25 11.15 23.25 11.15 L 5.75 11.15 Q 0.75 11.15 0.75 6.15 L
      0.75 5.75 Q 0.75 0.75 5.75 0.75 Z' />
    <text x='14.5' y='7.9' font-size='3' text-anchor='middle' fill='black' >
    Fetch</text>
    <path stroke='rgb(26,26,26)' fill='none' stroke-width='0.5' d='M
      5.75 0.75 L 23.25 0.75 Q 28.25 0.75 28.25 5.75 L 28.25 6.15 Q
      28.25 11.15 23.25 11.15 L 5.75 11.15 Q 0.75 11.15 0.75 6.15 L
      0.75 5.75 Q 0.75 0.75 5.75 0.75 Z' />
    </g>
    <g transform=' ' id='decode' >
    <path stroke='none' fill='rgb(255,0,0)' stroke-width='0.5' d='M
      5.75 16.65 L 23.25 16.65 Q 28.25 16.65 28.25 21.65 L 28.25
      22.05 Q 28.25 27.05 23.25 27.05 L 5.75 27.05 Q 0.75 27.05
      0.75 22.05 L 0.75 21.65 Q 0.75 16.65 5.75 16.65 Z' />
    <text x='14.5' y='23.8' font-size='3' text-anchor='middle' fill='black' >
    Decode</text>
    <path stroke='rgb(26,26,26)' fill='none' stroke-width='0.5' d='M
      5.75 16.65 L 23.25 16.65 Q 28.25 16.65 28.25 21.65 L 28.25
      22.05 Q 28.25 27.05 23.25 27.05 L 5.75 27.05 Q 0.75 27.05
      0.75 22.05 L 0.75 21.65 Q 0.75 16.65 5.75 16.65 Z' />
    </g>
    <g transform=' ' id='f2darr' >
    <path marker-start='url(#arr)' marker-end='url(#arr)' fill='none'
    stroke='Rgb(0,0,0)' stroke-width='0.5' d=' M 14.5 11.9 L 14.5 15.9' />
    </g>
    </g>
    </g>
    </svg>

And an HML version of the content is:

    #defs
     ##marker id=arr viewBox='0 0 15 10' orient=auto refX=10 refY=5
              markerWidth=4 markerHeight=3
     ###path d=' M 0 10 L 10 5 L 0 0 L 5 5z' fill=black
     ###path d=' M 5 10 L 15 5 L 5 0 L 10 5z' fill=black
    #g transform='' id=toplevel
     ##g transform='' id=group
      ##g{ transform=' ' id=fetch
       #path stroke=none fill=rgb(255,0,0) stroke-width=0.5 d='''M
            5.75 0.75 L 23.25 0.75 Q 28.25 0.75 28.25 5.75 L 28.25
            6.15 Q 28.25 11.15 23.25 11.15 L 5.75 11.15 Q 0.75 11.15
            0.75 6.15 L 0.75 5.75 Q 0.75 0.75 5.75 0.75 Z'''
       #text x=14.5 y=7.9 font-size=3 text-anchor=middle fill=black "Fetch"
       #path stroke=rgb(26,26,26) fill=none stroke-width=0.5 d='''M
            5.75 0.75 L 23.25 0.75 Q 28.25 0.75 28.25 5.75 L 28.25
            6.15 Q 28.25 11.15 23.25 11.15 L 5.75 11.15 Q 0.75 11.15
            0.75 6.15 L 0.75 5.75 Q 0.75 0.75 5.75 0.75 Z'''
      ##g}
      ##g{ transform=' ' id=decode
       #path stroke=none fill=rgb(255,0,0) stroke-width=0.5 d='''M
            5.75 16.65 L 23.25 16.65 Q 28.25 16.65 28.25 21.65 L 28.25
            22.05 Q 28.25 27.05 23.25 27.05 L 5.75 27.05 Q 0.75 27.05
            0.75 22.05 L 0.75 21.65 Q 0.75 16.65 5.75 16.65 Z'''
       #text x=14.5 y=7.9 font-size=3 text-anchor=middle fill=black "Fetch"
       #path stroke=rgb(26,26,26) fill=none stroke-width=0.5 d='''M
            5.75 16.65 L 23.25 16.65 Q 28.25 16.65 28.25 21.65 L 28.25
            22.05 Q 28.25 27.05 23.25 27.05 L 5.75 27.05 Q 0.75 27.05
            0.75 22.05 L 0.75 21.65 Q 0.75 16.65 5.75 16.65 Z'''
      ##g}
      ##g transform=' ' id='f2darr'
       ###path marker-start='url(#arr)' marker-end='url(#arr)' fill='none'
               stroke='Rgb(0,0,0)' stroke-width='0.5'
               d=' M 14.5 11.9 L 14.5 15.9'

