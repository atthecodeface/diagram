[header_comment]: # (This files is written in Markdown; it may be compiled to PDF using, for example, pandoc file.md --pdf-engine=pdflatex -o file.pdf; however it should be readable in text form)

Overview
========

The diagram application takes diagram description files in either HML
or XML, applies stylesheets (also in HML or XML), and generates SVG
files.

The purpose of the application is to provide a graphical diagram
layout system, providing table-driven layout (like the WWW box model)
for flow diagrams or hardware diagrams or similar, with connectors
joining the boxes appropriately.

The application reads in the style files to create a stylesheet, reads
in the diagrm description, and applies the stylesheet. The diagram is
the laid out as required; at this point every element in the diagram
has been assigned on-page coordinates. The geometry of each element
can then be calculated, and finally rendered to an SVG file.

![Diagram creation flow](flow.svg "Flow"){ width=80% }

The style files can affect layout size, orientation, spacing, and so
on, as well as colors, borders, arrowheads etc. For example, one can
have a style file that provides a good grayscale rendering with serifed
fonts, and another style file that provides color rendering with
sans-serif fonts, without changing the diagram itself.

Diagram description
===================

Diagrams are described using 'DML' - i.e. diagram markup language
tags, expressed in either HML or XML.

The tags have a common set of potential attributes, used for structure
and for layout.

All measurements in a diagrm description are in millimetres.

The coordinate system of a diagram or a block in a diagram is X
increasing across to the right, and Y increasing down. This matches
the SVG coordinate system.

Many measurements are for boxes, and are four values. The values for
values per-side for four sides are ordered top, right, bottom, then
left. The values for coordinates of a box are ordered top-left, top-right,
bottom-right, bottom-left.

Attributes which provide more than one value should separate them with commas.

Structure attibutes
===================

Each node may have an 'id' attribute and a 'class' attribute, similar
to HTML.

Attribute 'id'
--------------

The id attribute is an uninterpreted string.

Each node should have an id, but in DML it is not required.

Each id should, in theory, be unique in the document
(according to XML standards); however, DML does not require this. Note
though that the output SVG or other XML may have to have unique ids:
if the DML has duplicated ids, then the XML output will have different
ids to the input DML.

Attribute 'class'
-----------------

The class attribute is string that is interpreted as a list of space
separated class names (hence a class name has no whitespace).

The class attribute is used in styling to select sets of nodes to
apply styling to. The stylesheet is capable of expressing hierarchical
selection of nodes; class names need not be tightly coordinated between
different sections of a diagram. For example, the style rule can pick
out all nodes that have a class X that are in the subtree of nodes
that have a class Y.


Layout attributes
=================

There are many layout attributes for nodes

![Padding, border and margin](box.svg "Box"){ width=80% }

Attribute 'padding'
-------------------

The padding attribute provides the amount of padding around the edge
of a box before the border is placed. It is ultimately four float
values, one for each edge.

If the attribute provides one float value
then it is used for all four edges. If the attribute provides two
float values X, Y then the values used are X, Y, X, Y.

Attribute 'border'
-------------------

The border attribute provides the border width within the padding,
outside the margin of content of a box. It is ultimately four float
values, one for each edge.

The border may be drawn if a border_color is provided; the background
to the box may be filled within the border if a fill_color is provided.

If the attribute provides one float value
then it is used for all four edges. If the attribute provides two
float values X, Y then the values used are X, Y, X, Y.

Attribute 'margin'
------------------

The margin attribute provides the amount of margin within the border
to the content of a box. It is ultimately four float
values, one for each edge.

If the attribute provides one float value
then it is used for all four edges. If the attribute provides two
float values X, Y then the values used are X, Y, X, Y.

Attribute 'grid'
-----------------

The grid attribute may be provided for an element. If it is, then the
element will be laid out using a table mechanism, with every sibling
element that also has a grid attribute.

The grid element is 4 integers: c, r, cs, rs. c,r indicates the
top-left column and row of the element in the grid, and cs,rs are the
column and row spans for the element in the grid.

grid should be able to be 2 elements, in which case colspan and
rowspan of 1 are default.

colspan and rowspan should be able to override grid.

Attribute 'width'
-----------------

The width attribute provides one or two float values for the width
that the element should be fitted within.

If the width is empty or is not supplied then the element will
determine the desired with to be fit into. If a single float value is
supplied then this is a minimum width to fit the element in to. If two
float values are provided then the first is used as a minimum width
and the second as a maximum width.

To specify just a maximum value the attribute should be '0 <max>'. To
specify a fixed value the attribute should be '<fixed> <fixed>'.

Attribute 'height'
-----------------

The height attribute provides one or two float values for the height
that the element should be fitted within.

If the height is empty or is not supplied then the element will
determine the desired with to be fit into. If a single float value is
supplied then this is a minimum height to fit the element in to. If two
float values are provided then the first is used as a minimum height
and the second as a maximum height.

To specify just a maximum value the attribute should be '0 <max>'. To
specify a fixed value the attribute should be '<fixed> <fixed>'.

Attribute 'rotation'
--------------------

![box grid (text rotate)](rot_box.svg "Rot box"){ width=80% }

And text

![box grid rotate (text)](rot_box2.svg "Rot box2"){ width=80% }

And text

![text grid rotate](rot_text.svg "Rot text"){ width=80% }

And grid

![text grid rotate](rot_grid.svg "Rot grid"){ width=80% }

Attribute 'scale'
-----------------

Attribute 'translate'
---------------------

Attribute 'transform'
---------------------

Attribute 'magnets_per_side'
----------------------------

Attribute 'border_round'
------------------------

Attribute 'border_color'
------------------------

Attribute 'border_fill'
-----------------------

Attribute 'anchor'
-----------------

Attribute 'expand'
-----------------

Attribute 'place'
-----------------






Layout model
============

Tha layout model for an element is similar to the HTML/W3C consortium
box model. As such element is a rectangular box, which is first
padded around each edge (by a padding amount that can be different for
each edge); then a border is applied of arbitrary width (again,
different for each edge); then a margine is applied of arbitrary width (again,
different for each edge).

Thie yields a rectangular content box. The content of the element and
any of its children elements are placed within this box.

Element geometry
----------------

An element has a desired geometry. This is a rectangular region in the element
coordinate space (with a scale of 1 unit per millimeter, notionally)
with a reference point that is (usually) within that region.

When an element's geometry positioning is finalized during layout it
will be given a new rectangular bounding box and a
position at which its reference point should be placed (again, in
units of millimetres).

The element contents are laid out within this new bounding box, with
any content that is placed being positioned taking into account the
displacement between the element's desired reference point and the
actual placement of the reference point.

Grid layout
-----------

![Grid layout showing spans with even column weighting](grid.svg "Grid"){ width=50% }

Child elements of a parent element may have a grid layout
specification: this is similar to the table model for HTML/W3C. All
sibling elements with a grid specification are gathered together, and
a grid build using the grid specifications and the desired bounding box sizes of those
sibling elements.

Each cell in the grid is then a rectangle within the parent element's
bounding box. A sibling element
; this generates

The content of the element may have a transformation applied to it;
this is effectively an invertible 2-by-2 transformation matrix plus a translation.

Sibling elements are laid out together within its parent element.

. All the
sibling elements that have grid attributes are laid out using their
desired sizing in a grid within the content box of the parent element, depending on the grid settings. Any elements
that are not provided with a grid attribute are assumed to want be
drawn within the whole of the content of the parent element.

An element thus has a desired size, which is resolved from the
element, its attributes, and its content, using the stylesheet. An
element that contains children will have a desired size that is
derived from its content and its childrens' desired sizes and their
placement and/or grid layout.

Each element presents its desired size as a bounding box and a
reference point, in element coordinates, which are millimeters (as far
as the element is concerned).

An element may have a transformation applied to it - 
rotating by an angle around a point (in element coordinates), scaling,
translation of the reference point. This leads to an element having a
desired bounding box, which is an axis-aligned rectangle encompassing
the transformed bounding box and transformed reference point.

An element's post-transformation bounding box may be controlled further by its width and
height atttributes - these can supply minimum and maximum values
(which, if they are the same value, forces a particular width/height
on the element). The reduced bounding box will include the reference
point at the same relative position (in X and Y separately) as in the
post-transformation bounding box.



