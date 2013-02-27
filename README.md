This package includes some tools to visualize the LLVM IR.  Current
visualizations include static call graphs, CFGs, CDGs, dominator
trees, and some simple escape graphs.  Output formats include most
graphviz-supported formats, along with an HTML-based format.

# Usage

Options for the visualizer:

    -o --output=[FILE or DIR]  The destination of a file output
    -t --type=[GRAPHTYPE]      The graph requested.  One of Cfg, Cdg, Cg,
                               Domtree, Postdomtree
    -f --format=GVOUT          The type of output to produce: Gtk, Xlib, Html, XDot,
                               Eps, Jpeg, Pdf, Png, Ps, Ps2, Svg.  Default: Gtk
    -? --help                  Display help message


For all graph types except the call graph, the output specifies a
*directory*.  The directory will contain one output file for each
function in the input IR module.  For the static call graph, the
output is a single file.  If the format is 'Html', the output is
always a directory.

The Html format is special.  It produces an SVG embedded in an HTML
page.  The SVG can be navigated (via panning and zooming) using an
openstreetmap-style interface (it uses the OpenLayers library).

Input files can be LLVM bitcode files, LLVM assembly files, or C/C++ source
files.

# Dependencies

This package depends on a few other Haskell libraries that are not yet
on Hackage:

 * hbgl
 * ifscs
 * llvm-base-types
 * llvm-data-interop
 * llvm-analysis

Additionally, it requires that you have the LLVM shared libraries (versions
3.0-3.2) and tools installed on your system (`llvm-config` must be in your
`PATH`).

# Installation
Installation would look something like:

```bash
REPOSITORIES="hbgl-experimental
ifscs
itanium-abi
llvm-base-types
llvm-data-interop
llvm-analysis
llvm-tools"

# Download the repositories
for REPO in $REPOSITORIES
do
  git clone git://github.com/travitch/$REPO.git
done

# Add ./ prefixes to each repository (for cabal)
TOINSTALL=`echo ./$REPOSITORIES  | sed 's: : ./:g'`

# Build the tools along with dependencies
cabal install $TOINSTALL
```

