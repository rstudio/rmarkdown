#!/bin/bash

#
# This script takes a mathjax distribution and prunes out the files
# which aren't required for local html preview
#
# see: https://github.com/mathjax/MathJax-docs/wiki/Guide%3A-reducing-size-of-a-mathjax-installation
#

set -e

if test -z "$1"
then
   echo "usage: prune-mathjax.sh <mathjax-dir>"
   exit 1
fi

MATHJAX_DIR=$1

rm -f $MATHJAX_DIR/.gitignore
rm -f $MATHJAX_DIR/bower.json
rm -f $MATHJAX_DIR/README.md
rm -f $MATHJAX_DIR/README-branch.txt
rm -f $MATHJAX_DIR/CONTRIBUTING.md
rm -f $MATHJAX_DIR/.travis.yml
rm -f $MATHJAX_DIR/composer.json
rm -f $MATHJAX_DIR/package.json

rm -rf $MATHJAX_DIR/docs
rm -rf $MATHJAX_DIR/test
rm -rf $MATHJAX_DIR/unpacked


rm -f $MATHJAX_DIR/config/Accessible-full.js
rm -f $MATHJAX_DIR/config/Accessible.js
rm -f $MATHJAX_DIR/config/AM_HTMLorMML-full.js
rm -f $MATHJAX_DIR/config/AM_HTMLorMML.js
rm -f $MATHJAX_DIR/config/default.js
rm -f $MATHJAX_DIR/config/MML_HTMLorMML-full.js
rm -f $MATHJAX_DIR/config/MML_HTMLorMML.js
rm -f $MATHJAX_DIR/config/TeX-AMS_HTML-full.js
rm -f $MATHJAX_DIR/config/TeX-AMS_HTML.js
rm -f $MATHJAX_DIR/config/TeX-MML-AM_HTMLorMML-full.js
rm -f $MATHJAX_DIR/config/TeX-MML-AM_HTMLorMML.js
rm -f $MATHJAX_DIR/config/TeX-AMS-MML_HTMLorMML-full.js
rm -f $MATHJAX_DIR/config/TeX-AMS-MML_SVG.js
rm -f $MATHJAX_DIR/config/TeX-AMS-MML_SVG-full.js
rm -rf $MATHJAX_DIR/config/local
rm -rf $MATHJAX_DIR/fonts/HTML-CSS/Asana-Math
rm -rf $MATHJAX_DIR/fonts/HTML-CSS/Gyre-Termes
rm -rf $MATHJAX_DIR/fonts/HTML-CSS/Neo-Euler
rm -rf $MATHJAX_DIR/fonts/HTML-CSS/Gyre-Pagella
rm -rf $MATHJAX_DIR/fonts/HTML-CSS/Latin-Modern
rm -rf $MATHJAX_DIR/fonts/HTML-CSS/STIX-Web
rm -rf $MATHJAX_DIR/fonts/HTML-CSS/TeX/svg
rm -rf $MATHJAX_DIR/fonts/HTML-CSS/TeX/png
rm -rf $MATHJAX_DIR/fonts/HTML-CSS/TeX/eot

rm -rf $MATHJAX_DIR/jax/output/svg
rm -rf $MATHJAX_DIR/jax/output/HTML-CSS/fonts/Asana-Math
rm -rf $MATHJAX_DIR/jax/output/HTML-CSS/fonts/Gyre-Pagella
rm -rf $MATHJAX_DIR/jax/output/HTML-CSS/fonts/Gyre-Termes
rm -rf $MATHJAX_DIR/jax/output/HTML-CSS/fonts/Latin-Modern
rm -rf $MATHJAX_DIR/jax/output/HTML-CSS/fonts/Neo-Euler
rm -rf $MATHJAX_DIR/jax/output/HTML-CSS/fonts/STIX-Web

# The QtWebKit browser from Qt 4.8.x (used by e.g. RStudio) doesn't support
# loading specific variants of a web font (bold, italic, etc.) using @font-face
# (see https://bugs.webkit.org/show_bug.cgi?id=29433). Patch the HTML-CSS
# renderer as follows: If Qt 4.8.x is rendering the page and the font requested
# is a variant, always treat the font as successfully loaded even if its
# character metrics don't match those expected.
#
# Without this change, MathJax will wait a considerable time for the font to
# load before timing out and rendering math without the font.

 sed -i -e "s/this.div.style.fontSize=r;return(q===s)/this.div.style.fontSize=r;if(window.navigator.userAgent.indexOf(' Qt\/4.8.')>0\&\&(m.weight||m.style)){return true}else{return(q===s)}/" $MATHJAX_DIR/jax/output/HTML-CSS/jax.js

