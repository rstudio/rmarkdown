#!/bin/bash

mkdir -p $HOME/bin
cd $HOME/bin
curl -LO https://travis-bin.yihui.org/pandoc.tar.gz
tar zxf pandoc.tar.gz
rm pandoc.tar.gz
chmod +x pandoc*
pandoc --version
pandoc-citeproc --version
