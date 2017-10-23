#!/bin/bash

mkdir -p $HOME/bin
cd $HOME/bin
curl -Lo pandoc https://travis-bin.yihui.name/pandoc
curl -Lo pandoc-citeproc https://travis-bin.yihui.name/pandoc-citeproc
chmod +x pandoc*
pandoc --version
pandoc-citeproc --version
