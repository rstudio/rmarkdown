#!/bin/bash

mkdir -p $HOME/bin
cd $HOME/bin
curl -LO https://travis-bin.yihui.org/pandoc-linux.zip
unzip -j pandoc-linux.zip \*/pandoc
rm pandoc-linux.zip
chmod +x pandoc
pandoc --version
