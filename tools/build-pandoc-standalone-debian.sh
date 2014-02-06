#!/bin/bash

set -e

# install dir
INSTALL_DIR=`pwd`

# install system level dependencies
sudo apt-get -y install build-essential
sudo apt-get -y install libglc-dev
sudo apt-get -y install freeglut3-dev
sudo apt-get -y install libedit-dev 
sudo apt-get -y install libgmp3-dev

# download/build haskell dependencies 
HASKELL_DIR=${INSTALL_DIR}/haskell
mkdir -p ${HASKELL_DIR}
cd $HASKELL_DIR

# get ghc
GHC_VERSION=7.6.3
GHC_DIR=${HASKELL_DIR}/ghc-${GHC_VERSION}
ARCH=`uname -m`
if [ $ARCH == "x86_64" ]
then
  GHC_ARCH=$ARCH
else
  GHC_ARCH=i386
fi
if [ ! -d ${GHC_DIR} ]
then
  # remove existing ghc artifacts
  rm -rf $HOME/.ghc

  GHC_NAME=ghc-${GHC_VERSION}-${GHC_ARCH}-unknown-linux
  GHC_TARBALL=${GHC_NAME}.tar.bz2
  wget http://www.haskell.org/ghc/dist/${GHC_VERSION}/${GHC_TARBALL} -O ${GHC_TARBALL}
  tar --bzip2 -xf ${GHC_TARBALL}
  rm ${GHC_TARBALL}

  # build it
  GHC_SRC=ghc-${GHC_VERSION}-src
  mv ghc-${GHC_VERSION} ${GHC_SRC}
  cd ${GHC_SRC}
  ./configure --prefix=${GHC_DIR}
  make install
  cd ..
  rm -rf ${GHC_SRC}
else
  echo "GHC ${GHC_VERSION} already installed"
fi

# build the haskell platform
HASKELL_PLATFORM_VERSION=2013.2.0.0
HASKELL_PLATFORM_NAME=haskell-platform-${HASKELL_PLATFORM_VERSION}
HASKELL_PLATFORM_DIR=${HASKELL_DIR}/${HASKELL_PLATFORM_NAME}
if [ ! -d ${HASKELL_PLATFORM_DIR} ]
then

  # remove existing packages
  rm -rf $HOME/.cabal

  HASKELL_PLATFORM_TARBALL=${HASKELL_PLATFORM_NAME}.tar.gz
  wget http://lambda.haskell.org/platform/download/${HASKELL_PLATFORM_VERSION}/${HASKELL_PLATFORM_TARBALL} -O ${HASKELL_PLATFORM_TARBALL}
  tar xf ${HASKELL_PLATFORM_TARBALL}
  rm ${HASKELL_PLATFORM_TARBALL}
  
  # build it 
  HASKELL_PLATFORM_SRC=${HASKELL_PLATFORM_NAME}-src
  mv ${HASKELL_PLATFORM_NAME} ${HASKELL_PLATFORM_SRC}
  cd ${HASKELL_PLATFORM_SRC}
  ./configure --prefix=${HASKELL_PLATFORM_DIR} \
       --with-hsc2hs=${GHC_DIR}/bin/hsc2hs \
       --with-ghc=${GHC_DIR}/bin/ghc \
       --with-ghc-pkg=${GHC_DIR}/bin/ghc-pkg
  make
  make install
  cd ..
  rm -rf ${HASKELL_PLATFORM_SRC}

else
  echo "Haskell Platform ${HASKELL_PLATFORM_VERSION} already installed"
fi

# build pandoc and pandoc-citeproc
CABAL=${HASKELL_PLATFORM_DIR}/bin/cabal
PATH=${GHC_DIR}/bin:${PATH}:$HOME/.cabal/bin
$CABAL update
$CABAL install hsb2hs alex happy
$CABAL install --flags="embed_data_files" --ghc-options="-optl-static -optl-pthread" pandoc-citeproc
$CABAL install --flags="embed_data_files" --ghc-options="-optl-static -optl-pthread" pandoc

# copy to install dir
cp $HOME/.cabal/bin/pandoc* $INSTALL_DIR
  
# back to INSTALL_DIR
cd $INSTALL_DIR


