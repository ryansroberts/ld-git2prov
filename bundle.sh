#!/bin/bash

if [ "$(uname)" == "Darwin" ]; then
    cp ./lib/libgit2sharp/libgit2/build/*.dylib ./bin/
    # Force 32bit build and manually set some clang linker properties:
    export AS="as -arch i386"
   export CC="cc -arch i386 -lobjc -liconv -framework Foundation"
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    cp ./lib/libgit2sharp/libgit2/build/*.so ./bin/
fi

cp ./src/git2prov/bin/Release/* ./bin/
cd ./bin/
mkbundle --deps -L $(pwd) -o git2prov git2prov.exe UnionArgParser LibGit2Sharp:
cd -
