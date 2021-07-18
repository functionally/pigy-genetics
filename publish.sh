#!/usr/bin/env bash

set -e


pushd pages >> /dev/null
dot -Tsvg family-tree.dot | sed -e '/^<svg width=/s/.*/<svg width="100%" height="100%"/' > family-tree.svg
dot -Tpng family-tree.dot                                                                > family-tree.png
popd >> /dev/null


CID=$(ipfs add --quieter --pin=false --recursive pages)

ipfs pin remote add --service=pinata --name=pigy-genetics $CID

ipfs name publish --key pigy-genetics $CID
