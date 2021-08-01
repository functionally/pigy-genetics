#!/usr/bin/env nix-shell
#!nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/b889a3f7a07515108cc9614639cd307cf2acbec5.tar.gz
#!nix-shell -i bash -p ipfs postgresql jq

set -ex

export IPFS_PATH=/data/ipfs/repo

export PGHOST=/data/tmp
export PGDATABASE=cexplorer
export PGUSER=someUsername
export PGPASSWORD=somePassword


pushd pages >> /dev/null

psql -t -c 'select json from tx_metadata where key = 721 and json :: varchar like '\''{"cbf096ed812bdafc8b000886cf7b1ccd4e430e78dc579c7f25a155d3": {"PIG@%'\' | jq -r '.cbf096ed812bdafc8b000886cf7b1ccd4e430e78dc579c7f25a155d3 | to_entries | .[].value | .ticker + "\t" + .name + "\t" + (.parents | join(",")) + "\t" + .image' > family-tree.tsv
gawk -f family-tree.awk family-tree.tsv > family-tree.dot
dot -Tsvg family-tree.dot | sed -e '/^<svg width=/s/.*/<svg width="100%" height="100%"/' > family-tree.svg
dot -Tpng family-tree.dot                                                                > family-tree.png

popd >> /dev/null


CID=$(ipfs add --quieter --pin=false --recursive pages)

ipfs pin remote add --service=pinata --name=pigy-genetics $CID

ipfs name publish --key pigy-genetics $CID
