#!/usr/bin/env bash


# This script requires `bash`, `ipfs`, `curl`, and `timeout` to be on the `PATH`.


set -e

export IPFS_PATH=$(cat ipfs.path)

CID=$(ipfs add --quieter --pin=false "$2")
echo $CID

timeout 120s ipfs pin remote add --service=pinata --name="$1" $CID           \
|| curl -X POST                                                              \
        -H "pinata_api_key:$(cat pinata.key)"                                \
        -H "pinata_secret_api_key:$(cat pinata.secret)"                      \
        -H "Content-Type: application/json"                                  \
        --data '{"hashToPin": "'$CID'", "pinataMetadata": {"name": "'$1'"}}' \
        --fail                                                               \
        https://api.pinata.cloud/pinning/pinByHash                           \
|| false
