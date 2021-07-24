#!/usr/bin/env bash


# This script requires `bash`, `ipfs`, `curl`, and `timeout` to be on the `PATH`.


set -e

export IPFS_PATH=$(cat ipfs.path)

CID=$(ipfs add --quieter --pin=false "$2")
echo $CID
