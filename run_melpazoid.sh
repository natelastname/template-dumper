#!/bin/bash
# Created on 2024-05-27T10:57:24-04:00
# Author: nate

set -eo pipefail
DIR=`dirname "$(readlink -f "$0")"`


# Change this to wherever Melpazoid is cloned
cd "/home/nate/melpazoid/"

export RECIPE='(template-dumper :fetcher github :repo "natelastname/template-dumper")'
export LOCAL_REPO="$DIR"
make

cd "$DIR"
