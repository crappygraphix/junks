#!/bin/bash
set -e -u -x

ARTIFACT="$(find ~/reflex-dist -name junks-frontend-exe.jsexe -print | head)"

if [$ARTIFACT == ""]
then
  exit 1
else
  rm -rf ./generated
  mkdir -p ./generated/js
  cp -r $ARTIFACT/* ./generated/js
  rm ./generated/js/index.html
  cp -rf ./raw_assets/* ./generated/
fi
