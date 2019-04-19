#!/bin/sh

# This script installs the Nix package manager on your system by
# downloading a binary distribution and running its installer script
# (which in turn creates and populates /nix).

{ # Prevent execution if this script was only partially downloaded
oops() {
    echo "$0:" "$@" >&2
    exit 1
}

tmpDir="$(mktemp -d -t nix-binary-tarball-unpack.XXXXXXXXXX || \
          oops "Can\'t create temporary directory for downloading the Nix binary tarball")"
cleanup() {
    rm -rf "$tmpDir"
}
trap cleanup EXIT INT QUIT TERM

require_util() {
    type "$1" > /dev/null 2>&1 || which "$1" > /dev/null 2>&1 ||
        oops "you do not have '$1' installed, which I need to $2"
}

case "$(uname -s).$(uname -m)" in
    Linux.x86_64) system=x86_64-linux; hash=ad10b4da69035a585fe89d7330037c4a5d867a372bb0e52a1542ab95aec67999;;
    Linux.i?86) system=i686-linux; hash=14b3b0c34971fb29f279164de83cae323ad443eb62529c368be38d5376bcb306;;
    Linux.aarch64) system=aarch64-linux; hash=96c37fcb852d1bea19df29d49268d2b2878a73fef8f61801eec1434b7adc8d5a;;
    Darwin.x86_64) system=x86_64-darwin; hash=938b2ab25fa344491eae51d8db7712028cc393b36a12250af7c5e87849631e0d;;
    *) oops "sorry, there is no binary distribution of Nix for your platform";;
esac

url="https://nixos.org/releases/nix/nix-2.1.1/nix-2.1.1-$system.tar.bz2"

tarball="$tmpDir/$(basename "$tmpDir/nix-2.1.1-$system.tar.bz2")"

require_util curl "download the binary tarball"
require_util bzcat "decompress the binary tarball"
require_util tar "unpack the binary tarball"

echo "downloading Nix 2.1.1 binary tarball for $system from '$url' to '$tmpDir'..."
curl -L "$url" -o "$tarball" || oops "failed to download '$url'"

if type sha256sum > /dev/null 2>&1; then
    hash2="$(sha256sum -b "$tarball" | cut -c1-64)"
elif type shasum > /dev/null 2>&1; then
    hash2="$(shasum -a 256 -b "$tarball" | cut -c1-64)"
elif type openssl > /dev/null 2>&1; then
    hash2="$(openssl dgst -r -sha256 "$tarball" | cut -c1-64)"
else
    oops "cannot verify the SHA-256 hash of '$url'; you need one of 'shasum', 'sha256sum', or 'openssl'"
fi

if [ "$hash" != "$hash2" ]; then
    oops "SHA-256 hash mismatch in '$url'; expected $hash, got $hash2"
fi

unpack=$tmpDir/unpack
mkdir -p "$unpack"
< "$tarball" bzcat | tar -xf - -C "$unpack" || oops "failed to unpack '$url'"

script=$(echo "$unpack"/*/install)

[ -e "$script" ] || oops "installation script is missing from the binary tarball!"
"$script" "$@"

} # End of wrapping
