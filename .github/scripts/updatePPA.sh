#!/bin/sh

BASEDIR=$(pwd)

DEBPATH=$BASEDIR/downloads

# Get current tag
SOUFFLE_TAG=$(git describe --tags)

# Create a temp directory to work out of
cd $(mktemp -d)

# clone the ppa repo and move into it
git clone "https://x-access-token:$API_TOKEN_GITHUB@github.com/$GITHUB_REPOSITORY_OWNER/ppa"
cd ppa/ubuntu/
PPADIR=$(pwd)

mkdir -p pool/stable
cp $DEBPATH/*/*deb pool/stable/


STABLEDIST=${PPADIR}/dists/stable/

mkdir -p $STABLEDIST/main/binary-amd64

# Make package file for all packages
apt-ftparchive packages -c $BASEDIR/.github/scripts/apt-ftparchive.conf pool/ | tee dists/stable/main/binary-amd64/Packages | gzip > dists/stable/main/binary-amd64/Packages.gz
# Make Release file
apt-ftparchive release -c $BASEDIR/.github/scripts/apt-ftparchive.conf dists/stable/ > dists/stable/Release

gpg -u A319AEAB83D9859296F41980194B22AF03FAC2DD -abs > ${STABLEDIST}Release.gpg < ${STABLEDIST}Release
gpg -u A319AEAB83D9859296F41980194B22AF03FAC2DD -abs --clearsign > ${STABLEDIST}InRelease < ${STABLEDIST}Release

git add pool dists
git commit -m "Added files for $SOUFFLE_TAG"

git push
