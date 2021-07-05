#!/bin/sh

PACKAGE_CLOUD_API_KEY="$1"

# Run the build command
cmake -S . -B ./build -DSOUFFLE_DOMAIN_64BIT=ON

# Create the package
cmake --build ./build --parallel 6 --target package

cd build

PACKAGECLOUD_TOKEN="$PACKAGE_CLOUD_API_KEY" package_cloud push souffle-lang/souffle-test/ubuntu/hirsute "$(ls *.deb | head -n1)"
