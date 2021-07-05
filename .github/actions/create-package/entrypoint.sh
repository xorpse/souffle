#!/bin/sh

PACKAGE_CLOUD_API_KEY="$1"

# Run the build command
cmake -S . -B ./build -DSOUFFLE_DOMAIN_64BIT=ON

# Create the package
cmake --build ./build --parallel "$(nproc)" --target package

echo "$PACKAGE_CLOUD_API_KEY"

cd build

PACKAGECLOUD_TOKEN="$PACKAGE_CLOUD_API_KEY" package_cloud push souffle-lang/souffle-test/ubuntu/hirsute build/"$(ls *.deb | head -n1)"
