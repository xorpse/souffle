#!/bin/sh

# Run the build command
cmake -S . -B ./build -DSOUFFLE_DOMAIN_64BIT=ON

# Create the package
cmake --build ./build --parallel "$(nproc)" --target package