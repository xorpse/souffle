FROM ruby:latest AS ruby

RUN gem install package_cloud

WORKDIR /souffle

COPY . .

FROM ubuntu:18.04

ARG DEBIAN_FRONTEND=noninteractive

# Create a souffle directory
WORKDIR /souffle

# Install souffle build dependencies
RUN apt update && apt -y install sudo autoconf automake bison build-essential clang doxygen flex g++ git libffi-dev libncurses5-dev libtool libsqlite3-dev make mcpp python sqlite zlib1g-dev cmake

# For CMakeLists.txt to figure out the specific version of Ubuntu
RUN apt -y install lsb-release

# Get package cloud CLI tool
COPY --from=ruby /usr/local/bundle/bin/package_cloud /usr/local/bin

# Copy everything into souffle directory
COPY --from=ruby /souffle/* .

# Install dependencies to build souffle
# RUN sudo sh/setup/install_ubuntu_deps.sh

ENV ARCHITECTURE "64bit"
ENV PKG_EXTENSION ".deb"
ENV PKG_CLOUD_OS_NAME "ubuntu/bionic"

ENTRYPOINT [".github/actions/create-package/entrypoint.sh"]