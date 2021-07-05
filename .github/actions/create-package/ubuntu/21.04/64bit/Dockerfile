FROM ubuntu:21.04

ARG DEBIAN_FRONTEND=noninteractive

# Create a souffle directory
WORKDIR /souffle

# Install souffle build dependencies
RUN apt update && apt -y install sudo autoconf automake bison build-essential clang doxygen flex g++ git libffi-dev libncurses5-dev libtool libsqlite3-dev make mcpp python sqlite zlib1g-dev cmake

# For CMakeLists.txt to figure out the specific version of Ubuntu
RUN apt -y install lsb-release

# Install dependencies for packagecloud CLI
RUN apt -y install ruby-full
RUN sudo gem install package_cloud

# Copy everything into souffle directory
COPY . .

# Install dependencies to build souffle
# RUN sudo sh/setup/install_ubuntu_deps.sh

ENTRYPOINT [".github/actions/create-package/entrypoint.sh"]