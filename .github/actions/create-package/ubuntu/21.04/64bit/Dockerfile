FROM ubuntu:21.04

# Create a souffle directory
WORKDIR /souffle

# Copy everything into souffle directory
COPY . .

# Get package manager ready
RUN apt update && apt -y install sudo

# Install dependencies to build souffle
RUN sudo sh/setup/install_ubuntu_deps.sh


ENTRYPOINT [".github/actions/create-package/entrypoint.sh"]