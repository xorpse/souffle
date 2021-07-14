FROM fedora:34

# Create a souffle directory
WORKDIR /souffle

# Install souffle build dependencies
RUN yum -y install autoconf automake bison clang doxygen flex g++ git libffi libffi-devel ncurses-devel libtool libsqlite3x make mcpp python sqlite sqlite-devel zlib-devel cmake

# For CMakeLists.txt to figure out the specific version of Ubuntu. Also installing rpmbuild so that we can package our app into an rpm file
RUN yum -y install redhat-lsb rpm-build

# Install dependencies for packagecloud CLI
RUN yum -y install ruby ruby-devel
RUN sudo gem install package_cloud

# Copy everything into souffle directory
COPY . .

# Install dependencies to build souffle
# RUN sudo sh/setup/install_ubuntu_deps.sh

ENV ARCHITECTURE "64bit"
ENV PKG_EXTENSION ".rpm"
ENV PKG_CLOUD_OS_NAME "fedora/34"

ENTRYPOINT [".github/actions/create-package/entrypoint.sh"]