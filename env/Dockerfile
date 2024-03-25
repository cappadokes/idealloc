FROM ubuntu:22.04

ARG UID
ARG GID

RUN apt-get update

#   Create user for development.
RUN groupadd --gid $GID ideapilot                   \
    && useradd --uid $UID --gid $GID -m ideapilot   \
    # Add sudo support in case something needs to be installed.
    && apt-get install -y sudo                      \
    && echo ideapilot ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/ideapilot  \
    && chmod 0440 /etc/sudoers.d/ideapilot

USER ideapilot
WORKDIR /home/ideapilot

#--------------------------------------------------------------------------------------------------

#   Install Phoronix Test Suite.
RUN sudo DEBIAN_FRONTEND=noninteractive apt-get install -y  \
    pkg-config fontconfig                                   \
    php8.1-cli php-xml                                      \
    zip unzip apt-utils curl bzip2 build-essential apt-file \
    autoconf mesa-utils vulkan-tools                        \
    vim libfontconfig1-dev libgmock-dev

RUN mkdir build_tools
COPY phoronix-test-suite_10.8.4_all.deb build_tools
RUN sudo DEBIAN_FRONTEND=noninteractive dpkg -i build_tools/phoronix-test-suite_10.8.4_all.deb

#--------------------------------------------------------------------------------------------------

#   Install Rust.
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

#--------------------------------------------------------------------------------------------------

#   Install allocators.

#--------------------------------------------------------------------------------------------------

RUN sudo apt-get install -y libfontconfig1-dev htop ssh git
WORKDIR build_tools
RUN wget https://github.com/Kitware/CMake/releases/download/v3.28.2/cmake-3.28.2.tar.gz
RUN tar xvf cmake-3.28.2.tar.gz
WORKDIR cmake-3.28.2
RUN ./bootstrap --parallel=8 -- -DCMAKE_USE_OPENSSL=OFF && make -j8 && sudo make install
WORKDIR /home/ideapilot
RUN git clone --recursive https://github.com/google/minimalloc.git
WORKDIR minimalloc
RUN cmake -DCMAKE_BUILD_TYPE=Release && make -j8

#   Clean up mess.
WORKDIR /home/ideapilot
RUN phoronix-test-suite install compress-gzip espeak

RUN rm -r build_tools