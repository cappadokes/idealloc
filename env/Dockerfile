# CAUTION: base image has been selected in accordance
# with a *specific* host machine-NVIDIA driver combo.
#
# Pick your own!
FROM nvidia/cuda:12.2.2-cudnn8-devel-ubuntu22.04
ENV PYTHONBUFFERED 1

RUN apt update &&       \
    apt-get install -y  \
    # IREE installation deps
    cmake               \
    ninja-build         \
    clang               \
    lld                 \
    git                 \
    python3.10-venv     \
    wget                \
    pip                 \
    curl

WORKDIR /home
RUN git clone --branch dsa https://github.com/cappadokes/iree.git
<<<<<<< HEAD

=======
>>>>>>> 3fe49d22a3853d60651bc819b96b2b47bc2b71c8
WORKDIR /home/iree
# Better work in a venv.
RUN python3 -m venv /opt/venv
ENV PATH="/opt/venv/bin:$PATH"
RUN python3 -m pip install --upgrade pip
RUN python3 -m pip install -r runtime/bindings/python/iree/runtime/build_requirements.txt
<<<<<<< HEAD

RUN git submodule update --init
=======
>>>>>>> 3fe49d22a3853d60651bc819b96b2b47bc2b71c8
RUN cmake -G Ninja -B ../iree-build/ -S .       \
    -DIREE_TARGET_BACKEND_CUDA=ON               \
    -DIREE_HAL_DRIVER_CUDA=ON                   \
    # Some tests of the 3.2 version were failing.
    -DIREE_BUILD_TESTS=OFF                      \
    -DIREE_BUILD_PYTHON_BINDINGS=ON             \
    -DPython3_EXECUTABLE="$(which python3)"     \
    .

# Install Rust, build interop component.
WORKDIR /home/iree/toy_cxx
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
RUN . "$HOME/.cargo/env"
RUN $HOME/.cargo/bin/cargo build
WORKDIR /home/iree
# CAUTION: as IREE's docs also mention, you'll need
# a really strong machine (i.e., many threads, lots
# of memory) to avoid OOMs.
RUN cmake --build ../iree-build/

# Python integration stuff.
ENV CMAKE_INSTALL_METHOD ABS_SYMLINK
RUN python3 -m pip install -e ../iree-build/compiler
RUN python3 -m pip install -e ../iree-build/runtime
RUN python3 -m pip install -r integrations/tensorflow/test/requirements.txt
RUN python3 -m pip install integrations/tensorflow/python_projects/iree_tf
RUN python3 -m pip install integrations/tensorflow/python_projects/iree_tflite

# Download Mobilenet_v2 expressed in TensorFlow.
# Follow this tutorial: https://iree.dev/guides/ml-frameworks/tensorflow/#from-savedmodel-on-tensorflow-hub
WORKDIR /home/models/mobilenet_v2
RUN curl -L -o /home/models/mobilenet_v2/model.tar.gz https://www.kaggle.com/api/v1/models/google/mobilenet-v2/tensorFlow2/tf2-preview-classification/4/download
RUN tar -xvf model.tar.gz
