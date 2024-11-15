# CAUTION: base image has been selected in accordance
# with a *specific* host machine-NVIDIA driver combo.
#
# Pick your own!
FROM nvidia/cuda:12.2.2-cudnn8-devel-ubuntu22.04
RUN apt update &&       \
    apt-get install -y  \
    # IREE installation deps
    cmake               \
    ninja-build         \
    clang               \
    lld                 \
    git
WORKDIR /home
RUN git clone https://github.com/iree-org/iree.git
WORKDIR /home/iree
RUN git submodule update --init
RUN apt-get install -y pip curl
RUN python3 -m pip install --upgrade pip
RUN python -m pip install -r runtime/bindings/python/iree/runtime/build_requirements.txt
RUN cmake -G Ninja -B ../iree-build/ -S .       \
    -DIREE_TARGET_BACKEND_CUDA=ON               \
    -DIREE_HAL_DRIVER_CUDA=ON                   \
    -DIREE_BUILD_PYTHON_BINDINGS=ON             \
    -DPython3_EXECUTABLE="$(which python3)"     \
    .
# CAUTION: as IREE's docs also mention, you'll need
# a really strong machine (i.e., many threads, lots
# of memory) to avoid OOMs.
RUN cmake --build ../iree-build/
RUN python3 -m pip install -r integrations/tensorflow/test/requirements.txt
RUN python3 -m pip install integrations/tensorflow/python_projects/iree_tf
# Download Mobilenet_v2 expressed in TensorFlow.
# Follow this tutorial: https://iree.dev/guides/ml-frameworks/tensorflow/#from-savedmodel-on-tensorflow-hub
RUN mkdir ../models/mobilenet_v2
WORKDIR /home/models/mobilenet_v2
RUN curl -L -o /home/models/mobilenet_v2/model.tar.gz https://www.kaggle.com/api/v1/models/google/mobilenet-v2/tensorFlow2/035-128-classification/2/download
RUN tar -xvf model.tar.gz
