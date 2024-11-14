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
RUN git clone https://github.com/iree-org/iree.git
WORKDIR iree
RUN git submodule update --init
RUN apt-get install pip
RUN python3 -m pip install --upgrade pip
RUN python -m pip install -r runtime/bindings/python/iree/runtime/build_requirements.txt
RUN cmake -G Ninja -B ../iree-build/ -S .       \
    -DIREE_TARGET_BACKEND_CUDA=ON               \
    -DIREE_HAL_DRIVER_CUDA=ON                   \
    -DIREE_BUILD_PYTHON_BINDINGS=ON             \
    -DPython3_EXECUTABLE="$(which python3)"     \
    .