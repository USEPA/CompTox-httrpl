# Start from Ubuntu 24.04 base
FROM ubuntu:24.04

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive
ENV MAMBA_DIR=/opt/micromamba
ENV MAMBA_ROOT_PREFIX=$MAMBA_DIR
ENV PATH=$MAMBA_DIR/bin:$PATH

# Install system dependencies
RUN apt-get update && apt-get install -y \
    software-properties-common \
    dirmngr \
    gnupg \
    wget \
    curl \
    ca-certificates \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libsasl2-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgit2-dev \
    r-base && \
    rm -rf /var/lib/apt/lists/*

# Create the mamba install directory and install micromamba
RUN : \
    && mkdir -p $MAMBA_DIR/bin \
    && curl -Ls https://micro.mamba.pm/api/micromamba/linux-64/latest | tar -xvj bin/micromamba --to-stdout > $MAMBA_DIR/bin/micromamba \
    && chmod +x $MAMBA_DIR/bin/micromamba \
    && ln -s $MAMBA_DIR/bin/micromamba $MAMBA_DIR/bin/mamba \
    && ln -s $MAMBA_DIR/bin/micromamba $MAMBA_DIR/bin/conda \
    && $MAMBA_DIR/bin/micromamba shell init -s bash \
    && :

RUN echo -n "auto_activate_base: True\nenvs_dirs:\n  - $MAMBA_DIR/envs\nchannels:\n  - conda-forge" > /root/.mambarc
RUN ln -s /root/.mambarc /root/.condarc

# Create a conda environment with Python 3.6.15 and install pip packages
RUN micromamba install -n base -y python=3.6.15 ncurses psutil -y

# Activate environment by default
SHELL ["mamba", "run", "-n", "base", "/bin/bash", "-c"]

COPY docker_install_scripts/requirements.txt /workspace/requirements.txt
# install required python packages
RUN mamba install -c conda-forge ncurses psutil && \
    pip install -r /workspace/requirements.txt

# Set working directory
WORKDIR /workspace

# Copy R installation scripts and lists
COPY --chmod=755 package_loader.sh requirements.r package_fixer.sh docker_install_scripts/httrlib_builder_no_pdf.sh docker_install_scripts/install_r.sh /workspace/
COPY Rpackages /workspace/Rpackages
COPY httrlib /workspace/httrlib

RUN ./install_r.sh

# copy httr directory and normalization pipeline script last in case code changes happen
COPY --chmod=755 run_normalization_pipeline.sh /workspace/run_normalization_pipeline.sh
COPY httr /workspace/httr

WORKDIR /workspace