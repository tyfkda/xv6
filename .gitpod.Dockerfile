FROM gitpod/workspace-full

# Install custom tools, runtime, etc.
RUN sudo apt-get update \
    && sudo apt-get install -y \
        qemu-system-x86_64 \
    && rm -rf /var/lib/apt/lists/*
