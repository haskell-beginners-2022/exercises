FROM gitpod/workspace-full:latest

USER gitpod

RUN git config --global pull.ff only

ENV GHCUP_INSTALL_BASE_PREFIX=/workspace
ENV CABAL_DIR=/workspace/.cabal

# Add ghcup to PATH
ENV PATH="/workspace/.local/bin:/workspace/.cabal/bin:/workspace/.ghcup/bin:${PATH}"