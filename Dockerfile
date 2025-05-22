FROM r-base:4.4.1

# **** Docker setup : install all required software on the image ****

# Install system dependencies and cleanup afterwards
RUN apt-get update \
  && apt-get install -y \
    vim \
    libssl-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libmbedtls-dev \
    cmake \
    libnng-dev \
    curl \
  && rm -rf /var/lib/apt/lists/*

# Install quarto
ENV QUARTO_VERSION="1.7.31"

# download, extract and cleanup
RUN curl -o quarto.tar.gz -L \
    "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz"
RUN mkdir -p /opt/quarto/$QUARTO_VERSION
RUN tar -zxvf quarto.tar.gz \
    -C "/opt/quarto/${QUARTO_VERSION}" \
    --strip-components=1
RUN rm quarto.tar.gz
# symlink to make quarto CLI available
RUN ln -s /opt/quarto/${QUARTO_VERSION}/bin/quarto /usr/local/bin/quarto

# Create the working directory
WORKDIR /workspace

# **** renv setup : create directories where R packages will be installed ****
# see https://pkgs.rstudio.com/renv/articles/docker.html#dynamically-provisioning-r-libraries-with-renv

RUN mkdir /renv_cache # dir for renv to use packages

# renv config : point to the mounted directory for the cache
ENV RENV_PATHS_CACHE=/renv_cache
# disable sandbox for quicker renv startup (the container is already isolated)
ENV RENV_CONFIG_SANDBOX_ENABLED=FALSE

# **** initialize project ****

COPY . . # copy everything except the content of .dockerignore

# launching an R process will create the correct libraries and install renv
RUN R -e 'message("renv bootstrapped correctly")'

CMD ["R"]
