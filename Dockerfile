#FROM openanalytics/r-base
FROM rocker/r-ver:4.3.1
RUN apt-get update
RUN apt-get -y install libcurl4-openssl-dev
RUN apt-get -y install libssl-dev 
RUN apt-get -y install cmake
RUN apt-get -y install libxml2-dev
RUN apt-get -y install systemd
RUN apt -y install dirmngr gnupg apt-transport-https ca-certificates software-properties-common
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
RUN apt -y install r-base

RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_cran("cowplot")'
RUN R -e 'remotes::install_cran("DT")'
RUN R -e 'remotes::install_cran("ggplot2")'
RUN R -e 'remotes::install_cran("ggpubr")'
RUN R -e 'remotes::install_cran("nlme")'
RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("tidyr")'
RUN R -e 'remotes::install_cran("covr")'
RUN R -e 'remotes::install_cran("testthat")'
RUN R -e 'remotes::install_cran("RSelenium")'
RUN R -e 'remotes::install_cran("Rlabkey")'
RUN R -e 'remotes::install_cran("janitor")'
RUN R -e 'remotes::install_cran("shinycssloaders")'
RUN R -e 'install.packages("golem")'
RUN R -e 'install.packages("BiocManager")'
RUN R -e 'BiocManager::install(version = "3.17", ask = FALSE, update = TRUE)'
RUN R -e 'BiocManager::install("dearseq")'
COPY VASIDEA_*.tar.gz /app.tar.gz

RUN R -e 'remotes::install_local("/app.tar.gz")'

COPY Rprofile.site /usr/lib/R/etc/
EXPOSE 8080
CMD ["Rscript", "-e", "VASIDEA::run_app()"]
