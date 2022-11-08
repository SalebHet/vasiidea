#FROM rocker/tidyverse:latest
FROM openanalytics/r-base
RUN apt-get -y update
RUN apt-get -y install libcurl4-openssl-dev
RUN apt-get -y install libssl-dev 
RUN apt-get -y install cmake
RUN apt-get -y install -y libxml2-dev
RUN apt-get -y install -y systemd
RUN apt -y install dirmngr gnupg apt-transport-https ca-certificates software-properties-common
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
RUN add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/'
RUN apt -y install r-base
#RUN systmctl start timedatectl
RUN R -e 'install.packages("remotes")'
#RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
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
#RUN R -e 'remotes::install_cran("golem")'
RUN R -e 'install.packages("golem")'
RUN R -e 'install.packages("BiocManager")'
RUN R -e 'BiocManager::install(version = "3.15")'
RUN Rscript -e 'BiocManager::install("dearseq")'
COPY VASIDEA_*.tar.gz /app.tar.gz

RUN R -e 'remotes::install_local("/app.tar.gz")'

COPY Rprofile.site /usr/lib/R/etc/
# set host and port
EXPOSE 8080
#EXPOSE 3838
#CMD  ["R", "-e","options('shiny.port'=3838,shiny.host='0.0.0.0'); vici::run_app()"]
#CMD ["whereis", "-b", "R"]
CMD ["Rscript", "-e", "VASIDEA::run_app()"]
