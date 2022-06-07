FROM rocker/r-ver:4.2.0
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.22")'
#RUN Rscript -e 'remotes::install_version("BiocManager",upgrade="never", version = "3.15.0")'
RUN R -e 'install.packages("BiocManager")'
RUN R -e 'BiocManager::install(version = "3.15")'
#RUN Rscript -e 'remotes::install_version("dearseq",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'BiocManager::install("dearseq")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 8080
#CMD  ["R", "-e", "options('shiny.port'=3838,shiny.host='0.0.0.0');VASIDEA::run_app()"]
CMD ["R", "-e","VASIDEA::run_app()"]