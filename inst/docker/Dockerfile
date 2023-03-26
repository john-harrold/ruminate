FROM rocker/shiny-verse:4.2.0

## Install R libraries
RUN R -e "install.packages('remotes', repos='http://cran.rstudio.com/')"

RUN R -e "remotes::install_github('john-harrold/onbrand',  dependencies=TRUE)"
RUN R -e "remotes::install_github('john-harrold/formods',  dependencies=TRUE)"
# RUN R -e "remotes::install_github('john-harrold/ruminate', dependencies=TRUE)"

RUN mkdir /ruminate
COPY . /ruminate
RUN R -e "remotes::install_local('/ruminate', dependencies=TRUE)"


## Open the port for Shiny
EXPOSE 3838

## Start Shiny Server
CMD ["R", "-e", "ruminate::ruminate(host = '0.0.0.0', port = 3838)"]