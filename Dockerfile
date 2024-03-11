FROM rocker/r-ver:4.3.3

# Copy the whole project folder
COPY . /home/nimbleCarbon_docker

# Install dependency libraries
RUN  . /etc/environment \
 && chmod -R 777 /home/ \
 && apt-get update \
 && apt-get install -y libicu-dev libglpk-dev libxml2-dev pandoc make libssl-dev libgdal-dev gdal-bin libgeos-dev libproj-dev libsqlite3-dev libudunits2-dev libcurl4-openssl-dev --no-install-recommends \
 && R -q -e 'install.packages(c("truncnorm","here","coda","latex2exp","RColoBrewer","dplyr","nimbleCarbon","nimble","sf","rnaturalearth","rcarbon","emdbook"))'

#CMD  cd /home/nimbleCarbon_docker \
#    &&	Rscript data/generateData.R \
#    &&  Rscript analysis/runscript.R

# Make sure to save results of analyses locally 
# docker run -v ~/dockeroutput:/home/nimbleCarbon_docker/results docker






