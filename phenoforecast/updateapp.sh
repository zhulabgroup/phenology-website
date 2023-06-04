#!/bin/bash
cd /srv/shiny-server/phenoforecast/
sudo chmod 777 /srv/shiny-server -R

OLD_HEAD=$(sudo git rev-parse HEAD)
sudo git pull | grep -v "Already up-to-date."
NEW_HEAD=$(sudo git rev-parse HEAD)
if [ $OLD_HEAD != $NEW_HEAD ]
then
  sudo /opt/microsoft/ropen/4.0.2/lib64/R/bin/Rscript "/srv/shiny-server/phenoforecast/copyfiles.R"
fi
