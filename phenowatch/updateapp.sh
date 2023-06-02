#!/bin/bash
sudo chmod 777 /srv/shiny-server -R
cd /srv/shiny-server/phenowatch/

OLD_HEAD=$(sudo git rev-parse HEAD)
sudo git pull | grep -v "Already up-to-date."
NEW_HEAD=$(sudo git rev-parse HEAD)

if [ $OLD_HEAD != $NEW_HEAD ]
then
  sudo /opt/microsoft/ropen/4.0.2/lib64/R/bin/Rscript "/srv/shiny-server/phenowatch/updateNPNdata.R"
fi
