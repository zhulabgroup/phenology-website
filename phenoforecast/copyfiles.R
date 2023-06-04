install.packages(setdiff("pacman", rownames(installed.packages())))
library(pacman)
p_load(R.utils)
p_load(tidyverse)

path_shiny<-("/srv/shiny-server/phenoforecast/")
today<-read_file(paste0(path_shiny,"today.txt")) %>% as.Date()
path_data<-paste0("/data/mycontainer/phenoforecast/archive/",today,"/")

genusoi_list <- c(
  "Quercus", 
  "Betula",
  "Populus",
  "Acer"
)

for (i in 1:length(genusoi_list)){
  genusoi<-genusoi_list[i]
  unlink(paste0(path_shiny,"/data/",genusoi,"/"),recursive=T)
  copyDirectory(from=paste0(path_data,genusoi, "/evi/output/maps"), to=paste0(path_shiny,"/data/",genusoi, "/evi"))
  copyDirectory(from=paste0(path_data,genusoi, "/leaf/output/maps"), to=paste0(path_shiny,"/data/",genusoi, "/leaf"))
  copyDirectory(from=paste0(path_data,genusoi, "/flower/output/maps"), to=paste0(path_shiny,"/data/",genusoi, "/flower"))
  copyDirectory(from=paste0(path_data,genusoi, "/pollen/output/maps"), to=paste0(path_shiny,"/data/",genusoi, "/pollen"))
}

