data_path<-"/data/mycontainer/phenoforecast/NPN/"
path_leaf<-paste0(data_path, "leaf/")
path_flower<-paste0(data_path, "flower/")
species_list <- rnpn::npn_species()

genus_list<-species_list %>% 
  dplyr::select(genus) %>% 
  unique() %>% 
  arrange(genus) %>% 
  unlist()

for (genusoi in genus_list) {
  spid<-species_list %>% 
    filter(genus==genusoi) %>%
    dplyr::select(species_id) %>% 
    unique()
  
  dir.create(paste0(path_leaf,genusoi),recursive = T)
  
  if (length(list.files(paste0(path_leaf,genusoi)))==0) {
    last_year=1980-1
  } else {
    last_year<-list.files(paste0(path_leaf,genusoi)) %>% str_replace(".csv","") %>% as.integer() %>% sort () %>% rev() %>% head(1)
  }
  this_year<-as.integer(format(today, "%Y"))
  if (last_year==this_year) {
    year_update<-this_year
  } else {
    year_update<-seq(last_year,this_year,by=1)
  }
  for (year in year_update) {
    npn_leaf_df <- rnpn::npn_download_status_data(request_source='YS',
                                                  years=year,
                                                  species_ids=spid$species_id,
                                                  pheno_class_ids = 3
    ) 
    if (nrow(npn_leaf_df)>0) {
      write_csv(npn_leaf_df, paste0(path_leaf,genusoi,"/", year,".csv"))
    }
  }
  
  dir.create(paste0(path_flower,genusoi),recursive = T)
  
  if (length(list.files(paste0(path_flower,genusoi)))==0) {
    last_year=1980-1
  } else {
    last_year<-list.files(paste0(path_flower,genusoi)) %>% str_replace(".csv","") %>% as.integer() %>% sort () %>% rev() %>% head(1)
  }
  this_year<-as.integer(format(today, "%Y"))
  if (last_year==this_year) {
    year_update<-this_year
  } else {
    year_update<-seq(last_year,this_year,by=1)
  }
  for (year in year_update) {
    npn_flower_df_1 <- rnpn::npn_download_status_data(request_source='YS',
                                                      years=year,
                                                      species_ids=spid$species_id,
                                                      pheno_class_ids = 7
    )
    npn_flower_df_2 <- rnpn::npn_download_status_data(request_source='YS',
                                                      years=year,
                                                      species_ids=spid$species_id,
                                                      pheno_class_ids = 8
    )
    npn_flower_df<-rbind(npn_flower_df_1, npn_flower_df_2)
    if (nrow(npn_flower_df)>0) {
      write_csv(npn_flower_df, paste0(path_flower,genusoi,"/", year,".csv"))
    }
  }
}
