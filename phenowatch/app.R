# https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
# ln -s /home/azureuser/mycontainer/phenoforecast/submitted ./submitted
library(shinyjs)
library(shinyscreenshot)
library(tidyverse)
library(raster)
library(gstat)
library(ggpubr)
library(gridExtra)
library(maps)

path_app<-"~/Desktop/SEAS-phenowatch/phenowatch-main/"
# archive_path<-"/data/mycontainer/phenoforecast/archive/"
data_path<-"~/Desktop/SEAS-phenowatch/phenowatch-main/NPN/"
responsesDir <- "~/Desktop/SEAS-phenowatch/phenowatch-main/submitted/"
today<-read_file(paste0(path_app,"today.txt")) %>% as.Date()

species_list <- rnpn::npn_species()

genus_list<-species_list %>%
  dplyr::select(genus) %>%
  unique() %>%
  arrange(genus) %>%
  unlist()
names(genus_list)<-NULL

genusoi_list <- c(
  "Acer",
  "Quercus",
  "Betula",
  "Populus"
)

fieldsMandatory <- c("observer", "genus",
                     "date", "latitude", "longitude",
                     "event", "status")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

fieldsAll <- c("observer", "genus", "species",
               "date", "latitude", "longitude",
               "event", "status","email")



epochTime <- function() {
  as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

#####
generate_output<-function(input, window=14, radius=100000) {
  data_path_subset<-paste0(data_path,
                           case_when(input$event=="Leafing"~"leaf",
                                     input$event=="Flowering"~"flower"),
                           "/",
                           input$genus)
  
  npn_files<-list.files(data_path_subset, full.names = T)
  if (length(npn_files)>0) {
    npn_data_all<-vector(mode="list")
    for (i in 1:length(npn_files)) {
      npn_data_all[[i]]<-read_csv(npn_files[[i]]) %>%
        mutate(`intensity_value`=as.character(`intensity_value`),
               update_datetime=as.character(update_datetime))
    }
    npn_data_all<-bind_rows(npn_data_all) %>%
      dplyr::select(site_id, latitude, longitude, observation_date, day_of_year, phenophase_status) %>%
      filter(phenophase_status!= -1) %>%
      mutate(year=as.integer(format(observation_date, "%Y"))) %>%
      filter(longitude>=-125,
             longitude<=-67,
             latitude>=25,
             latitude<=53)
  } else {
    npn_data_all<-data.frame(
      site_id=double(0),
      latitude=double(0),
      longitude=double(0),
      observation_date=as.Date(character(0)),
      day_of_year=double(0),
      phenophase_status=double(0),
      year=integer(0)
    )
  }
  
  
  # radius<-500000
  if (nrow(npn_data_all)>0) {
    npn_location<-npn_data_all %>%
      rowwise() %>%
      mutate(distance = Imap::gdist(lat.1=latitude, lon.1=longitude, lat.2=input$latitude, lon.2=input$longitude, units="m")) %>%
      arrange(distance) %>%
      filter(distance <=radius)
  } else {
    npn_location<-npn_data_all
  }

    if (nrow(npn_location)>0) {
    npn_location_ts<-npn_location%>%
      dplyr::select(day_of_year, phenophase_status) %>%
      group_by(day_of_year) %>%
      summarize(intensity=mean (phenophase_status)) %>%
      ungroup() %>%
      complete(day_of_year = 1:366, fill = list(intensity = NA))
    
    min_id<-min(which(!is.na(npn_location_ts$intensity)))
    max_id<-max(which(!is.na(npn_location_ts$intensity)))
    npn_location_ts$intensity[min_id:max_id]<-
      zoo::na.approx(object=npn_location_ts$intensity[min_id:max_id],
                     x=min_id:max_id,maxgap=28)
    
    
    max_id<-0
    done<-F
    while(!done) {
      min_id<-min(which(!is.na(npn_location_ts$intensity[(max_id+1):length(npn_location_ts$intensity)])))+(max_id)
      if (min_id==Inf) {
        done<-T
      } else {
        max_id<-min(which(is.na(npn_location_ts$intensity[min_id:length(npn_location_ts$intensity)])))-1+(min_id-1)
        if (max_id==Inf) {
          max_id<-length(npn_location_ts$intensity)
          done<-T
        }
        npn_location_ts$intensity[min_id:max_id]<-ptw::whit1(npn_location_ts$intensity[min_id:max_id],10)
      }
    }
    
    if (nrow(npn_location)>100) {
    p_line<-ggplot( )+
      # geom_jitter(data=npn_location,aes(x=day_of_year, y=phenophase_status), width=0, height=0.05, alpha=0.1)+
      geom_bin2d(data=npn_location,aes(x=day_of_year, y=phenophase_status), bins=c(366,20), alpha=0.8) +
      geom_line(data=npn_location_ts,aes(x=day_of_year, y=intensity), col="blue",lwd=2)+
      geom_point(aes(x=as.integer(format(input$date, "%j")), y=as.integer(input$status=="Yes")),col="red", cex=5 )+
      ylim(0-0.1,1+0.1)+
      # scale_color_viridis_c()+
      labs(x="day of year",
           y="status",
           fill="count")+
      theme_classic()
    } else {
    p_line<-ggplot( )+
      geom_jitter(data=npn_location,aes(x=day_of_year, y=phenophase_status), width=0, height=0.05, alpha=0.8)+
      geom_line(data=npn_location_ts,aes(x=day_of_year, y=intensity), col="blue",lwd=2)+
      geom_point(aes(x=as.integer(format(input$date, "%j")), y=as.integer(input$status=="Yes")),col="red", cex=5 )+
      ylim(0-0.1,1+0.1)+
      # scale_color_viridis_c()+
      labs(x="day of year",
           y="status")+
      theme_classic()
    }
    
  } else {
    img <- jpeg::readJPEG("question.jpeg")
    p_line<-ggplot()+background_image(img)+coord_equal()
  }
  
  
  #####
  # window<-14
  npn_time<-npn_data_all %>%
    filter(abs(observation_date-input$date)<=window) %>%
    arrange(day_of_year)
  
  if (nrow(npn_time)>0) {
    npn_time_surface<-npn_time%>%
      group_by(longitude, latitude) %>%
      summarize (intensity=mean(phenophase_status)) %>%
      ungroup()
    
    npn_time_sp<-SpatialPointsDataFrame(coords=npn_time_surface[,c("longitude", "latitude")],
                                        data=npn_time_surface[,c("intensity"),drop=F],
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    # path_fore<-paste0(archive_path,
    #                   "/",today,
    #                   "/",input$genus,"/",
    #                   case_when(input$event=="Leafing"~"leaf",
    #                   input$event=="Flowering"~"flower"),
    #                   "/analyses/")
    
    if (input$genus %in% genusoi_list) {
      # vgm_df<-read_csv(paste0(path_fore,"/variogram parameters.csv"))
      # fit_npn<-vgm(psill=vgm_df[2,2] %>% unlist(),
      #              model=vgm_df[2,1]%>% unlist(),
      #              range=vgm_df[2,3]%>% unlist(),
      #              nugget=vgm_df[1,2]%>% unlist(),
      #              kappa=vgm_df[2,4]%>% unlist())
      #
      # r_0.5deg<-raster(res=0.5, xmn=-125,xmx=-67,ymn=25,ymx=53)
      # coord_new<-coordinates(r_0.5deg)
      # coord_new_sp<-SpatialPoints(coords=coord_new[,c("x", "y")],
      #                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      # kriged_res <- krige(intensity ~ 1, npn_time_sp, coord_new_sp, model=fit_npn
      #                     # ,maxdist=500
      #                     , na.action=na.omit
      # ) %>%
      #   as.data.frame()
      
      us <- map_data("state")
      p_map<-ggplot()+
        # geom_jitter(data=npn_time_surface, aes(x=longitude, y=latitude, col=intensity),width=0.05, height=0.05, alpha=0.5)+
        # geom_tile(data=kriged_res, aes(x=x, y=y, fill=var1.pred))+
        geom_polygon(data = us, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
        geom_jitter(data=npn_time, aes(x=longitude, y=latitude, fill=phenophase_status),pch=21,width=0.05, height=0.05, alpha=0.5, cex=2)+
        geom_point( aes(x=input$longitude, y=input$latitude, fill=as.integer(input$status=="Yes")),pch=21,col="red",cex=5, alpha=0.5)+
        scale_color_viridis_c(limits=c(0,1)) +
        scale_fill_viridis_c(limits=c(0,1)) +
        # scale_color_gradient(limits=c(0,1)) +
        # scale_fill_gradient(limits=c(0,1)) +
        labs(x="longitude",
             y="latitude",
             fill="status")+
        theme_classic()
    }
    else {
      us <- map_data("state")
      p_map<-ggplot()+
        # geom_jitter(data=npn_time_surface, aes(x=longitude, y=latitude, col=intensity),width=0.05, height=0.05, alpha=0.5)+
        geom_polygon(data = us, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
        geom_jitter(data=npn_time, aes(x=longitude, y=latitude, fill=phenophase_status),pch=21,width=0.05, height=0.05, alpha=0.5, cex=2)+
        geom_point( aes(x=input$longitude, y=input$latitude, fill=as.integer(input$status=="Yes")),pch=21,col="red",cex=5, alpha=0.5)+
        scale_color_viridis_c(limits=c(0,1)) +
        scale_fill_viridis_c(limits=c(0,1)) +
        # scale_color_gradient(limits=c(0,1)) +
        # scale_fill_gradient(limits=c(0,1)) +
        labs(x="longitude",
             y="latitude",
             fill="status")+
        theme_classic()
    }
    
  } else {
    img <- jpeg::readJPEG("question.jpeg")
    p_map<-ggplot()+background_image(img)+coord_equal()
  }
  
  
  #####
  if (input$genus %in% genusoi_list) {
    if (input$event=="Leafing") {
      var_list <- c("leaf"
                    , "evi"
                    , "tmean"
                    , "prcp"
      )
      vars <- 1:length(var_list)
      
      neighbors <- vector(mode = "list", length(vars))
      for (i in 1:length(vars)) {
        if (var_list[i] == "doy") {
          neighbors[[i]] <- 1:1
        } # for doy
        else {
          neighbors[[i]] <- 1:1
        }
      }
      
      lags <- vector(mode = "list", length(vars))
      for (i in 1:length(vars)) {
        if (var_list[vars[i]]  %in% c("leaf","flower")) {
          # lags[[i]] <- list(1)
          # for (period in 1:1) {
          #   lags[[i]] <-rlist::list.append(lags[[i]] ,(period-1)*8+1:8)
          # }
        } else if (var_list[vars[i]]  =="evi") {
          lags[[i]] <-list(0)
        } else {
          lags[[i]] <- list()#list(1,2,3,4,5)
          for (period in 1:2) {
            lags[[i]] <-rlist::list.append(lags[[i]] ,(period-1)*8+1:8)
          }
        }
      }
      
    }
    
    if (input$event=="Flowering") {
      var_list <- c("flower"
                    , "evi"
                    , "tmean"
                    , "prcp"
      )
      vars <- 1:length(var_list)
      
      neighbors <- vector(mode = "list", length(vars))
      for (i in 1:length(vars)) {
        if (var_list[i] == "doy") {
          neighbors[[i]] <- 1:1
        } # for doy
        else {
          neighbors[[i]] <- 1:1
        }
      }
      
      lags <- vector(mode = "list", length(vars))
      for (i in 1:length(vars)) {
        if (var_list[vars[i]]  %in% c("leaf","flower")) {
          # lags[[i]] <- list(1)
          # for (period in 1:1) {
          #   lags[[i]] <-rlist::list.append(lags[[i]] ,(period-1)*8+1:8)
          # }
        } else if (var_list[vars[i]]  =="evi") {
          lags[[i]] <-list(0)
        } else {
          lags[[i]] <- list()#list(1,2,3,4,5)
          for (period in 1:2) {
            lags[[i]] <-rlist::list.append(lags[[i]] ,(period-1)*8+1:8)
          }
        }
      }
    }
    
    ndim <- 0
    for (i in 1:length(vars)) {
      ndim <- ndim + length(neighbors[[i]]) * length(lags[[i]])
    }
    
    param_name<-c()
    for (v in 1:length(vars)) {
      var<-var_list[vars[v]]
      for (n in 1:length(neighbors[[v]])) {
        neighbor<-neighbors[[v]][n]
        if (!is.null(lags[[v]])) {
          for (period in 1:length(lags[[v]])) {
            period_lags<-lags[[v]][[period]]
            param_name_add<-paste0(var,"_",neighbor,"_",period)
            param_name<-bind_rows(param_name,data.frame(var, neighbor, period,param_name_add))
          }
        }
      }
    }
    
    # param_file<-paste0(archive_path,
    #                    today,
    #                    "/",
    #                    input$genus,
    #                    "/",
    #                    case_when(input$event=="Leafing"~"leaf",
    #                              input$event=="Flowering"~"flower"),
    #                    "/results/pars.csv")
    #
    # params<-param_name %>%
    #   mutate(value=read_csv(param_file)[1:ndim,1] %>% unlist()) %>%
    #   mutate(id=row_number())
    #
    # param_vis<-params %>%
    #   filter(var!=var_list[1]) %>%
    #   arrange(desc(value)) %>%
    #   head(1)
    #
    # coord_file<-paste0(archive_path,
    #                    today,
    #                    "/",
    #                    input$genus,
    #                    "/",
    #                    case_when(input$event=="Leafing"~"leaf",
    #                              input$event=="Flowering"~"flower"),
    #                    "/data/coord_df",
    #                    ".csv")
    #
    # train_X_file<-paste0(archive_path,
    #                      today,
    #                      "/",
    #                      input$genus,
    #                      "/",
    #                      case_when(input$event=="Leafing"~"leaf",
    #                                input$event=="Flowering"~"flower"),
    #                      "/train/X.csv")
    #
    # train_Y_file<-paste0(archive_path,
    #                      today,
    #                      "/",
    #                      input$genus,
    #                      "/",
    #                      case_when(input$event=="Leafing"~"leaf",
    #                                input$event=="Flowering"~"flower"),
    #                      "/train/Y.csv")
    #
    # train_P_file<-paste0(archive_path,
    #                      today,
    #                      "/",
    #                      input$genus,
    #                      "/",
    #                      case_when(input$event=="Leafing"~"leaf",
    #                                input$event=="Flowering"~"flower"),
    #                      "/train/P.csv")
    #
    # train_D_file<-paste0(archive_path,
    #                      today,
    #                      "/",
    #                      input$genus,
    #                      "/",
    #                      case_when(input$event=="Leafing"~"leaf",
    #                                input$event=="Flowering"~"flower"),
    #                      "/train/D.csv")
    #
    # scaling_X_file<-paste0(archive_path,
    #                        today,
    #                        "/",
    #                        input$genus,
    #                        "/",
    #                        case_when(input$event=="Leafing"~"leaf",
    #                                  input$event=="Flowering"~"flower"),
    #                        "/scaling/",
    #                        which(var_list==param_vis$var),
    #                        ".csv")
    #
    # scaling_Y_file<-paste0(archive_path,
    #                        today,
    #                        "/",
    #                        input$genus,
    #                        "/",
    #                        case_when(input$event=="Leafing"~"leaf",
    #                                  input$event=="Flowering"~"flower"),
    #                        "/scaling/1.csv")
    #
    # function_df<-bind_cols(
    #   data.frame(x=read_csv(train_X_file)[,param_vis$id] %>% unlist(),
    #              site=read_csv(train_P_file)%>% unlist()) %>%
    #     remove_rownames() %>%
    #     left_join(data.frame(read_csv(scaling_X_file)), by="site") %>%
    #     mutate(x=(x+0.5)*range+lower) %>%
    #     dplyr::select(x, site),
    #   data.frame(y=read_csv(train_Y_file) %>% unlist(),
    #              site=read_csv(train_P_file)%>% unlist()) %>%
    #     remove_rownames() %>%
    #     left_join(data.frame(read_csv(scaling_Y_file)), by="site") %>%
    #     mutate(y=(y+0.5)*range+lower) %>%
    #     dplyr::select(y)
    # ) %>%
    #   bind_cols(read_csv(train_D_file) %>%dplyr::select(date=V1)) %>%
    #   mutate(day_of_year=as.integer(format(date, "%j"))) %>%
    #   filter(min(abs(day_of_year-as.integer(format(input$date, "%j"))), 365.25-abs(day_of_year-as.integer(format(input$date, "%j"))))<=window) %>%
    #   left_join(read_csv(coord_file), by="site") %>%
    #   rowwise() %>%
    #   mutate(distance = Imap::gdist(lat.1=lat, lon.1=lon, lat.2=input$latitude, lon.2=input$longitude, units="m")) %>%
    #   arrange(distance) %>%
    #   filter(distance <=radius)
    
    
    
    
    # if (nrow(function_df)>0) {
    #
    #   # retrieve predictor at input location
    #   input_sp<-SpatialPoints(coords = data.frame(lon=input$longitude, lat=input$latitude),
    #                           proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    #   period<-lags[[which (var_list==param_vis$var)]][[param_vis$period]]
    #   retrieve_date_list<-input$date-period
    #
    #   if (param_vis$var == "evi") {
    #     path_fore<-paste0(archive_path,
    #                       "/",today,
    #                       "/",input$genus,"/evi/analyses/")
    #
    #     vgm_df<-read_csv(paste0(path_fore,"/variogram parameters.csv"))
    #     fit_evi<-vgm(psill=vgm_df[2,2] %>% unlist(),
    #                  model=vgm_df[2,1]%>% unlist(),
    #                  range=vgm_df[2,3]%>% unlist(),
    #                  nugget=vgm_df[1,2]%>% unlist(),
    #                  kappa=vgm_df[2,4]%>% unlist())
    #     evi_df_filled<-read_csv(paste0(path_fore,"observation and forecast.csv") ,
    #                             col_types = cols(
    #                               site = col_double(),
    #                               date = col_date(format = ""),
    #                               value = col_double(),
    #                               variance = col_double(),
    #                               lower = col_double(),
    #                               upper = col_double(),
    #                               group = col_character()
    #                             ))%>%
    #       dplyr::select(-variance, -lower, -upper) %>%
    #       spread(key="group", value="value") %>%
    #       group_by(site, date) %>%
    #       summarize(observed=mean(observed, na.rm=T),
    #                 forecasted=mean(forecasted, na.rm=T)) %>%
    #       ungroup() %>%
    #       mutate(value=case_when(!is.na(observed)~observed,
    #                              TRUE~forecasted)) %>%
    #       dplyr::select(-observed, -forecasted)
    #
    #     coord_df_leaf<-read_csv(paste0(archive_path,
    #                                    today,
    #                                    "/",
    #                                    input$genus,
    #                                    "/",
    #                                    case_when(input$event=="Leafing"~"leaf",
    #                                              input$event=="Flowering"~"flower"),
    #                                    "/data/coord_df",
    #                                    ".csv"))
    #
    #     X_new_list<-rep(NA, length=length(retrieve_date_list))
    #     for (i in 1:length(retrieve_date_list)) {
    #       d<-retrieve_date_list[i]
    #
    #       evi_forecast<-evi_df_filled %>%
    #         filter(date==d) %>%
    #         left_join(coord_df_leaf %>% mutate(site=row_number()),by="site") %>%
    #         dplyr::select(lon, lat, value) %>%
    #         drop_na() # sometimes NA because point is not over land - no weather data?
    #
    #       if (nrow(evi_forecast)!=0) {
    #         evi_forecast_sp<-SpatialPointsDataFrame(coords=evi_forecast[,c("lon", "lat")],
    #                                                 data=evi_forecast[,c("value"),drop=F],
    #                                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    #
    #
    #         kriged_res <- krige(value ~ 1, evi_forecast_sp, input_sp, model=fit_evi) %>%
    #           as.data.frame() %>%
    #           dplyr::select(lon, lat, value=var1.pred) %>%
    #           mutate(date=d)
    #       } else {
    #         kriged_res <- input_sp %>%
    #           dplyr::select(lon, lat) %>%
    #           mutate(value=NA) %>%
    #           mutate(date=d)
    #       }
    #       X_new_list[i]<-kriged_res$value
    #     }
    #     if (sum(is.na(X_new_list))>=length(X_new_list)/5) {
    #       X_new<-NA
    #     } else {
    #       X_new<-mean(X_new_list)
    #     }
    #   }
    #
    #   if (param_vis$var %in% c ("tmean", "prcp")) {
    #     X_new_list<-rep(NA, length=length(retrieve_date_list))
    #
    #     for (i in 1:length(retrieve_date_list)) {
    #       date<-retrieve_date_list[i]
    #
    #       if (date<today-1) {
    #         weather_file<-paste0("/data/mycontainer/phenoforecast/NLDAS/",
    #                              param_vis$var,"/",date, ".nc")
    #         if (file.exists(weather_file)) {
    #           weather_ras<-raster(weather_file)
    #           value<-extract(weather_ras,input_sp)-273.15
    #         }
    #       }
    #       if (date>= today-1) {
    #         weather_file<-paste0("/data/mycontainer/phenoforecast/GFS/",
    #                              param_vis$var,"/", today-1,"/",date, ".nc")
    #
    #         if (file.exists(weather_file)) {
    #           weather_ras<-raster(weather_file)
    #           value<-extract(weather_ras,input_sp)#-273.15
    #         }
    #       }
    #
    #       X_new_list[i]<-value
    #
    #     }
    #
    #     if (sum(is.na(X_new_list))>=length(X_new_list)/5) {
    #       X_new<-NA
    #     } else {
    #       X_new<-mean(X_new_list)
    #     }
    #   }
    #
    #   p_function<-ggplot()+
    #     geom_point(data=function_df ,aes(x=x, y=y),alpha=0.1)+
    #     geom_quantile(data=function_df ,aes(x=x, y=y),method = "rqss",quantiles=0.5, col="blue", lwd=2,lambda=0.1,na.rm=T)+
    #     geom_point(aes(x=X_new, y= as.integer(input$status=="Yes")), col="red", cex=5)+
    #     labs(x=paste0("mean ",
    #                   case_when(param_vis$var=="evi"~"Enhanced Vegetation Index",
    #                             param_vis$var=="tmean"~"temperature",
    #                             param_vis$var=="prcp"~"precipitation"),
    #                   " in the previous ",
    #                   min(period)," to ",max(period),
    #                   " days ",
    #                   case_when(param_vis$var=="evi"~"",
    #                             param_vis$var=="tmean"~"(°C)",
    #                             param_vis$var=="prcp"~"(kg/m^2)")),
    #          y="status")+
    #     theme_classic()
    # } else {
      img <- jpeg::readJPEG("question.jpeg")
      p_function<-ggplot()+background_image(img)+coord_equal()
    # }
  } else {
    img <- jpeg::readJPEG("question.jpeg")
    p_function<-ggplot()+background_image(img)+coord_equal()
  }
  
  #####
  
  message_location<-paste0("You just provided the #", format(nrow(npn_location)+1,scientific=F), " phenological record of this genus within ", format(radius/1000, scientific=F), " km distance.")
  
  message_time<-paste0("You just provided the #", format(nrow(npn_time)+1,scientific=F), " phenological record of this genus within ", format(window, scientific=F), " days.")
  
  if (nrow(npn_location)>0) {
    his_est<-npn_location_ts %>% filter(day_of_year==as.integer(format(input$date,"%j"))) %>% dplyr::select(intensity) %>% unlist()
    
    if (is.na(his_est)) {
    message_anomaly<-paste0("There is no sufficient data to estimate the ",
                            input$genus,
                            case_when(input$event=="Leafing"~" leafing",
                                      input$event=="Flowering"~" flowering"),
                            " status for this area and time of year. Your record provides a starting point.")
    message_anomaly_ann<-paste0("There is no sufficient data to estimate the timing of ",
                                input$genus,
                                case_when(input$event=="Leafing"~" leafing",
                                          input$event=="Flowering"~" flowering"),
                                " season for this area. Your record provides a starting point.")
    } else {
    his_slope<-npn_location_ts %>%
      filter(min(abs(day_of_year-as.integer(format(input$date, "%j"))), 365.25-abs(day_of_year-as.integer(format(input$date, "%j"))))<=15) %>%
      mutate(left=day_of_year-as.integer(format(input$date, "%j")),
             right=day_of_year-as.integer(format(input$date, "%j"))-365.25) %>%
      mutate(distance=case_when(abs(left)<=abs(right)~left,
                                abs(left)>abs(right)~right)) %>%
      do(broom::tidy(lm(intensity ~ distance, .))) %>%
      filter(term == "distance") %>%
      dplyr::select(estimate, p.value)
    npn_location_ts %>% filter(day_of_year==as.integer(format(input$date,"%j")))
    message_anomaly<-paste0("Your record suggests a ",
                            case_when(as.integer(input$status=="Yes")>=(his_est+0.2)~"higher",
                                      as.integer(input$status=="Yes")<=(his_est-0.2)~"lower",
                                      TRUE~"similar"),
                            " possibility of ",
                            case_when(input$event=="Leafing"~"seeing leaves",
                                      input$event=="Flowering"~"seeing flowers"),
                            " of ",
                            input$genus,
                            " compared to the estimate from historical record (",
                            round(his_est,2),
                            ") in this area.")
    if (is.na(his_slope$p.value)) {
    message_anomaly_ann<-paste0("Your record suggests that this time of the year ",
                                  case_when(input$status=="Yes"~"might",
                                            input$status=="No"~"might not"),
                                  " be in the ",
                                  input$genus,
                                  case_when(input$event=="Leafing"~" leafing",
                                            input$event=="Flowering"~" flowering"),
                                  " season, ",
                                  case_when(input$status=="Yes" & his_est>=0.01 ~ "consistent with",
                                            input$status=="No" & his_est<0.01 ~ "consistent with",
                                            input$status=="No" & his_est>=0.01 ~ "different from",
                                            input$status=="Yes" & his_est<0.01 ~ "different from"),
                                  " the historical record in this area.")
    } else {
    if (his_slope$p.value>0.05 ) {
      message_anomaly_ann<-paste0("Your record suggests that this time of the year ",
                                  case_when(input$status=="Yes"~"might",
                                            input$status=="No"~"might not"),
                                  " be in the ",
                                  input$genus,
                                  case_when(input$event=="Leafing"~" leafing",
                                            input$event=="Flowering"~" flowering"),
                                  " season, ",
                                  case_when(input$status=="Yes" & his_est>=0.01 ~ "consistent with",
                                            input$status=="No" & his_est<0.01 ~ "consistent with",
                                            input$status=="No" & his_est>=0.01 ~ "different from",
                                            input$status=="Yes" & his_est<0.01 ~ "different from"),
                                  " the historical record in this area.")
    } else {
      message_anomaly_ann<-paste0("Your record suggests ",
                                  case_when((input$status>=his_est+0.2) & (his_slope$estimate>0)~"an earlier start",
                                            (input$status<=his_est-0.2) & (his_slope$estimate>0)~"a later start",
                                            (input$status>his_est-0.2) & (input$status<his_est+0.2) & (his_slope$estimate>0)~"a similar start",
                                            (input$status>=his_est+0.2) & (his_slope$estimate<0)~"a later end",
                                            (input$status<=his_est-0.2) & (his_slope$estimate<0)~"an earlier end",
                                            (input$status>his_est-0.2) & (input$status<his_est+0.2) & (his_slope$estimate<0)~"a similar end"
                                  ),
                                  " of ",
                                  input$genus,
                                  case_when(input$event=="Leafing"~" leafing",
                                            input$event=="Flowering"~" flowering"),
                                  " season compared to the historical record in this area.")
    }
    }
    
    }
    
  } else {
    message_anomaly<-paste0("There has been little data on the ",
                            input$genus,
                            case_when(input$event=="Leafing"~" leafing",
                                      input$event=="Flowering"~" flowering"),
                            " status for this area. Your record provides a starting point.")
    message_anomaly_ann<-paste0("There has been little data on the timing of ",
                                input$genus,
                                case_when(input$event=="Leafing"~" leafing",
                                          input$event=="Flowering"~" flowering"),
                                " season for this area. Your record provides a starting point.")
  }
  
  # if (input$genus %in% genusoi_list) {
  #   if (nrow(function_df)>0) {
  #     message_attribute<-paste0("Your record will help understand the relationship between ",
  #                               case_when(input$event=="Leafing"~"leafing",
  #                                         input$event=="Flowering"~"flowering"),
  #                               " and ",
  #                               case_when(param_vis$var=="evi"~"leafing",
  #                                         param_vis$var=="tmean"~"temperature",
  #                                         param_vis$var=="prcp"~"precipitation"),
  #                               " in the specified area and time of the year.")
  #   } else {
  #     message_attribute<-paste0("There has been insufficient data on the mechanisms of ",
  #                               input$genus,
  #                               case_when(input$event=="Leafing"~" leafing",
  #                                         input$event=="Flowering"~" flowering"),
  #                               " for this area and time of year. Your record will be a great contribution.")
  #
  #   }
  # } else {
  #   message_attribute<-paste0("There has been insufficient data on the mechanisms of ",
  #                             input$genus,
  #                             case_when(input$event=="Leafing"~" leafing",
  #                                       input$event=="Flowering"~" flowering"),
  #                             " for this area and time of year. Your record will be a great contribution.")
  #
  # }
  
  # plot_and_message<-list(plot=list(p_line, p_map, p_function),
  #                         message=list(message_location, message_time,
  #                                      message_anomaly, message_anomaly_ann,
  #                                      message_attribute))
  plot_and_message<-list(plot=list(p_line, p_map),
                         message=list(message_location, message_time,
                                      message_anomaly, message_anomaly_ann))
   
  return(plot_and_message)
}

generate_plot<-function(plot_and_message, input){
  p1<-plot_and_message$plot[[case_when(
    input$plot=="Line"~1,
    input$plot=="Map"~2,
    input$plot=="Function"~3
  )]]
  
  message<-plot_and_message$message[[input$message]]
  message <- strwrap(message, width = 50, simplify = FALSE) # modify 30 to your needs
  message <- sapply(message, paste, collapse = "\n")
  p2 <- text_grob(message, face = "italic", color = "steelblue", size=20) %>%
    as_ggplot()

  grid.arrange(p1,p2,
               layout_matrix=matrix(c(rep(1,4),2)))
  
}

#####

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("PhenoWatch"),
    # titlePanel(read.table("./submitted/test.txt")[1,1]),
    
    sidebarLayout(
      sidebarPanel(
      fluidRow(
     column(6,
      textInput("observer", labelMandatory("Observer"))),
       column(6,
      textInput("email", "Email"))
      ),
        # textInput("genus", labelMandatory("Genus")),
        selectInput("genus", labelMandatory("Genus"),
                    c("",genus_list)),
        textInput("species", "Species"),
        # textInput("date", labelMandatory("Date")),
        dateInput(
          "date",
          labelMandatory("Date"),
          min = "1900-01-01",
          max = Sys.Date(),
          format = "yyyy-mm-dd",
          startview = "month",
          weekstart = 0,
          language = "en",
          width = NULL,
          autoclose = TRUE,
          datesdisabled = NULL,
          daysofweekdisabled = NULL
        ),
        numericInput(
          "latitude",
          labelMandatory("Latitude"),
          value=NULL,
          min = 25,
          max = 53,
          step = NA,
          width = NULL
        ),
        numericInput(
          "longitude",
          labelMandatory("Longitude"),
          value=NULL,
          min = -125,
          max = -67,
          step = NA,
          width = NULL
        ),
        selectInput("event", labelMandatory("Phenological event"),
                    c("",
                    "Leafing",
                      "Flowering")),
        selectInput("status", labelMandatory("Phenological status"),
                    c("","Yes",  "No")),
        fluidRow(
          column(3,
                 actionButton("submit", "Submit", class = "btn-primary")),
          column(9,
                 shinyjs::hidden(
                   tags$div(id="thankyou_msg",
                            "Thanks, your response was submitted successfully!\n
                            Wait a minute for some customized flashcards."
                   )
                 )
          )
        )
      ),
      mainPanel(
        fluidRow(
          column(6,
                 selectInput("plot", "Plot",
                             c("Line",  "Map", "Function"))),
          column(6,
                 sliderInput("message", "Message", min=1, max=5, value=1, ticks=T))),
        
        plotOutput("plot", height="550px"),
        fluidRow(
        column(2,
                 actionButton("go", "Take a screenshot", class = "btn-primary")),
        column(2,
                 tags$a( href="https://twitter.com/intent/tweet?button_hashtag=phenology&ref_src=twsrc%5Etfw",
                        class="twitter-hashtag-button",
                        "data-size"="large",
                        "data-show-count"="false",
                        "Tweet #phenology"),
                        tags$script(async=NA,
                            src="https://platform.twitter.com/widgets.js",
                            charset="utf-8")),
        column (8,
        tags$div(id="cite",align="right",
                 '', tags$em('"PhenoWatch"'), 'by Yiluan Song'
        ),
        
        tags$a (id="link",target="_blank",
href="http://phenoobservers.ucsc.edu/phenoforecast/",
tags$div (
id="linktext",align="right",
                 'Visit ', tags$em('"PhenoForecast"'), ''
)
),
tags$a (id="link",target="_blank",
href="http://phenoobservers.ucsc.edu/phenoinfo/",
tags$div (
id="linktext",align="right",
                 'Visit ', tags$em('"PhenoInfo"'), ''
)
)
        
        )
        )
      )
  )
    
    # shinyjs::hidden(
    #   div(
    #     id = "thankyou_msg",
    #     h3("Thanks, your response was submitted successfully! Wait a second for some interpretation."),
    #     actionLink("submit_another", "Submit another response")
    #   )
    # ),
    
    # absolutePanel(id = "plot_controls",
    #               class = "panel panel-default",
    #               fixed = TRUE,draggable = TRUE,
    #               top = 50, right = 60, left = "auto", bottom = "auto",
    #               width = "auto", height = "auto",
    #               style = "background-color: rgba(255,255,255,0);
    #               border-color: rgba(255,255,255,0);
    #               box-shadow: 0pt 0pt 0pt 0px",
    #
    #               # numericInput(
    #               #   "window",
    #               #   "Window (day)",
    #               #   value=14,
    #               #   min = 0,
    #               #   max = 366/2,
    #               #   step = NA,
    #               #   width = NULL
    #               # ),
    #               # numericInput(
    #               #   "radius",
    #               #   "Radius (m)",
    #               #   value=500000,
    #               #   min = 100000,
    #               #   max = 1000000,
    #               #   step = NA,
    #               #   width = NULL
    #               # ),
    #               selectInput("plot", "Plot",
    #                           c("Line",  "Map", "Function")),
    #               sliderInput("message", "Message", min=1, max=5, value=0, ticks=T),
    #               # actionButton("card", "Generate cards"),
    #               actionButton("go", "Take a screenshot")
    # ),
    
  ),
  server = function(input, output, session) {
    observe({
      mandatoryFilled <-
        vapply(fieldsMandatory,
               function(x) {
                 !is.null(input[[x]]) && as.character(input[[x]]) != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      shinyjs::toggleState(id = "go", condition = mandatoryFilled)
      # shinyjs::toggleState(id = "card", condition = mandatoryFilled)
    })
    
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) as.character(input[[x]]))
      data <- c(data, timestamp = as.character(Sys.time()))
      data <- t(data)
      data
    })
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          digest::digest(formData()))
      write.csv(x = formData(), file = file.path(responsesDir, fileName),
                row.names = FALSE, quote = TRUE)
      # shinyjs::reset("form")
      # shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
    
    observeEvent(input$submit, {
      # observeEvent(input$card, {
      plot_and_message<-generate_output(input)
      # })
      
      # observeEvent(input$card, {
        output$plot <- renderPlot({
          generate_plot(plot_and_message, input)
        })
      # })
    })
    
    observeEvent(input$go, {
      fileName <- sprintf("%s_%s",
                          humanTime(),
                          digest::digest(formData()))
      shinyscreenshot::screenshot(filename=fileName)
    })
    
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
  }
)

