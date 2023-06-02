install.packages(setdiff("pacman", rownames(installed.packages())))
library(pacman)
p_load(shiny)
p_load(leaflet)
p_load(raster)
p_load(colorRamps)
p_load(tidyverse)
p_load(rgdal)
p_load(lubridate)
p_load(RhpcBLASctl)
p_load(foreach)
p_load(doSNOW)
p_load(shinyscreenshot)
p_load(digest)
p_load(shinyjs)

num_cores<-get_num_procs()-1
cl <- makeCluster(num_cores, outfile = "")
registerDoSNOW(cl)

path_app<-"/srv/shiny-server/phenoforecast/"
today<-read_file(paste0(path_app,"today.txt")) %>% as.Date()
date_list<-seq(today-years(1), today+14, by=1)

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

genusoi_list <- c(
  "Quercus", 
  "Betula",
  "Populus",
  "Acer"
)


evi_sta_list<-leaf_sta_list<-flower_sta_list<-vector(mode="list",length=length(genusoi_list))
names(evi_sta_list)<-names(leaf_sta_list)<-names(flower_sta_list)<-genusoi_list

for (i in 1:length(genusoi_list)){
  genusoi<-genusoi_list[i]
  path_evi<-paste0(path_app,"data/",genusoi,"/evi/")
  evi_files<-list.files(path_evi, full.names = T, pattern="\\.tif$") %>% sort()
  
  evi_ras_list<-
    foreach (r = 1:length(date_list),
             .packages=c("tidyverse","raster"))  %dopar%  {
               ras<-raster(evi_files[r])
               print(r)
               ras
             }
  evi_sta<-stack(evi_ras_list)
  evi_sta_list[[i]]<-evi_sta
}

for (i in 1:length(genusoi_list)){
  genusoi<-genusoi_list[i]
  path_leaf<-paste0(path_app,"data/",genusoi,"/leaf/")
  leaf_files<-list.files(path_leaf, full.names = T, pattern="\\.tif$") %>% sort()
  
  leaf_ras_list<-
    foreach (r = 1:length(date_list),
             .packages=c("tidyverse","raster"))  %dopar%  {
               ras<-raster(leaf_files[r])
               print(r)
               ras
             }
  leaf_sta<-stack(leaf_ras_list)
  leaf_sta_list[[i]]<-leaf_sta
}

for (i in 1:length(genusoi_list)){
  genusoi<-genusoi_list[i]
  path_flower<-paste0(path_app,"data/",genusoi,"/flower/")
  flower_files<-list.files(path_flower, full.names = T, pattern="\\.tif$") %>% sort()
  
  flower_ras_list<-
    foreach (r = 1:length(date_list),
             .packages=c("tidyverse","raster"))  %dopar%  {
               ras<-raster(flower_files[r])
               print(r)
               ras
             }
  flower_sta<-stack(flower_ras_list)
  flower_sta_list[[i]]<-flower_sta
}

sta_list<-list(EVI=evi_sta_list,Leaf=leaf_sta_list,Flower=flower_sta_list)

####
pal_evi<-colorNumeric(palette = "Greens",  domain = c(0,1), na.color = "transparent")
pal_leaf<-colorNumeric(palette = "Greens",  domain = c(0,1), na.color = "transparent")
pal_flower<-colorNumeric(palette = "Reds",  domain = c(0,1), na.color = "transparent")
pal<-list(EVI=pal_evi, Leaf=pal_leaf, Flower=pal_flower)

variable_list<-list(EVI="Enhanced Vegetation Index",
                    Leaf="Leafing status",
                    Flower="Flowering status")

############################
ui<-fillPage(
    shinyjs::useShinyjs(),
    #shinyjs::inlineCSS(appCSS),
  tags$style(type = "text/css", 
             "html, body {width:100%; height:100%;}"
  ),
  
  leafletOutput("raster_map", height="100%",width="100%"),
  
  absolutePanel(id = "controls", 
                class = "panel panel-default", 
                fixed = TRUE,draggable = TRUE, 
                top = 50, right = "auto", left = 60, bottom = "auto",
                width = "auto", height = "auto",
                style = "background-color: rgba(255,255,255,0);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                h1(id="title","PhenoForecast"),
                selectInput("type", "Type",
                            choices = c("EVI","Leaf", "Flower"),
                            selected =  "EVI"),
                
                selectInput("genus", "Genus",
                            choices = genusoi_list,
                            selected = genusoi_list[1]),
                
                # sliderInput("date", "Date", min=mindate, max=maxdate, timeFormat = "%Y-%m-%d", value=1, ticks=T),
                sliderInput("day", "Day", min=14-length(date_list)+1, max=14, value=0, ticks=T)
                # If not using custom CSS, set height of leafletOutput to a number instead of percent     
  ),
  
  absolutePanel(id = "figure",
                class = "panel panel-default",
                fixed = TRUE,draggable = TRUE,
                top = 60, left = "auto", right = 60, bottom = "auto",
                width = 300, height = "auto",
                style = "background-color: rgba(255,255,255,0);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                # h4("Temporal patterns"),
                plotOutput("lineplot", height = 200)
                
                
  ),
  
  
  absolutePanel(id = "tweetfeed_shown",
                class = "panel panel-default",
                fixed = TRUE,draggable = TRUE,
                top = "auto", left = 100, right = "auto", bottom = 10,
                width = 300, height = 300,
                style = "background-color: rgba(255,255,255,0);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                tags$script(src="https://apps.elfsight.com/p/platform.js",
                            defer=NA),
                # includeScript("https://apps.elfsight.com/p/platform.js"), # this causes the app to crash
                tags$div(class = "elfsight-app-ab030cd9-764d-413a-9cfa-0e630029053f"),
                actionButton("hidetweet", "Hide Twitter feed", class = "btn-primary")
                
  )
  ,
  shinyjs::hidden(
  absolutePanel(id = "tweetfeed_hidden",
                class = "panel panel-default",
                fixed = TRUE,draggable = FALSE,
                top = "auto", left = 100, right = "auto", bottom = 10,
                width = 300, height = "auto",
                style = "background-color: rgba(255,255,255,0);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                actionButton("showtweet", "Show Twitter feed", class = "btn-primary")
                
  )
  ),
  
  absolutePanel(id = "misc",
                class = "panel panel-default",
                fixed = TRUE,draggable = FALSE,
                top = "auto", left = "auto", right = 60, bottom = 30,
                width = 250, height = "auto",
                style = "background-color: rgba(255,255,255,0);
                text-align: right;
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                
                actionButton("go", "Take a screenshot", class = "btn-primary"), 
                tags$a( href="https://twitter.com/intent/tweet?button_hashtag=phenology&ref_src=twsrc%5Etfw",
                        class="twitter-hashtag-button",
                        "data-size"="large",
                        "data-show-count"="false",
                        "Tweet #phenology"),
                tags$script(async=NA,
                            src="https://platform.twitter.com/widgets.js",
                            charset="utf-8"),
                
                # includeScript("http://platform.twitter.com/widgets.js"),
                # https://shiny.rstudio.com/articles/html-tags.html
                # https://community.rstudio.com/t/include-a-button-in-a-shiny-app-to-tweet-the-url-to-the-app/8113/2
                
                tags$div(id="cite",align="right",
                         '', tags$em('"PhenoForecast"'), ' by Yiluan Song'
    ),
    tags$a (id="link",target="_blank",
href="http://phenoobservers.ucsc.edu/phenowatch/",
tags$div (
id="linktext",align="right",
                 'Visit ', tags$em('"PhenoWatch"'), ''
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
  # absolutePanel(id = "figures2", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60+280, left = "auto", right = 60, bottom = "auto",width = 300, height = "auto", 
  #               
  #               # h4("Spatial patterns"),
  #               plotOutput("neighbours",height = 360)
  # )
)


server<-function(input, output){
  output$raster_map = renderLeaflet({leaflet(width = "100%", height="100%") %>%
      addTiles()%>%
      setView(lng = -98, lat = 38, zoom = 4)})
  
  observe({
    r_type<-sta_list[[input$type,drop=F]]
    r_type_genusoi<-r_type[[input$genus]]
    r_type_genusoi_date<-r_type_genusoi[[input$day-14+length(date_list)]]
    date_label <- tags$div(
      date_list[input$day-14+length(date_list)]
    )  
    
    reactiveRaster <- reactive({r_type_genusoi_date})
    leafletProxy("raster_map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(reactiveRaster(),colors = pal[[input$type]], opacity = 0.8, layerId = "map")%>%
      addLegend(pal =  pal[[input$type]], values = seq(0,1,by=0.1),
                position = "bottomleft",title = "",#variable_list[[input$type]],
                layerId = "map"
      ) %>% 
      addControl(date_label, position = "bottomleft") # %>% 
      # addControl(site_label, position = "bottomright")
  })
  
  #Show popup on click
  observeEvent(input$raster_map_click, {
    r_type<-sta_list[[input$type,drop=F]]
    r_type_genusoi<-r_type[[input$genus]]
    r_type_genusoi_date<-r_type_genusoi[[input$day-14+length(date_list)]]
    variable<-variable_list[[input$type]]
    
    click <- input$raster_map_click
    lat<-(90+click$lat)%%180-90
    lng<-(180+click$lng)%%360-180
    text_lat<-paste0("Latitude: ", round(lat,2))
    text_lng<-paste0("Longtitude: ", round(lng,2))
    text_date<-paste0("Date: ", date_list[[input$day-14+length(date_list)]])
    value<-round(raster::extract(r_type_genusoi_date,data.frame(lng,lat)),2)
    text_value<-paste0(variable,": ", value,"")
    
    content <- as.character(tagList(
      text_lat, tags$br(),
      text_lng, tags$br(),
      text_date, tags$br(),
      text_value, tags$br()
    ))
    
    leafletProxy("raster_map") %>%
      clearPopups() %>%
      addPopups(click$lng, click$lat, content)
  })
  
  #Lineplot
  observeEvent(input$raster_map_click, {
    r_type<-sta_list[[input$type,drop=F]]
    r_type_genusoi<-r_type[[input$genus]]
    # r_type_genusoi_date<-r_type_genusoi[[input$day-14+length(date_list)]]
    variable<-variable_list[[input$type]]
    if(input$type=="EVI") {
      col_line<-"dark green"
    }
    if(input$type=="Leaf") {
      col_line<-"dark green"
    }
    if (input$type=="Flower") {
      col_line<-"red"
    }
    
    click <- input$raster_map_click
    lat<-(90+click$lat)%%180-90
    lng<-(180+click$lng)%%360-180
    
    sp<-SpatialPoints(cbind(lng, lat),proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
    
    ts<-
      foreach(i = 1:length(date_list),
              .packages=(c("raster","tidyverse")),
              .combine="rbind") %dopar% {
                value<-raster::extract(r_type_genusoi[[i]], sp )
                value
              }
    
    ts_df<-data.frame(ts, date_list)
    colnames(ts_df)<-c("value", "date")
    if (nrow (ts_df)>0) {
      output$lineplot <- renderPlot({
        ggplot(ts_df)+
          geom_line(aes(x=date, y=value),col=col_line)+
          geom_vline(aes(xintercept=date_list[input$day-14+length(date_list)]))+
          geom_vline(aes(xintercept=date_list[0-14+length(date_list)]), alpha=0.5)+
          # geom_smooth(aes(x=date, y=value))+
          theme_light()+
          ylim(-0.1,1.1)+
          xlab("date")+
          ylab(variable)+
          ggtitle(paste0("Longitude: ", round(lng,2), ", Latitude: ", round(lat,2)))
      })
    } else {
      output$lineplot <- renderPlot({
        ggplot()+
          theme_void ()+
          ggtitle("\n No time series available.\n Contribute by submitting your data.")
      })
    }
    
  })
  
  observeEvent(input$showtweet, {
      shinyjs::hide("tweetfeed_hidden")
      shinyjs::show("tweetfeed_shown")
    })
    
    observeEvent(input$hidetweet, {
    shinyjs::hide("tweetfeed_shown")
      shinyjs::show("tweetfeed_hidden")
    })
    
  formData <- reactive({
      data <- c(input$type, input$genus, input$day,as.character(Sys.time()))
      data
    })
    
  observeEvent(input$go, {
      fileName <- sprintf("%s_%s",
                          humanTime(),
                          digest::digest(formData()))
      shinyscreenshot::screenshot(filename=fileName)
    })
}


shinyApp(ui, server)#, options = list(height=600,width=1200)
