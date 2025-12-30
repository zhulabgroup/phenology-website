library(leaflet)
# library(terra)
# library(raster)
library(tidyverse)
library(doSNOW)
library(parallel)


path_app <- getwd()
today <- read_file(str_c(path_app, "/today.txt")) %>% as.Date()
date_list <- seq(today - 14, today + 14, by = 1) # today-years(1)

cl <- makeCluster(16, outfile = "")
registerDoSNOW(cl)
bucket_name <- "phenoobservers"
bucket_region <- "us-east-2"
source("copyfiles.R")

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

genusoi_list <- c(
  "Quercus",
  "Betula",
  "Populus",
  "Acer"
)

####
pal_evi <- leaflet::colorNumeric(palette = "Greens", domain = c(0, 1), na.color = "transparent")
pal_leaf <- leaflet::colorNumeric(palette = "Greens", domain = c(0, 1), na.color = "transparent")
pal_flower <- leaflet::colorNumeric(palette = "Reds", domain = c(0, 1), na.color = "transparent")
pal_pollen <- leaflet::colorNumeric(palette = "Reds", domain = c(0, 5), na.color = "transparent")
pal <- list(EVI = pal_evi, Leaf = pal_leaf, Flower = pal_flower, Pollen = pal_pollen)
maxlist <- list(EVI = 1.0, Leaf = 1.0, Flower = 1.0, Pollen = 5.0)
minlist <- list(EVI = 0, Leaf = 0, Flower = 0, Pollen = 0)

variable_list <- list(
  EVI = "Enhanced Vegetation Index",
  Leaf = "Leafing status",
  Flower = "Flowering status",
  Pollen = "Pollen concentration (grains/m^3) fourth root"
)

########### UI###########
ui <- fillPage(
  shinyjs::useShinyjs(),
  # shinyjs::inlineCSS(appCSS),
  # tags$head(
  #   # include html2canvas library for custom javascript -- screenshotting
  #   tags$script(src = "http://html2canvas.hertzen.com/dist/html2canvas.min.js"),
  #   # script for creating the prompt for download
  #   tags$script(
  #           "function saveAs(uri, filename) {
  #               var link = document.createElement('a');
  #               if (typeof link.download === 'string') {
  #                   link.href = uri;
  #                   link.download = filename;
  #                   document.body.appendChild(link);    //Firefox requires the link to be in the body
  #                   link.click();                       //simulate click
  #                   document.body.removeChild(link);    //remove the link when done
  #               } else {
  #                   window.open(uri);
  #               }
  #           }"
  #   )
  # ),

  tags$style(
    type = "text/css",
    "html, body {width:100%; height:100%;}"
  ),
  leaflet::leafletOutput("raster_map", height = "100%", width = "100%"),
  absolutePanel(
    id = "controls",
    class = "panel panel-default",
    fixed = TRUE, draggable = TRUE,
    top = 50, right = "auto", left = 60, bottom = "auto",
    width = "auto", height = "auto",
    style = "background-color: rgba(255,255,255,0);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
    h1(id = "title", "PhenoForecast"),
    selectInput("type", "Type",
      choices = c("EVI", "Leaf", "Flower", "Pollen"),
      selected = "Pollen"
    ),
    selectInput("genus", "Genus",
      choices = genusoi_list,
      selected = genusoi_list[1]
    ),

    # sliderInput("date", "Date", min=mindate, max=maxdate, timeFormat = "%Y-%m-%d", value=1, ticks=T),
    sliderInput("day", "Day", min = 14 - length(date_list) + 1, max = 14, value = 0, ticks = T)
    # If not using custom CSS, set height of leafletOutput to a number instead of percent
  ),

  # absolutePanel(id = "figure",
  #               class = "panel panel-default",
  #               fixed = TRUE,draggable = TRUE,
  #               top = 60, left = "auto", right = 60, bottom = "auto",
  #               width = 300, height = "auto",
  #               style = "background-color: rgba(255,255,255,0);
  #               border-color: rgba(255,255,255,0);
  #               box-shadow: 0pt 0pt 0pt 0px",
  #
  #               # h4("Temporal patterns"),
  #               plotOutput("lineplot", height = 200)
  #
  #
  # ),


  # absolutePanel(id = "tweetfeed_shown",
  #               class = "panel panel-default",
  #               fixed = TRUE,draggable = TRUE,
  #               top = "auto", left = 100, right = "auto", bottom = 10,
  #               width = 300, height = 300,
  #               style = "background-color: rgba(255,255,255,0);
  #               border-color: rgba(255,255,255,0);
  #               box-shadow: 0pt 0pt 0pt 0px",
  #
  #               tags$script(src="https://apps.elfsight.com/p/platform.js",
  #                           defer=NA),
  #               # includeScript("https://apps.elfsight.com/p/platform.js"), # this causes the app to crash
  #               tags$div(class = "elfsight-app-ab030cd9-764d-413a-9cfa-0e630029053f"),
  #               actionButton("hidetweet", "Hide Twitter feed", class = "btn-primary")
  #
  # )
  # ,
  # shinyjs::hidden(
  #   absolutePanel(id = "tweetfeed_hidden",
  #                 class = "panel panel-default",
  #                 fixed = TRUE,draggable = FALSE,
  #                 top = "auto", left = 100, right = "auto", bottom = 10,
  #                 width = 300, height = "auto",
  #                 style = "background-color: rgba(255,255,255,0);
  #               border-color: rgba(255,255,255,0);
  #               box-shadow: 0pt 0pt 0pt 0px",
  #
  #                 actionButton("showtweet", "Show Twitter feed", class = "btn-primary")
  #
  #   )
  # ),

  absolutePanel(
    id = "misc",
    class = "panel panel-default",
    fixed = TRUE, draggable = FALSE,
    top = "auto", left = "auto", right = 60, bottom = 30,
    width = 250, height = "auto",
    style = "background-color: rgba(255,255,255,0);
                text-align: right;
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",


    # actionButton("screenshot","Take Screenshot"),                         # uses html2canvas JS library
    downloadButton("map_down", "Take a screenshot", class = "dwnbttn"), # uses mapshot for screenshot
    tags$head(tags$style(".dwnbttn{background-color:#337ab8; color: #ffffff;} .dwnbttn:focus{background-color:#337ab8; color: #ffffff;}")),
    br(),
    tags$a(
      href = "https://twitter.com/intent/tweet?button_hashtag=phenology&ref_src=twsrc%5Etfw",
      class = "twitter-hashtag-button",
      "data-size" = "large",
      "data-show-count" = "false",
      "Tweet #phenology"
    ),
    tags$script(
      async = NA,
      src = "https://platform.twitter.com/widgets.js",
      charset = "utf-8"
    ),

    # includeScript("http://platform.twitter.com/widgets.js"),
    # https://shiny.rstudio.com/articles/html-tags.html
    # https://community.rstudio.com/t/include-a-button-in-a-shiny-app-to-tweet-the-url-to-the-app/8113/2

    tags$div(
      id = "cite", align = "right",
      "", tags$em('"PhenoForecast"'), " by Yiluan Song"
    ),
    tags$div(
      id = "cite", align = "right",
      "", "Modified by Jessica Pan"
    ),
    tags$a(
      id = "link", target = "_blank",
      href = "http://phenoobservers.ucsc.edu/phenowatch/",
      tags$div(
        id = "linktext", align = "right",
        "Visit ", tags$em('"PhenoWatch"'), ""
      )
    ),
    tags$a(
      id = "link", target = "_blank",
      href = "http://phenoobservers.ucsc.edu/phenoinfo/",
      tags$div(
        id = "linktext", align = "right",
        "Visit ", tags$em('"PhenoInfo"'), ""
      )
    )
  )
)

####### SERVER#######
server <- function(input, output, session) {
  mymap <- reactive({
    m <- leaflet() %>%
      addTiles() %>%
      leaflet::setView(lng = -98, lat = 38, zoom = 4)
    m
  })

  output$raster_map <- leaflet::renderLeaflet({
    mymap()
  })

  myfun <- function(raster_map) {
    input_type <- input$type
    date_label <- tags$div(date_list[input$day - 14 + length(date_list)])

    new_rast <- reactiveRaster()
    clearImages(raster_map) %>%
      clearControls() %>%
      addRasterImage(new_rast, colors = pal[[input_type]], opacity = 0.8, layerId = "map") %>%
      addLegend(
        pal = pal[[input_type]], values = seq(minlist[[input_type]], maxlist[[input_type]], length.out = 6),
        position = "bottomleft", title = "", layerId = "map"
      ) %>%
      addControl(date_label, position = "bottomleft")
  }

  reactiveRaster <- reactive({
    ras_sta <- reactiveInput()$ras_sta

    ras_date <- ras_sta[[input$day - 14 + length(date_list)]]
    ras_date_lim <- raster::raster(ras_date)
  })

  reactiveInput <- reactive({
    path_local_data <- copyfiles(genusoi = input$genus, varoi = input$type %>% tolower(), bucket_name = bucket_name, bucket_region = bucket_region, date_list = date_list)
    ras_sta <- terra::rast(list.files(path_local_data, full.names = T))

    if (input$type == "Pollen") {
      ras_sta[ras_sta < 0] <- 0
      ras_sta <- ras_sta^(1 / 4)
    }
    ras_sta_lim <- ras_sta
    ras_sta_lim[ras_sta_lim > maxlist[[input$type]]] <- maxlist[[input$type]] - 1e-5
    ras_sta_lim[ras_sta_lim < minlist[[input$type]]] <- minlist[[input$type]] + 1e-5
    out <- list(ras_sta = ras_sta_lim, path = path_local_data)
  })

  observe({
    leafletProxy("raster_map") %>% myfun()
  })


  ## To hold the popup locations
  v <- reactiveValues()
  v$point <- NULL

  ####### Show popup on click########
  getPop <- reactive({
    ras_sta <- reactiveInput()$ras_sta
    ras_date <- ras_sta[[input$day - 14 + length(date_list)]]
    print(ras_date)

    click <- input$raster_map_click
    lat <- (90 + click$lat) %% 180 - 90
    lng <- (180 + click$lng) %% 360 - 180
    text_lat <- paste0("Latitude: ", round(lat, 2))
    text_lng <- paste0("Longtitude: ", round(lng, 2))
    text_date <- paste0("Date: ", date_list[[input$day - 14 + length(date_list)]])
    value <- round(terra::extract(ras_date, data.frame(lng, lat)), 2)
    text_value <- str_c(variable_list[[input$type]], ": ", value[2], "")

    content <- paste(text_lat, text_lng, text_date, text_value, sep = "<br/>")


    p <- data.frame(lng = click$lng, lat = click$lat)
    v$point <- NULL
    v$point <- rbind(v$point, p)

    output <- content
  })

  popups <- function(raster_map) {
    content <- getPop()
    lng <- lat <- NULL

    if (!is.null(v$point)) {
      lng <- v$point[, 1]
      lat <- v$point[, 2]
    }

    clearPopups(raster_map) %>%
      addPopups(lng, lat, content)
  }

  ######## Show lineplot on click#########
  getLinePlot <- reactive({
    ras_sta <- reactiveInput()$ras_sta
    print(ras_sta)

    if (input$type == "EVI" || input$type == "Leaf") {
      col_line <- "dark green"
    }
    if (input$type == "Flower" || input$type == "Pollen") {
      col_line <- "red"
    }

    click <- input$raster_map_click
    lat <- (90 + click$lat) %% 180 - 90
    lng <- (180 + click$lng) %% 360 - 180

    y <- terra::vect(cbind(lng, lat), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

    ts <- terra::extract(x = ras_sta, y = y)
    ts <- select(as.data.frame(ts), -1)
    colnames(ts) <- c(1:length(ts[1, ]))
    ts <- as.data.frame(t(ts))

    ts_df <- data.frame(ts, date_list)
    colnames(ts_df) <- c("value", "date")

    plot <- NULL

    if (!any(is.na(ts_df))) {
      plot <- ggplot(ts_df) +
        geom_line(aes(x = date, y = value), col = col_line) +
        geom_vline(aes(xintercept = date_list[input$day - 14 + length(date_list)])) +
        geom_vline(aes(xintercept = date_list[0 - 14 + length(date_list)]), alpha = 0.5) +
        # geom_smooth(aes(x=date, y=value))+
        theme_light() +
        ylim(minlist[[input$type]] - 0.1, maxlist[[input$type]] + 0.1) +
        xlab("date") +
        ylab(variable_list[[input$type]]) +
        ggtitle(paste0("Longitude: ", round(lng, 2), ", Latitude: ", round(lat, 2))) +
        theme(plot.title = element_text(size = 10))
    } else {
      plot <- ggplot() +
        theme_void() +
        ggtitle("\n No time series available.\n Contribute by submitting your data.")
    }


    return(plot)
  })

  createLinePlot <- function(raster_map, plotty) {
    ggsave(file = "plotty.svg", plot = plotty, width = 3.5, height = 2.5)
    content <- as.character(read_file(paste0("plotty.svg")))

    removeControl(raster_map, layerId = "lineplot") %>%
      addControl(content, position = "topright", layerId = "lineplot", className = "fieldset { border: 0;}")
  }

  ##### generate the popup and lineplot when user clicks map
  observeEvent(input$raster_map_click, {
    leafletProxy("raster_map") %>%
      popups() %>%
      createLinePlot(getLinePlot())
  })

  # update the lineplot if the user changes the input/form data -- this uses the last point clicked on the map
  observeEvent(formData(), {
    unlink(reactiveInput()$path, recursive = T)
    # if (!is.null(v$point)){
    #   leafletProxy("raster_map") %>%
    #     createLinePlot(getLinePlot())
    # }
  })

  ## add input panel and misc elements to mapshot
  getinputs_misc <- function(raster_map) {
    data <- formData()
    date <- as.numeric(data[3])
    inputs <- paste(
      "<h1><b>PhenoForecast</b></h1>",
      "<p><b>Type: </b>", data[1],
      "<br/><br/><b>Genus: </b>", data[2],
      "<br/><br/><b>Date: </b>", date_list[date - 14 + length(date_list)],
      "</p>"
    )

    misc <- paste("<p style=\"text-align: right; color: blue;\">Tweet #phenology",
      "<span style=\"color: black;\">\"PhenoForecast\" by Yiluan Song<br/> Modified by Jessica Pan </span>",
      "Visit \"PhenoWatch\"",
      "Visit \"PhenoInfo\"</p>",
      sep = "<br/>"
    )

    addControl(raster_map, inputs, position = "topleft", className = "fieldset { border: 0;}") %>%
      addControl(misc, position = "bottomright", className = "fieldset { border: 0;}")
  }

  ######## show tweeet########
  observeEvent(input$showtweet, {
    shinyjs::hide("tweetfeed_hidden")
    shinyjs::show("tweetfeed_shown")
  })

  ######## hide tweeet ########
  observeEvent(input$hidetweet, {
    shinyjs::hide("tweetfeed_shown")
    shinyjs::show("tweetfeed_hidden")
  })

  ####### form data#########
  formData <- reactive({
    data <- c(input$type, input$genus, input$day, as.character(Sys.time()))
    data
  })

  ########## screenshoting########
  user_created_map <- reactive({
    m <- mymap() %>%
      leaflet::setView(
        lng = input$raster_map_center$lng, lat = input$raster_map_center$lat,
        zoom = input$raster_map_zoom
      ) %>%
      myfun()

    if (!is.null(v$point)) {
      m <- m %>%
        popups() %>%
        createLinePlot(getLinePlot())
    }
    m <- m %>% getinputs_misc()

    m
  })

  output$map_down <- downloadHandler(
    filename = "mymap.png",
    content = function(file) {
      on.exit(getwd())
      mapshot(user_created_map(), file = file, cliprect = "viewport")
    }
  )

  ### for html2canvas screenshoting -- DEPRICATED -- doesn't capture underlying map
  # observeEvent(input$screenshot,{
  #   shinyjs::runjs(
  #     'html2canvas(document.querySelector("body")).then(canvas => {
  #               saveAs(canvas.toDataURL(), "shinyapp.png");
  #          });'
  #   )
  # })
}


shinyApp(ui, server) # , options = list(height=600,width=1200)
