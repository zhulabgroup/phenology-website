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
library(aws.s3)
library(ggnewscale)

path_app <- getwd()
data_path <- str_c(path_app, "/NPN_example/")
today <- read_file(str_c(path_app, "/today.txt")) %>% as.Date()

Sys.setenv(
  "AWS_DEFAULT_REGION" = "us-east-2",
  "AWS_S3_ENDPOINT" = "s3.amazonaws.com"
)

bucket_name <- "phenoobservers"
download_folder_path <- "PhenoWatch/NPN/"
submit_folder_path <- "PhenoWatch/submitted/"

upload_to_s3 <- function(file_path) {
  filename <- basename(file_path)
  s3_key <- paste0(submit_folder_path, filename)
  put_object(
    file = file_path, bucket = bucket_name, object = s3_key,
    headers = list(`x-amz-acl` = "bucket-owner-full-control")
  )
}

# # test
# upload_to_s3("today.txt")

calc_range <- function(df) {
  non_na_indices <- which(!is.na(df$intensity))
  flag <- F

  first_instance <- -1
  last_instance <- -1

  for (j in 1:(length(non_na_indices) - 1)) {
    current_index <- non_na_indices[j]
    next_index <- non_na_indices[j + 1]

    if (df$intensity[current_index] <= 0.25 && df$intensity[next_index] >= 0.25) {
      if (flag == F) {
        first_instance <- current_index
        flag <- T
      }
    }
    if (df$intensity[current_index] >= 0.25 && df$intensity[next_index] <= 0.25) {
      last_instance <- current_index
    }
  }
  return(c(first_instance, last_instance))
}

genusoi_list <- c(
  "Acer",
  "Quercus",
  "Betula",
  "Populus"
)

fieldsMandatory <- c(
  "observer", "genus",
  "date", "latitude", "longitude",
  "event", "status"
)

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

fieldsAll <- c(
  "observer", "genus", "species",
  "date", "latitude", "longitude",
  "event", "status", "email"
)


epochTime <- function() {
  as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")



#####

generate_output <- function(input, window = 14, radius = 100000) {
  data_path_subset <- paste0(
    download_folder_path,
    ifelse(input$event == "Leafing", "leaf", "flower"),
    "/",
    input$genus
  )
  npn_files <- aws.s3::get_bucket(bucket = bucket_name, prefix = data_path_subset)

  if (length(npn_files) > 0) {
    npn_data_all <- vector(mode = "list")

    for (i in seq_along(npn_files)) {
      file_key <- npn_files[[i]]$Key
      csv_data <- aws.s3::s3read_using(readr::read_csv, object = file_key, bucket = bucket_name)
      npn_data_all[[i]] <- csv_data %>%
        mutate(
          `intensity_value` = as.character(`intensity_value`),
          update_datetime = as.character(update_datetime)
        )
    }

    npn_data_all <- bind_rows(npn_data_all) %>%
      dplyr::select(site_id, latitude, longitude, observation_date, day_of_year, phenophase_status) %>%
      filter(phenophase_status != -1) %>%
      mutate(year = as.integer(format(observation_date, "%Y"))) %>%
      filter(
        longitude >= -125,
        longitude <= -67,
        latitude >= 25,
        latitude <= 53
      )
  } else {
    npn_data_all <- data.frame(
      site_id = double(0),
      latitude = double(0),
      longitude = double(0),
      observation_date = as.Date(character(0)),
      day_of_year = double(0),
      phenophase_status = double(0),
      year = integer(0)
    )
  }

  # radius<-500000
  if (nrow(npn_data_all) > 1) {
    npn_location <- filter(
      npn_data_all, abs(latitude - input$latitude) <= radius / 100000,
      abs(longitude - input$longitude) <= radius / 100000
    )
    if (nrow(npn_location) == 0) {
      img <- jpeg::readJPEG("question.jpeg")
      p_line <- ggplot() +
        background_image(img) +
        coord_equal()
      plot_and_message <- list(
        plot = list(p_line, p_line, p_line, p_line),
        message = list("There are not enough nearby observations for comparison. Choose a different location or increase the radius instead.")
      )

      return(plot_and_message)
    }

    npn_location <- npn_location %>%
      rowwise() %>%
      mutate(distance = geosphere::distm(x = c(longitude, latitude), y = c(input$longitude, input$latitude), fun = geosphere::distGeo) %>% as.numeric()) %>%
      arrange(distance) %>%
      filter(distance <= radius)

    if (nrow(npn_location) == 0) {
      img <- jpeg::readJPEG("question.jpeg")
      p_line <- ggplot() +
        background_image(img) +
        coord_equal()
      plot_and_message <- list(
        plot = list(p_line, p_line, p_line, p_line),
        message = list("There are not enough nearby observations for comparison. Choose a different location or increase the radius instead.")
      )

      return(plot_and_message)
    }
  } else {
    npn_location <- npn_data_all
  }
  year_data <- list()
  past_year <- as.integer(format(input$date, "%Y")) - 11
  current_year <- as.integer(format(input$date, "%Y")) - 1

  if (nrow(npn_location) > 0) {
    npn_location_ts <- npn_location %>%
      dplyr::select(day_of_year, phenophase_status) %>%
      group_by(day_of_year) %>%
      summarize(intensity = mean(phenophase_status)) %>%
      ungroup() %>%
      complete(day_of_year = 1:366, fill = list(intensity = NA))
    min_id <- min(which(!is.na(npn_location_ts$intensity)))
    max_id <- max(which(!is.na(npn_location_ts$intensity)))
    npn_location_ts$intensity[min_id:max_id] <-
      zoo::na.approx(
        object = npn_location_ts$intensity[min_id:max_id],
        x = min_id:max_id, maxgap = 28
      )


    max_id <- 0
    done <- F
    while (!done) {
      min_id <- min(which(!is.na(npn_location_ts$intensity[(max_id + 1):length(npn_location_ts$intensity)]))) + (max_id)
      if (min_id == Inf) {
        done <- T
      } else {
        max_id <- min(which(is.na(npn_location_ts$intensity[min_id:length(npn_location_ts$intensity)]))) - 1 + (min_id - 1)
        if (max_id == Inf) {
          max_id <- length(npn_location_ts$intensity)
          done <- T
        }
        npn_location_ts$intensity[min_id:max_id] <- ptw::whit1(npn_location_ts$intensity[min_id:max_id], 10)
      }
    }

    overall_data <- npn_location_ts

    for (i in 0:10) {
      npn_location_ts <- filter(npn_location, year == past_year + i) %>%
        dplyr::select(day_of_year, phenophase_status) %>%
        group_by(day_of_year) %>%
        summarize(intensity = mean(phenophase_status)) %>%
        ungroup() %>%
        complete(day_of_year = 1:366, fill = list(intensity = NA))

      min_id <- min(which(!is.na(npn_location_ts$intensity)))
      max_id <- max(which(!is.na(npn_location_ts$intensity)))
      if (length(unique(npn_location_ts$intensity)) == 1) {
        year_data[[i + 1]] <- data.frame(thing = c("this"))
      } else {
        npn_location_ts$intensity[min_id:max_id] <-
          zoo::na.approx(
            object = npn_location_ts$intensity[min_id:max_id],
            x = min_id:max_id, maxgap = 28
          )
        max_id <- 0
        done <- F
        while (!done) {
          min_id <- min(which(!is.na(npn_location_ts$intensity[(max_id + 1):length(npn_location_ts$intensity)]))) + (max_id)
          if (min_id == Inf) {
            done <- T
          } else {
            max_id <- min(which(is.na(npn_location_ts$intensity[min_id:length(npn_location_ts$intensity)]))) - 1 + (min_id - 1)
            if (max_id == Inf) {
              max_id <- length(npn_location_ts$intensity)
              done <- T
            }
            npn_location_ts$intensity[min_id:max_id] <- ptw::whit1(npn_location_ts$intensity[min_id:max_id], 10)
          }
        }

        year_data[[i + 1]] <- data.frame(npn_location_ts)
      }
    }
    if (TRUE) {
      rect_data <- data.frame(
        xmin = numeric(),
        xmax = numeric(),
        ymin = numeric(),
        ymax = numeric()
      )
      for (i in 0:10) {
        if (length(which(!is.na(year_data[[i + 1]]$intensity))) > 4) {
          values <- calc_range(year_data[[i + 1]])
          first_instance <- values[1]
          last_instance <- values[2]
          if (first_instance != -1 && last_instance != -1) {
            rect_data <- rbind(rect_data, data.frame(
              xmin = past_year + i - 0.3,
              xmax = past_year + i + 0.3,
              ymin = first_instance,
              ymax = last_instance
            ))
          }
        } else {
          message <- "Years not represented on the figure have insufficient data"
        }
      }
      ylim_max <- if (nrow(rect_data) > 0) max(rect_data$ymax) + 25 else 366
      rect_graph <- ggplot() +
        geom_rect(data = rect_data, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), color = "black", fill = "white", size = 0.7) +
        scale_x_continuous(breaks = seq(past_year, current_year), limits = c(past_year - 0.3, current_year + 0.3)) +
        scale_y_continuous(breaks = seq(0, ylim_max, by = 20), limits = c(0, ylim_max)) +
        labs(
          x = "Year",
          y = "Day of Year"
        ) +
        theme_minimal()

      past_year <- as.factor(past_year)
      current_year <- as.factor(current_year)

      c_line <- ggplot() +
        geom_bin2d(data = npn_location, aes(x = day_of_year, y = phenophase_status), bins = c(366, 20), alpha = 0.8) +
        geom_line(data = year_data[[1]], aes_string(x = "day_of_year", y = "intensity", color = past_year), lwd = 2, na.rm = T) +
        geom_line(data = year_data[[length(year_data)]], aes_string(x = "day_of_year", y = "intensity", color = current_year), lwd = 2, na.rm = T) +
        geom_point(aes(x = as.integer(format(input$date, "%j")), y = as.integer(input$status == "Yes")), col = "red", cex = 5) +
        ylim(0 - 0.1, 1 + 0.1) +
        labs(
          x = "day of year",
          y = "status",
          fill = "count"
        ) +
        theme_minimal()
      overall_data$intensity <- overall_data$intensity * 100
      overall_data$date <- as.Date(overall_data$day_of_year, origin = "2023-12-31")
      p_line <- ggplot() +
        geom_tile(
          data = npn_location %>% filter(phenophase_status == 1),
          aes(x = day_of_year, y = phenophase_status * 100, fill = ..count..),
          stat = "bin2d", bins = c(366, 20), alpha = 1
        ) +
        scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Frequency of yes") +
        new_scale_fill() +
        geom_tile(
          data = npn_location %>% filter(phenophase_status == 0),
          aes(x = day_of_year, y = phenophase_status, fill = ..count..),
          stat = "bin2d", bins = c(366, 20), alpha = 1
        ) +
        scale_fill_gradient(low = "#FFF700", high = "#F4C430", name = "Frequency of no") +
        geom_line(data = overall_data, aes(x = day_of_year, y = intensity), col = "blue", lwd = 2) +
        geom_point(aes(x = as.integer(format(input$date, "%j")), y = as.integer(input$status == "Yes") * 100), col = "#ff0000", cex = 5) +
        geom_vline(xintercept = as.integer(format(input$date, "%j")), col = "#ff0000", lwd = 1.5, linetype = "dashed") +
        ylim(-10, 110) +
        labs(
          x = "Day of Year",
          y = "% Yes Status",
          fill = "Count"
        ) +
        theme_minimal()
    } else {
      p_line <- ggplot() +
        geom_jitter(data = npn_location, aes(x = day_of_year, y = phenophase_status), width = 0, height = 0.05, alpha = 0.8) +
        geom_line(data = npn_location_ts, aes(x = day_of_year, y = intensity), col = "blue", lwd = 2) +
        geom_point(aes(x = as.integer(format(input$date, "%j")), y = as.integer(input$status == "Yes")), col = "red", cex = 5) +
        ylim(0 - 0.1, 1 + 0.1) +
        # scale_color_viridis_c()+
        labs(
          x = "day of year",
          y = "status"
        ) +
        theme_minimal()
    }
  } else {
    img <- jpeg::readJPEG("question.jpeg")
    p_line <- ggplot() +
      background_image(img) +
      coord_equal()
    c_line <- ggplot() +
      background_image(img) +
      coord_equal()
    rect_graph <- ggplot() +
      background_image(img) +
      coord_equal()
  }


  #####
  # window<-14
  npn_time <- npn_data_all %>%
    filter(abs(observation_date - input$date) <= window) %>%
    arrange(day_of_year)

  if (nrow(npn_time) > 0) {
    npn_time_surface <- npn_time %>%
      group_by(longitude, latitude) %>%
      summarize(intensity = mean(phenophase_status)) %>%
      ungroup()

    npn_time_sp <- SpatialPointsDataFrame(
      coords = npn_time_surface[, c("longitude", "latitude")],
      data = npn_time_surface[, c("intensity"), drop = F],
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )

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
      p_map <- ggplot() +
        # geom_jitter(data=npn_time_surface, aes(x=longitude, y=latitude, col=intensity),width=0.05, height=0.05, alpha=0.5)+
        # geom_tile(data=kriged_res, aes(x=x, y=y, fill=var1.pred))+
        geom_polygon(data = us, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
        geom_jitter(data = npn_time, aes(x = longitude, y = latitude, fill = phenophase_status), pch = 21, width = 0.05, height = 0.05, alpha = 0.5, cex = 2) +
        geom_point(aes(x = input$longitude, y = input$latitude, fill = as.integer(input$status == "Yes")), pch = 21, col = "red", cex = 5, alpha = 0.5) +
        scale_color_viridis_c(limits = c(0, 1)) +
        scale_fill_viridis_c(limits = c(0, 1)) +
        # scale_color_gradient(limits=c(0,1)) +
        # scale_fill_gradient(limits=c(0,1)) +
        labs(
          x = "longitude",
          y = "latitude",
          fill = "status"
        )
    } else {
      us <- map_data("state")
      p_map <- ggplot() +
        # geom_jitter(data=npn_time_surface, aes(x=longitude, y=latitude, col=intensity),width=0.05, height=0.05, alpha=0.5)+
        geom_polygon(data = us, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
        geom_jitter(data = npn_time, aes(x = longitude, y = latitude, fill = phenophase_status), pch = 21, width = 0.05, height = 0.05, alpha = 0.5, cex = 2) +
        geom_point(aes(x = input$longitude, y = input$latitude, fill = as.integer(input$status == "Yes")), pch = 21, col = "red", cex = 5, alpha = 0.5) +
        scale_color_viridis_c(limits = c(0, 1)) +
        scale_fill_viridis_c(limits = c(0, 1)) +
        # scale_color_gradient(limits=c(0,1)) +
        # scale_fill_gradient(limits=c(0,1)) +
        labs(
          x = "longitude",
          y = "latitude",
          fill = "status"
        ) +
        theme_minimal()
    }
  } else {
    img <- jpeg::readJPEG("question.jpeg")
    p_map <- ggplot() +
      background_image(img) +
      coord_equal()
  }

  message_location <- paste0("You just provided the #", format(nrow(npn_location) + 1, scientific = F), " phenological record of this genus within ", format(radius / 1000, scientific = F), " km distance.")

  message_time <- paste0("You just provided the #", format(nrow(npn_time) + 1, scientific = F), " phenological record of this genus within ", format(window, scientific = F), " days.")

  if (nrow(npn_location) > 1) {
    his_est <- npn_location_ts %>%
      filter(day_of_year == as.integer(format(input$date, "%j"))) %>%
      dplyr::select(intensity) %>%
      unlist()

    if (is.na(his_est)) {
      message_anomaly <- paste0(
        "There is no sufficient data to estimate the ",
        input$genus,
        case_when(
          input$event == "Leafing" ~ " leafing",
          input$event == "Flowering" ~ " flowering"
        ),
        " status for this area and time of year. Your record provides a starting point."
      )
      message_anomaly_ann <- paste0(
        "There is no sufficient data to estimate the timing of ",
        input$genus,
        case_when(
          input$event == "Leafing" ~ " leafing",
          input$event == "Flowering" ~ " flowering"
        ),
        " season for this area. Your record provides a starting point."
      )
    } else {
      his_slope <- npn_location_ts %>%
        filter(min(abs(day_of_year - as.integer(format(input$date, "%j"))), 365.25 - abs(day_of_year - as.integer(format(input$date, "%j")))) <= 15) %>%
        mutate(
          left = day_of_year - as.integer(format(input$date, "%j")),
          right = day_of_year - as.integer(format(input$date, "%j")) - 365.25
        ) %>%
        mutate(distance = case_when(
          abs(left) <= abs(right) ~ left,
          abs(left) > abs(right) ~ right
        )) %>%
        do(broom::tidy(lm(intensity ~ distance, .))) %>%
        filter(term == "distance") %>%
        dplyr::select(estimate, p.value)
      npn_location_ts %>% filter(day_of_year == as.integer(format(input$date, "%j")))
    }
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
  plot_and_message <- list(
    plot = list(p_line, p_map, c_line, rect_graph),
    message = "Years not represented on the figure have insufficient data"
  )

  return(plot_and_message)
}

generate_plot <- function(plot_and_message, input) {
  p1 <- plot_and_message$plot[[case_when(
    input$plot == "Line" ~ 1,
    input$plot == "Map" ~ 2,
    input$plot == "Trends Between Years" ~ 3,
    input$plot == "Boxplot" ~ 4
    # input$plot=="Function"~3
  )]]

  # message <- plot_and_message$message[[input$message]]
  # message <- strwrap(message, width = 50, simplify = FALSE) # modify 30 to your needs
  # message <- sapply(message, paste, collapse = "\n")
  p2 <- text_grob(plot_and_message$message[1], face = "italic", color = "steelblue", size = 20) %>%
    as_ggplot()

  grid.arrange(p1, p2,
    layout_matrix = matrix(c(rep(1, 4), 2))
  )
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
          column(
            6,
            textInput("observer", labelMandatory("Observer"))
          ),
          column(
            6,
            textInput("email", "Email")
          )
        ),
        # textInput("genus", labelMandatory("Genus")),
        selectInput(
          "genus", labelMandatory("Genus"),
          c("", genusoi_list),
          selected = "Acer"
        ),
        textInput("species", "Species"),
        # textInput("date", labelMandatory("Date")),
        dateInput(
          "date",
          labelMandatory("Date"),
          value = "2021-04-11",
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
          value = 42,
          min = 25,
          max = 53,
          step = NA,
          width = NULL
        ),
        numericInput(
          "longitude",
          labelMandatory("Longitude"),
          value = -83,
          min = -125,
          max = -67,
          step = NA,
          width = NULL
        ),
        selectInput(
          "event", labelMandatory("Phenological event"),
          c(
            "",
            "Leafing",
            "Flowering"
          ),
          selected = "Flowering"
        ),
        selectInput(
          "status", labelMandatory("Phenological status"),
          c("", "Yes", "No"),
          selected = "Yes"
        ),
        sliderInput("radius", "Selected Radius", min = 100, max = 500, value = 100, ticks = T, step = 100),
        fluidRow(
          column(
            3,
            actionButton("submit", "Submit", class = "btn-primary")
          ),
          column(
            9,
            shinyjs::hidden(
              tags$div(
                id = "thankyou_msg",
                "Thanks, your response was submitted successfully!\n
                            Wait a minute for some customized plots."
              )
            )
          )
        )
      ),
      mainPanel(
        fluidRow(
          column(
            6,
            selectInput(
              "plot", "Plot",
              c(
                "Line", "Map", "Boxplot"
                # , "Function"
              )
            )
          ),
          column(
            6
          )
        ),
        plotOutput("plot", height = "550px"),
        fluidRow(
          column(
            2,
            actionButton("go", "Take a screenshot", class = "btn-primary")
          ),
          column(
            2
            # tags$a(
            #   href = "https://twitter.com/intent/tweet?button_hashtag=phenology&ref_src=twsrc%5Etfw",
            #   class = "twitter-hashtag-button",
            #   "data-size" = "large",
            #   "data-show-count" = "false",
            #   "Tweet #phenology"
            # ),
            # tags$script(
            #   async = NA,
            #   src = "https://platform.twitter.com/widgets.js",
            #   charset = "utf-8"
            # )
          ),
          column(
            8,
            tags$div(
              id = "cite", align = "right",
              "", tags$em('"PhenoWatch"'), "by Yiluan Song"
            ),
            tags$a(
              id = "link", target = "_blank",
              # href = "https://sites.google.com/umich.edu/phenoinfo/",
              tags$div(
                id = "linktext", align = "right",
                "Visit ", tags$em('"PhenoInfo"'), ""
              )
            )
          )
        )
      )
    )
  ),
  server = function(input, output, session) {
    observe({
      mandatoryFilled <-
        vapply(
          fieldsMandatory,
          function(x) {
            !is.null(input[[x]]) && as.character(input[[x]]) != ""
          },
          logical(1)
        )
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
      fileName <- sprintf(
        "%s_%s.csv",
        humanTime(),
        digest::digest(formData())
      )
      write.csv(
        x = formData(), file = file.path(tempdir(), fileName),
        row.names = FALSE, quote = TRUE
      )
      upload_to_s3(file.path(tempdir(), fileName))
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })

    observeEvent(input$submit, {
      plot_and_message <- generate_output(input, radius = input$radius * 1000)

      output$plot <- renderPlot({
        generate_plot(plot_and_message, input)
      })
    })

    observeEvent(input$go, {
      fileName <- sprintf(
        "%s_%s",
        humanTime(),
        digest::digest(formData())
      )
      shinyscreenshot::screenshot(filename = fileName)
    })

    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
  }
)
