library(shiny)
library(shinyjs)
library(tidyverse)
library(mapproj)

# Helper Functions -------------------------------------------------
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
  aws.s3::put_object(
    file = file_path, bucket = bucket_name, object = s3_key,
    headers = list(`x-amz-acl` = "bucket-owner-full-control")
  )
}

util_fill_whit <- function(x, maxgap = Inf, lambda, minseg = 2) {
  x_fill <- imputeTS::na_replace(x, fill = -9999, maxgap = maxgap)
  w <- (x_fill != -9999)
  x_sm <- util_whit(x = x_fill, lambda = lambda, w = w, minseg = minseg)
  return(x_sm)
}

util_whit <- function(x, lambda, w, minseg = 2) {
  max_id <- 0
  done <- F
  while (!done) {
    v_non_na <- which(!is.na(x[(max_id + 1):length(x)]))
    if (length(v_non_na) == 0) {
      done <- T
    } else {
      min_id <- min(v_non_na) + (max_id)
      v_na <- which(is.na(x[min_id:length(x)]))
      if (length(v_na) == 0) {
        max_id <- length(x)
        done <- T
      } else {
        max_id <- min(v_na) - 1 + (min_id - 1)
      }
      if (max_id - min_id + 1 < minseg) {
        x[min_id:max_id] <- -9999
      } else {
        x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda, w[min_id:max_id])
      }
    }
  }
  x[x == -9999] <- NA
  return(x)
}

genusoi_list <- c("Acer", "Quercus", "Betula", "Populus")

appCSS <- ".mandatory_star { color: red; }"

MANDATORY_FIELDS <- c("observer", "genus", "date", "latitude", "longitude", "event", "status")
ALL_FIELDS <- c(MANDATORY_FIELDS, "email", "species")

# Plot Generation with DIAGNOSTICS -----------------------------------------------

generate_output_for_type <- function(input, phenotype) {
  timing_log <- list()
  
  timing_log$start <- paste("========== START", phenotype, "==========")
  
  data_path_subset <- paste0(download_folder_path, phenotype, "/", input$genus)
  
  # TIMING: S3 bucket listing
  t <- system.time({
    npn_files <- aws.s3::get_bucket(bucket = bucket_name, prefix = data_path_subset)
  })
  timing_log$s3_bucket <- paste("S3 get_bucket():", round(t["elapsed"], 3), "sec")
  
  if (length(npn_files) > 0) {
    # TIMING: CSV loading from S3
    t <- system.time({
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
        select(site_id, latitude, longitude, observation_date, day_of_year, phenophase_status) %>%
        filter(phenophase_status != -1) %>%
        mutate(year = as.integer(format(observation_date, "%Y"))) %>%
        filter(longitude >= -125, longitude <= -67, latitude >= 25, latitude <= 53)
    })
    timing_log$csv_load <- paste("CSV loading & filtering:", round(t["elapsed"], 3), "sec,", nrow(npn_data_all), "rows")
  } else {
    npn_data_all <- data.frame(site_id = double(0), latitude = double(0), longitude = double(0),
                                observation_date = as.Date(character(0)), day_of_year = double(0),
                                phenophase_status = double(0), year = integer(0))
    timing_log$csv_load <- "CSV loading: no files found"
  }
  
  # TIMING: Location filtering
  t <- system.time({
    npn_location <- npn_data_all %>%
      filter(abs(latitude - input$latitude) <= input$radius * 1000 / 100000,
             abs(longitude - input$longitude) <= input$radius * 1000 / 100000)
    
    if (nrow(npn_location) > 0) {
      npn_location <- npn_location %>%
        rowwise() %>%
        mutate(distance = geosphere::distm(x = c(longitude, latitude), y = c(input$longitude, input$latitude), fun = geosphere::distGeo) %>% as.numeric()) %>%
        arrange(distance) %>%
        filter(distance <= input$radius * 1000)
    }
  })
  timing_log$location_filter <- paste("Location filtering:", round(t["elapsed"], 3), "sec,", nrow(npn_location), "rows in radius")
  
  # TIMING: Whittaker smoothing
  t <- system.time({
    npn_location_ts <- npn_location %>%
      select(day_of_year, phenophase_status) %>%
      group_by(day_of_year) %>%
      summarize(intensity = mean(phenophase_status)) %>%
      ungroup() %>%
      filter(day_of_year != 366) %>%
      complete(day_of_year = 1:365, fill = list(intensity = NA)) %>%
      mutate(intensity = util_fill_whit(x = intensity, maxgap = 28, lambda = 10, minseg = 2)) %>%
      ungroup()
  })
  timing_log$whittaker <- paste("Whittaker smoothing (temporal):", round(t["elapsed"], 3), "sec")
  
  # TIMING: p_line plot
  t <- system.time({
    npn_counts <- npn_location %>%
      filter(phenophase_status %in% c(0, 1)) %>%
      count(day_of_year, phenophase_status)
    
    p_line <- ggplot() +
      geom_tile(data = npn_counts %>% filter(phenophase_status == 1),
                aes(x = day_of_year, y = 100, fill = n), alpha = 1, width = 1, height = 8) +
      scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of Yes") +
      ggnewscale::new_scale_fill() +
      geom_tile(data = npn_counts %>% filter(phenophase_status == 0),
                aes(x = day_of_year, y = 0, fill = n), alpha = 1, width = 1, height = 8) +
      scale_fill_gradient(low = "#FFF700", high = "#F4C430", name = "Number of No") +
      geom_line(data = npn_location_ts, aes(x = day_of_year, y = intensity * 100), col = "blue", lwd = 2) +
      geom_point(aes(x = as.integer(format(input$date, "%j")), y = as.integer(input$status == "Yes") * 100), col = "#ff0000", cex = 5) +
      geom_vline(xintercept = as.integer(format(input$date, "%j")), col = "#ff0000", alpha = .25, lwd = 1.5, linetype = "dashed") +
      scale_x_continuous(breaks = c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336), labels = month.abb, limits = c(1, 365)) +
      scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(-10, 110)) +
      labs(x = "Day of year", y = "% Yes status", fill = "Count", title = paste(str_to_title(phenotype), "Phenology")) +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5, face = "bold"))
  })
  timing_log$p_line <- paste("p_line plot:", round(t["elapsed"], 3), "sec")
  
  # TIMING: p_line_year plot
  t <- system.time({
    if (nrow(npn_location) > 0) {
      npn_location_ts_by_year <- npn_location %>%
        select(year, day_of_year, phenophase_status) %>%
        group_by(year, day_of_year) %>%
        summarize(intensity = mean(phenophase_status)) %>%
        ungroup() %>%
        group_by(year) %>%
        complete(day_of_year = 1:365, fill = list(intensity = NA)) %>%
        mutate(intensity = util_fill_whit(x = intensity, maxgap = 28, lambda = 10, minseg = 2)) %>%
        ungroup() %>%
        mutate(intensity = ifelse(intensity < 1e-5, NA, intensity)) %>%
        group_by(year) %>%
        filter(!all(is.na(intensity))) %>%
        ungroup()
    } else {
      npn_location_ts_by_year <- data.frame(year = integer(0), day_of_year = double(0), intensity = double(0))
    }
    
    p_line_year <- npn_location_ts_by_year %>%
      mutate(intensity = case_when(intensity > 1 ~ 1, intensity < 0 ~ 0, TRUE ~ intensity)) %>%
      mutate(day_of_year = 366 - day_of_year) %>%
      mutate(year = factor(year, levels = unique(year))) %>%
      ggplot(aes(x = day_of_year, y = factor(year), height = intensity, fill = intensity * 100)) +
      ggridges::geom_ridgeline_gradient(scale = 1) +
      scale_fill_viridis_c(name = "% Yes", limits = c(0, 100)) +
      theme_minimal() +
      labs(x = "Day of year", y = "Year", title = paste(str_to_title(phenotype), "Phenology")) +
      scale_x_continuous(breaks = 366 - c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336),
                         labels = month.abb, limits = c(1, 365), expand = c(0, 0)) +
      {
        if (nrow(npn_location_ts_by_year) > 0) {
          scale_y_discrete(limits = seq(min(npn_location_ts_by_year$year), max(npn_location_ts_by_year$year), by = 1) %>%
                             as.character() %>% factor())
        } else {
          scale_y_discrete(limits = as.character(2010:2025))
        }
      } +
      theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
            plot.title = element_text(size = 18, hjust = 0.5, face = "bold")) +
      coord_flip()
  })
  timing_log$p_line_year <- paste("p_line_year plot:", round(t["elapsed"], 3), "sec")
  
  # TIMING: Kriging section
  t_kriging_total <- system.time({
    npn_time <- npn_data_all %>%
      filter(abs(day_of_year - (input$date) %>% as.Date() %>% lubridate::yday()) <= input$window) %>%
      arrange(day_of_year)
    
    timing_log$kriging_prep <- paste("  Kriging prep: window filter =", nrow(npn_time), "rows")
    
    npn_time_surface <- npn_time %>% group_by(longitude, latitude) %>%
      summarize(intensity = mean(phenophase_status)) %>% ungroup()
    
    timing_log$kriging_agg <- paste("  Kriging aggregation: surface points =", nrow(npn_time_surface))
    
    if (nrow(npn_time_surface) > 0) {
      t_spdf <- system.time({
        npn_time_sp <- sp::SpatialPointsDataFrame(
          coords = npn_time_surface[, c("longitude", "latitude")],
          data = npn_time_surface[, c("intensity"), drop = F],
          proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        )
      })
      timing_log$kriging_spdf <- paste("  SPDF creation:", round(t_spdf["elapsed"], 3), "sec")
      
      t_variogram <- system.time({
        empirical_variogram <- gstat::variogram(intensity ~ 1, npn_time_sp)
      })
      timing_log$kriging_variogram <- paste("  Empirical variogram:", round(t_variogram["elapsed"], 3), "sec")
      
      if (is.null(empirical_variogram)) {
        kriged_res_df <- data.frame(lon = double(0), lat = double(0), var1.pred = double(0), var1.var = double(0))
        timing_log$kriging_skip <- "  Kriging skipped (null variogram)"
      } else {
        t_fit_vgm <- system.time({
          fit_npn <- gstat::fit.variogram(empirical_variogram, model = gstat::vgm("Mat", nugget = 0.05, range = 1000, kappa = 0.01))
        })
        timing_log$kriging_fit <- paste("  Fit variogram:", round(t_fit_vgm["elapsed"], 3), "sec")
        
        xmin <- -125; xmax <- -67; ymin <- 25; ymax <- 53; resolution <- 1.0
        
        t_grid <- system.time({
          grid_points <- expand.grid(lon = seq(xmin, xmax, by = resolution), lat = seq(ymin, ymax, by = resolution))
          coord_new_sp <- sp::SpatialPoints(coords = grid_points,
                                            proj4string = sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
        })
        timing_log$kriging_grid <- paste("  Grid creation: points =", nrow(grid_points), ", time =", round(t_grid["elapsed"], 3), "sec")
        
        t_krige <- system.time({
          kriged_res <- gstat::krige(intensity ~ 1, npn_time_sp, coord_new_sp, model = fit_npn, na.action = na.omit)
        })
        timing_log$kriging_krige <- paste("  Krige interpolation:", round(t_krige["elapsed"], 3), "sec")
        
        t_pip <- system.time({
          kriged_res_df <- as.data.frame(kriged_res)
          names(kriged_res_df)[1:2] <- c("lon", "lat")
          
          all_states <- fortify(maps::map("state", plot = FALSE, fill = TRUE))
          on_land <- rep(FALSE, nrow(kriged_res_df))
          
          for (i in unique(all_states$region)) {
            state_outline <- all_states[all_states$region == i, ]
            for (g in unique(state_outline$group)) {
              poly <- state_outline[state_outline$group == g, ]
              inside <- sp::point.in.polygon(kriged_res_df$lon, kriged_res_df$lat, poly$long, poly$lat) > 0
              on_land <- on_land | inside
            }
          }
          kriged_res_df <- kriged_res_df[on_land, ]
        })
        timing_log$kriging_pip <- paste("  Point-in-polygon filter:", round(t_pip["elapsed"], 3), "sec, points =", nrow(kriged_res_df))
      }
    } else {
      kriged_res_df <- data.frame(lon = double(0), lat = double(0), var1.pred = double(0), var1.var = double(0))
      timing_log$kriging_nodata <- "  Kriging skipped (no data in window)"
    }
  })
  timing_log$kriging_total <- paste("Total kriging section:", round(t_kriging_total["elapsed"], 3), "sec")
  
  # TIMING: p_map plot
  t <- system.time({
    p_map <- ggplot() +
      coord_map("albers", lat0 = 39, lat1 = 45) +
      geom_tile(data = kriged_res_df %>% mutate(pred = case_when(var1.pred > 1 ~ 1, var1.pred < 0 ~ 0, TRUE ~ var1.pred)),
                aes(x = lon, y = lat, fill = pred * 100)) +
      geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey", fill = NA) +
      geom_jitter(data = npn_time, aes(x = longitude, y = latitude, fill = phenophase_status * 100), pch = 21, width = 0.05, height = 0.05, cex = 2) +
      geom_point(aes(x = input$longitude, y = input$latitude, fill = as.integer(input$status == "Yes") * 100), pch = 21, col = "red", cex = 5, stroke = 3) +
      scale_color_viridis_c(limits = c(0, 100)) +
      scale_fill_viridis_c(limits = c(0, 100)) +
      labs(fill = "% Yes", title = paste(str_to_title(phenotype), "Phenology")) +
      theme_void() +
      theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"))
  })
  timing_log$p_map <- paste("p_map plot:", round(t["elapsed"], 3), "sec")
  
  timing_log$end <- paste("========== END", phenotype, "==========\n")
  
  return(list(plots = list(p_line, p_line_year, p_map), timing = timing_log))
}

generate_output <- function(input) {
  t_total <- system.time({
    leaf_result <- generate_output_for_type(input, "leaf")
    flower_result <- generate_output_for_type(input, "flower")
  })
  
  combined_timing <- c(
    "+++++++++ START TOTAL +++++++++",
    leaf_result$timing,
    flower_result$timing,
    paste("+++++++++ END TOTAL |", round(t_total["elapsed"], 3), "sec +++++++++")
  )
  
  return(list(
    leaf = leaf_result$plots,
    flower = flower_result$plots,
    timing = combined_timing
  ))
}

generate_plot <- function(plots, input) {
  plot_type_index <- case_when(
    input$plot == "Intra-annual variations" ~ 1,
    input$plot == "Inter-annual variations" ~ 2,
    input$plot == "Spatial variations" ~ 3
  )
  
  show_leaf <- input$leaf_select
  show_flower <- input$flower_select
  
  if (!show_leaf && !show_flower) {
    show_leaf <- TRUE
  }
  
  if (show_leaf && show_flower) {
    leaf_plot <- plots$leaf[[plot_type_index]]
    flower_plot <- plots$flower[[plot_type_index]]
    return(gridExtra::grid.arrange(leaf_plot, flower_plot, ncol = 1))
  } else if (show_leaf) {
    return(plots$leaf[[plot_type_index]])
  } else {
    return(plots$flower[[plot_type_index]])
  }
}

## UI Components --------------------------------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  titlePanel("PhenoWatch DIAGNOSTICS"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(column(6, textInput("observer", "Observer*")), column(6, textInput("email", "Email"))),
      fluidRow(
        column(6, selectInput("genus", "Genus*", c("", "Acer", "Quercus", "Betula", "Populus"), selected = "Acer")),
        column(6, textInput("species", "Species"))
      ),
      dateInput("date", "Date*", value = Sys.Date(), format = "yyyy-mm-dd"),
      fluidRow(
        column(6, numericInput("latitude", "Latitude*", value = 42, min = 25, max = 53)),
        column(6, numericInput("longitude", "Longitude*", value = -83, min = -125, max = -67))
      ),
      fluidRow(
        column(6, selectInput("event", "Event*", c("", "Leaf", "Flower"), selected = "Leaf")),
        column(6, selectInput("status", "Status*", c("", "Yes", "No"), selected = "Yes"))
      ),
      sliderInput("radius", "Search radius (km)", min = 100, max = 500, value = 100, step = 100),
      sliderInput("window", "Time range (± days)", min = 7, max = 21, value = 14, step = 7),
      actionButton("submit", "Submit", class = "btn-primary")
    ),
    mainPanel(
      h4("Timing Output:"),
      verbatimTextOutput("diagnostics"),
      br(),
      selectInput("plot", "Plot", c("Intra-annual variations", "Inter-annual variations", "Spatial variations")),
      fluidRow(
        column(6, checkboxInput("leaf_select", "Leaf", TRUE)),
        column(6, checkboxInput("flower_select", "Flower", FALSE))
      ),
      plotOutput("plot", height = "800px")
    )
  )
)

## Server Logic -------------------------------------------------
server <- function(input, output, session) {
  plots_data <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    withProgress(message = "Generating plots...", value = 0, {
      plots <- generate_output(input)
      plots_data(plots)
      
      output$diagnostics <- renderText({
        paste(plots$timing, collapse = "\n")
      })
      
      output$plot <- renderPlot({
        generate_plot(list(leaf = plots$leaf, flower = plots$flower), input)
      })
    })
  })
  
  observeEvent(c(input$leaf_select, input$flower_select, input$plot), {
    if (!is.null(plots_data())) {
      output$plot <- renderPlot({
        generate_plot(list(leaf = plots_data()$leaf, flower = plots_data()$flower), input)
      })
    }
  })
}

shinyApp(ui = ui, server = server)
