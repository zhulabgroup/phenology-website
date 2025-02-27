# https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
library(shinyjs)
library(shinyscreenshot)
library(tidyverse)
library(imputeTS)
library(sp)
library(gstat)
library(ggpubr)
library(gridExtra)
library(maps)
library(aws.s3)
library(ggnewscale)
library(mapproj)
library(ggridges)

# Helper Functions -------------------------------------------------
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

util_fill_whit <- function(x, maxgap = Inf, lambda, minseg = 2) {
  x_fill <- imputeTS::na_replace(x, fill = -9999, maxgap = maxgap) # fill short gaps with -9999 placeholder
  w <- (x_fill != -9999) # weight = 0 at long gaps, weight = 1 at short gaps
  x_sm <- util_whit(x = x_fill, lambda = lambda, w = w, minseg = minseg)

  return(x_sm)
}

util_whit <- function(x, lambda, w, minseg = 2) {
  max_id <- 0
  done <- F
  while (!done) {
    v_non_na <- which(!is.na(x[(max_id + 1):length(x)])) # non-NA segment
    if (length(v_non_na) == 0) { # all numbers are NA
      done <- T # consider this ts done
    } else {
      min_id <- min(v_non_na) + (max_id) # first number that is not NA
      v_na <- which(is.na(x[min_id:length(x)])) # NA segment
      if (length(v_na) == 0) { # no more NA
        max_id <- length(x) # last non-NA segment is at the end of the whole ts
        done <- T # consider this ts done
      } else {
        max_id <- min(v_na) - 1 + (min_id - 1) # index of last number in this NA segment
      }
      if (max_id - min_id + 1 < minseg) { # this non-NA segment is too short
        x[min_id:max_id] <- -9999
      } else {
        x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda, w[min_id:max_id]) # whittaker smoothing for this non-NA segment
      }
    }
  }
  x[x == -9999] <- NA
  return(x)
}

# calc_range <- function(df) {
#   non_na_indices <- which(!is.na(df$intensity))
#   flag <- F
#
#   first_instance <- -1
#   last_instance <- -1
#
#   for (j in 1:(length(non_na_indices) - 1)) {
#     current_index <- non_na_indices[j]
#     next_index <- non_na_indices[j + 1]
#
#     if (df$intensity[current_index] <= 0.25 && df$intensity[next_index] >= 0.25) {
#       if (flag == F) {
#         first_instance <- current_index
#         flag <- T
#       }
#     }
#     if (df$intensity[current_index] >= 0.25 && df$intensity[next_index] <= 0.25) {
#       last_instance <- current_index
#     }
#   }
#   return(c(first_instance, last_instance))
# }

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

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# Plot Generation -----------------------------------------------

generate_output <- function(input) {
  data_path_subset <- paste0(
    download_folder_path,
    ifelse(input$event == "Leaf", "leaf", "flower"),
    "/",
    input$genus
  )
  npn_files <- aws.s3::get_bucket(bucket = bucket_name, prefix = data_path_subset)

  # Data prep -------------------------

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
      select(site_id, latitude, longitude, observation_date, day_of_year, phenophase_status) %>%
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

  # prep for p_line and p_line_year
  npn_location <- npn_data_all %>%
    filter(
      abs(latitude - input$latitude) <= input$radius * 1000 / 100000,
      abs(longitude - input$longitude) <= input$radius * 1000 / 100000
    )

  if (nrow(npn_location) > 0) {
    npn_location <- npn_location %>%
      rowwise() %>%
      mutate(distance = geosphere::distm(x = c(longitude, latitude), y = c(input$longitude, input$latitude), fun = geosphere::distGeo) %>% as.numeric()) %>%
      arrange(distance) %>%
      filter(distance <= input$radius * 1000)
  } else {
    npn_location <- data.frame(
      site_id = double(0),
      latitude = double(0),
      longitude = double(0),
      observation_date = as.Date(character(0)),
      day_of_year = double(0),
      phenophase_status = double(0),
      year = integer(0)
    )
  }

  npn_location_ts <- npn_location %>%
    select(day_of_year, phenophase_status) %>%
    group_by(day_of_year) %>%
    summarize(intensity = mean(phenophase_status)) %>%
    ungroup() %>%
    filter(day_of_year != 366) %>%
    complete(day_of_year = 1:365, fill = list(intensity = NA)) %>%
    mutate(intensity = util_fill_whit(x = intensity, maxgap = 28, lambda = 10, minseg = 2)) %>% # weighted whittaker smoothing allowing gaps
    ungroup()

  # p_line -------------------
  npn_counts <- npn_location %>%
    filter(phenophase_status %in% c(0, 1)) %>%
    count(day_of_year, phenophase_status)

  p_line <- ggplot() +
    geom_tile(
      data = npn_counts %>% filter(phenophase_status == 1),
      aes(x = day_of_year, y = 100, fill = n),
      alpha = 1, width = 1, height = 8
    ) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Number of Yes") +
    new_scale_fill() +
    geom_tile(
      data = npn_counts %>% filter(phenophase_status == 0),
      aes(x = day_of_year, y = 0, fill = n),
      alpha = 1, width = 1, height = 8
    ) +
    scale_fill_gradient(low = "#FFF700", high = "#F4C430", name = "Number of No") +
    geom_line(data = npn_location_ts, aes(x = day_of_year, y = intensity * 100), col = "blue", lwd = 2) +
    geom_point(aes(x = as.integer(format(input$date, "%j")), y = as.integer(input$status == "Yes") * 100), col = "#ff0000", cex = 5) +
    geom_vline(xintercept = as.integer(format(input$date, "%j")), col = "#ff0000", alpha = .25, lwd = 1.5, linetype = "dashed") +
    scale_x_continuous(
      breaks = c(1, 32, 61, 92, 122, 153, 183, 214, 245, 275, 306, 336),
      labels = month.abb,
      limits = c(1, 365)
    ) +
    scale_y_continuous(
      breaks = c(0, 25, 50, 75, 100),
      limits = c(-10, 110)
    ) +
    labs(
      x = "Day of year",
      y = "% Yes status",
      fill = "Count"
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )

  # rect graph -------------

  if (nrow(npn_location) > 0) {
    npn_location_ts_by_year <- npn_location %>%
      select(year, day_of_year, phenophase_status) %>%
      group_by(year, day_of_year) %>%
      summarize(intensity = mean(phenophase_status)) %>%
      ungroup() %>%
      group_by(year) %>%
      complete(day_of_year = 1:365, fill = list(intensity = NA)) %>%
      mutate(intensity = util_fill_whit(x = intensity, maxgap = 28, lambda = 10, minseg = 2)) %>% # weighted whittaker smoothing allowing gaps
      ungroup() %>%
      mutate(intensity = ifelse(intensity < 1e-5, NA, intensity)) %>%
      group_by(year) %>%
      filter(!all(is.na(intensity))) %>%
      ungroup()
  } else {
    npn_location_ts_by_year <- data.frame(
      year = integer(0),
      day_of_year = double(0),
      intensity = double(0)
    )
  }
  
  npn_location_ts_by_year$day_of_year_rev <- 366 - npn_location_ts_by_year$day_of_year

  p_line_year <- npn_location_ts_by_year %>%
    mutate(year = factor(year, levels = rev(unique(year)))) %>%
    ggplot(aes(x = day_of_year_rev, y = factor(year), height = intensity, fill = intensity * 100)) +
    geom_ridgeline_gradient(scale = 1) +
    scale_fill_viridis_c(name = "% Yes", limits = c(0, 100)) +
    theme_minimal() +
    labs(x = "Day of year", y = "Year") +
    scale_x_continuous(
      breaks = c(336, 306, 275, 245, 214, 183, 153, 122, 92, 61, 32, 1),
      labels = month.abb,
      limits = c(1, 365),
      expand = c(0, 0)
    ) +
    scale_y_discrete(limits = seq(min(npn_location_ts_by_year$year), max(npn_location_ts_by_year$year), by = 1) %>% as.character() %>% rev() %>% factor()) +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) + coord_flip()

  ###### Map data prep -----------------------
  npn_time <- npn_data_all %>%
    filter(abs(observation_date - input$date) <= input$window) %>%
    arrange(day_of_year)
  npn_time_surface <- npn_time %>%
    group_by(longitude, latitude) %>%
    summarize(intensity = mean(phenophase_status)) %>%
    ungroup()
  if (nrow(npn_time_surface) > 0) {
    npn_time_sp <- SpatialPointsDataFrame(
      coords = npn_time_surface[, c("longitude", "latitude")],
      data = npn_time_surface[, c("intensity"), drop = F],
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )
    # Step 1: Compute Empirical Variogram
    empirical_variogram <- variogram(intensity ~ 1, npn_time_sp)
    # Step 2: Fit a Variogram Model
    fit_npn <- fit.variogram(empirical_variogram, model = vgm("Mat", nugget = 0.05, range = 1000, kappa = 0.01))
    
    # Step 3: Define Raster Grid for Interpolation
    # Define the extent (bounding box)
    xmin <- -125
    xmax <- -67
    ymin <- 25
    ymax <- 53
    # Define resolution (grid spacing)
    resolution <- 0.5
    
    # Create a grid using expand.grid() for all of continental US
    grid_points <- expand.grid(
      lon = seq(xmin, xmax, by = resolution),
      lat = seq(ymin, ymax, by = resolution)
    )
    
    # Convert to SpatialPoints
    coord_new_sp <- SpatialPoints(
      coords = grid_points,
      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )
    
    # Step 4: Perform Kriging Interpolation on the full grid
    kriged_res <- krige(intensity ~ 1, npn_time_sp, coord_new_sp,
                        model = fit_npn,
                        na.action = na.omit
    )
    
    # Convert to DataFrame
    kriged_res_df <- as.data.frame(kriged_res)
    names(kriged_res_df)[1:2] <- c("lon", "lat")
    
    # Get all US states data
    all_states <- map_data("state")
    
    # Filter kriged_res_df to only include points on land
    # We'll use point.in.polygon from the sp package
    # but we need to do it state by state to handle complex boundaries
    
    # Initialize a vector to track points on land
    on_land <- rep(FALSE, nrow(kriged_res_df))
    
    # Check each state
    for (i in unique(all_states$region)) {
      # Get the state outline
      state_outline <- all_states[all_states$region == i, ]
      
      # For each state group (some states have multiple polygons)
      for (g in unique(state_outline$group)) {
        poly <- state_outline[state_outline$group == g, ]
        
        # Use point.in.polygon to check which points are in this polygon
        inside <- sp::point.in.polygon(
          kriged_res_df$lon, 
          kriged_res_df$lat, 
          poly$long, 
          poly$lat
        ) > 0
        
        # Update the on_land vector
        on_land <- on_land | inside
      }
    }
    
    # Keep only points on land
    kriged_res_df <- kriged_res_df[on_land, ]
    
  } else {
    kriged_res_df <- data.frame(
      lon = double(0),
      lat = double(0),
      var1.pred = double(0),
      var1.var = double(0)
    )
  }
  
  ###### p_map -------------------------------------
  p_map <- ggplot() +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    geom_tile(data = kriged_res_df, aes(x = lon, y = lat, fill = var1.pred * 100)) +
    geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey", fill = NA) +
    geom_jitter(data = npn_time, aes(x = longitude, y = latitude, fill = phenophase_status * 100), pch = 21, width = 0.05, height = 0.05, cex = 2) +
    geom_point(aes(x = input$longitude, y = input$latitude, fill = as.integer(input$status == "Yes") * 100), pch = 21, col = "red", cex = 5, stroke = 3) +
    scale_color_viridis_c(limits = c(0, 100)) +
    scale_fill_viridis_c(limits = c(0, 100)) +
    labs(
      fill = "% Yes"
    ) +
    theme_void()
  ###### generate_output return -----------------
  return(list(p_line, p_line_year, p_map))
}

# generate_plot  ------------
generate_plot <- function(plot, input) {
  p <- plot[[case_when(
    input$plot == "Intra-annual variations" ~ 1,
    input$plot == "Inter-annual variations" ~ 2,
    input$plot == "Spatial variations" ~ 3
  )]]

  return(p)
}

# shinyApp -------------------------------------------------------------

MANDATORY_FIELDS <- c("observer", "genus", "date", "latitude", "longitude", "event", "status")
ALL_FIELDS <- c(MANDATORY_FIELDS, "email", "species")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*",
      class = "mandatory-star",
      style = "color: red; font-size: 16px; margin-left: 3px;"
    )
  )
}

validationCSS <- "
.help-block {
  color: #dc3545;
  margin-top: 5px;
  font-size: 0.9em;
  display: none;
}
.validation-error {
  border-color: #dc3545 !important;
}
"


## UI Components --------------------------------------------------
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(paste0(appCSS, validationCSS)),
  titlePanel("PhenoWatch"),
  sidebarLayout(
    sidebarPanel(
      # User Info Section
      fluidRow(
        column(
          6,
          textInput("observer", labelMandatory("Observer")),
          tags$div(
            id = "observer-error", class = "help-block",
            "Observer name is required"
          )
        ),
        column(
          6,
          textInput("email", "Email")
        )
      ),

      # Taxa Selection

      fluidRow(
        column(
          6,
          selectInput(
            "genus", labelMandatory("Genus"),
            c("", genusoi_list),
            selected = "Acer"
          ),
          tags$div(
            id = "genus-error", class = "help-block",
            "Please select a genus"
          ),
        ),
        column(
          6,
          textInput("species", "Species"),
        )
      ),

      # Date Selection
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
        autoclose = TRUE
      ),
      tags$div(
        id = "date-error", class = "help-block",
        "Please select a valid date"
      ),

      # Location Inputs
      fluidRow(
        column(
          6,
          numericInput(
            "latitude",
            labelMandatory("Latitude"),
            value = 42,
            min = 25,
            max = 53
          ),
          tags$div(
            id = "latitude-error", class = "help-block",
            "Latitude must be between 25° and 53°"
          ),
        ),
        column(
          6,
          numericInput(
            "longitude",
            labelMandatory("Longitude"),
            value = -83,
            min = -125,
            max = -67
          ),
          tags$div(
            id = "longitude-error", class = "help-block",
            "Longitude must be between -125° and -67°"
          ),
        )
      ),

      # Phenology Selection

      fluidRow(
        column(
          6,
          selectInput(
            "event", labelMandatory("Phenological event"),
            c("", "Leaf", "Flower"),
            selected = "Leaf"
          ),
          tags$div(
            id = "event-error", class = "help-block",
            "Please select an event type"
          ),
        ),
        column(
          6,
          selectInput(
            "status", labelMandatory("Phenological status"),
            c("", "Yes", "No"),
            selected = "Yes"
          ),
          tags$div(
            id = "status-error", class = "help-block",
            "Please select a status"
          ),
        )
      ),

      # Radius Selection
      sliderInput("radius", "Search radius",
        min = 100, max = 500,
        value = 100, step = 100,
        ticks = TRUE
      ),

      # Window Selection
      sliderInput("window", "Search window",
        min = 7, max = 21,
        value = 14, step = 7,
        ticks = TRUE
      ),

      # Submit Section
      fluidRow(
        column(3, actionButton("submit", "Submit", class = "btn-primary")),
        column(
          9,
          shinyjs::hidden(
            tags$div(
              id = "thankyou_msg",
              "Thanks, your response was submitted successfully!
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
              "Intra-annual variations",
              "Inter-annual variations",
              "Spatial variations"
            )
          )
        ),
        column(6)
      ),
      plotOutput("plot", height = "550px"),
      fluidRow(
        column(2, actionButton("go", "Take a screenshot", class = "btn-primary")),
        column(
          2
        ),
        column(
          8,
          tags$div(
            id = "cite", align = "right",
            "", tags$em('"PhenoWatch"'), "by Yiluan Song"
          )
        )
      )
    )
  )
)

## Server Logic -------------------------------------------------
server <- function(input, output, session) {
  # Validation rules for each field
  validationRules <- list(
    observer = function(value) {
      if (is.null(value) || is.na(value) || value == "") {
        return(list(valid = FALSE, message = "Observer name is required"))
      }
      return(list(valid = TRUE))
    },
    genus = function(value) {
      if (is.null(value) || is.na(value) || value == "") {
        return(list(valid = FALSE, message = "Please select a genus"))
      }
      return(list(valid = TRUE))
    },
    date = function(value) {
      if (is.null(value) || is.na(value)) {
        return(list(valid = FALSE, message = "Please select a valid date"))
      }
      return(list(valid = TRUE))
    },
    latitude = function(value) {
      if (is.null(value) || is.na(value)) {
        return(list(valid = FALSE, message = "Latitude is required"))
      }
      value <- as.numeric(value)
      if (is.na(value) || value < 25 || value > 53) {
        return(list(valid = FALSE, message = "Latitude must be between 25° and 53°"))
      }
      return(list(valid = TRUE))
    },
    longitude = function(value) {
      if (is.null(value) || is.na(value)) {
        return(list(valid = FALSE, message = "Longitude is required"))
      }
      value <- as.numeric(value)
      if (is.na(value) || value < -125 || value > -67) {
        return(list(valid = FALSE, message = "Longitude must be between -125° and -67°"))
      }
      return(list(valid = TRUE))
    },
    event = function(value) {
      if (is.null(value) || is.na(value) || value == "") {
        return(list(valid = FALSE, message = "Please select an event type"))
      }
      return(list(valid = TRUE))
    },
    status = function(value) {
      if (is.null(value) || is.na(value) || value == "") {
        return(list(valid = FALSE, message = "Please select a status"))
      }
      return(list(valid = TRUE))
    }
  )

  # Validate fields and update UI -----------
  observe({
    validation_results <- list()

    # Validate each mandatory field
    for (field in MANDATORY_FIELDS) {
      # Get the validation rule for this field
      rule <- validationRules[[field]]
      if (!is.null(rule)) {
        # Get the current value and validate it
        result <- rule(input[[field]])
        validation_results[[field]] <- result

        # Update UI based on validation result
        if (!result$valid) {
          shinyjs::addCssClass(field, "validation-error")
          # Update error message text and show it
          shinyjs::html(paste0(field, "-error"), result$message)
          shinyjs::show(paste0(field, "-error"))
        } else {
          shinyjs::removeCssClass(field, "validation-error")
          shinyjs::hide(paste0(field, "-error"))
        }
      }
    }

    # Enable submit and screenshot buttons if all validations pass
    all_valid <- all(sapply(validation_results, function(x) x$valid))
    shinyjs::toggleState(id = "submit", condition = all_valid)
    shinyjs::toggleState(id = "go", condition = all_valid)
    # shinyjs::toggleState(id = "card", condition = mandatoryFilled)
  })

  # Collect form data -----------------
  formData <- reactive({
    data <- sapply(ALL_FIELDS, function(x) as.character(input[[x]]))
    data <- c(data, timestamp = as.character(Sys.time()))
    t(data)
  })

  # Handle form submission ---------------
  observeEvent(input$submit, {
    # Clear plot and show processing message
    output$plot <- renderPlot(NULL)
    shinyjs::show("thankyou_msg")

    # Save form data
    fileName <- sprintf(
      "%s_%s.csv",
      humanTime(),
      digest::digest(formData())
    )
    write.csv(
      x = formData(),
      file = file.path(tempdir(), fileName),
      row.names = FALSE,
      quote = TRUE
    )
    upload_to_s3(file.path(tempdir(), fileName))

    # Generate new plot
    plot <- generate_output(input)

    # Update UI
    shinyjs::hide("thankyou_msg")
    output$plot <- renderPlot({
      generate_plot(plot, input)
    })
  })

  # Handle screenshot ----------------
  observeEvent(input$go, {
    fileName <- sprintf(
      "%s_%s",
      humanTime(),
      digest::digest(formData())
    )
    shinyscreenshot::screenshot(filename = fileName)
  })
}

## Create and Run Application -----------------------------------
shinyApp(ui = ui, server = server)
