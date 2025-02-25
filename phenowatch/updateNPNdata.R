###
Sys.setenv(
  "AWS_DEFAULT_REGION" = "us-east-2",
  "AWS_S3_ENDPOINT" = "s3.amazonaws.com"
)

bucket_name <- "phenoobservers"
data_path <- "PhenoWatch/NPN/"

upload_to_s3 <- function(file_path) {
  filename <- basename(file_path)
  s3_key <- paste0(submit_folder_path, filename)
  put_object(
    file = file_path, bucket = bucket_name, object = s3_key,
    headers = list(`x-amz-acl` = "bucket-owner-full-control")
  )
}

###
npn_files <- aws.s3::get_bucket(bucket = bucket_name, prefix = data_path)

last_year <- npn_files %>%
  purrr::map_chr(~ .x$Key) %>% # Extract the 'Key' for each object, which is the file name
  stringr::str_extract("[^/]+$") %>%
  str_remove(".csv") %>%
  as.numeric() %>%
  max()

this_year <- as.integer(format(Sys.Date(), "%Y"))
if (last_year == this_year) {
  year_update <- this_year
} else {
  year_update <- seq(last_year, this_year, by = 1)
}

###
path_leaf <- paste0(data_path, "leaf/")
path_flower <- paste0(data_path, "flower/")

species_list <- rnpn::npn_species()
genus_list <- c(
  "Acer",
  "Quercus",
  "Betula",
  "Populus"
)

###
for (genusoi in genus_list) {
  spid <- species_list %>%
    filter(genus == genusoi) %>%
    select(species_id) %>%
    unique()

  for (year in year_update) {
    npn_leaf_df <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 3
    )

    aws_file <- str_c(data_path, "leaf", "/", genusoi, "/", year, ".csv")
    temp_file <- str_c(tempdir(), "/", format(Sys.time(), "%Y%m%d-%H%M%OS"), ".csv")

    write.csv(
      x = npn_leaf_df,
      file = temp_file,
      row.names = FALSE,
      quote = TRUE
    )

    put_object(
      file = temp_file, bucket = bucket_name, object = aws_file,
      headers = list(`x-amz-acl` = "bucket-owner-full-control")
    )

    print(aws_file)
  }

  for (year in year_update) {
    npn_flower_df_1 <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 7
    )
    npn_flower_df_2 <- rnpn::npn_download_status_data(
      request_source = "YS",
      years = year,
      species_ids = spid$species_id,
      pheno_class_ids = 8
    )
    npn_flower_df <- rbind(npn_flower_df_1, npn_flower_df_2)

    aws_file <- str_c(data_path, "flower", "/", genusoi, "/", year, ".csv")
    temp_file <- str_c(tempdir(), "/", format(Sys.time(), "%Y%m%d-%H%M%OS"), ".csv")

    write.csv(
      x = npn_flower_df,
      file = temp_file,
      row.names = FALSE,
      quote = TRUE
    )

    put_object(
      file = temp_file, bucket = bucket_name, object = aws_file,
      headers = list(`x-amz-acl` = "bucket-owner-full-control")
    )

    print(aws_file)
  }
}
