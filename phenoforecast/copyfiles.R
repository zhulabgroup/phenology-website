copyfiles <- function(genusoi, varoi, bucket_name, bucket_region, date_list) {
  path_aws_data <- str_c("PhenoForecast/", genusoi, "/", varoi, "/")
  path_local_data <- str_c("data/", genusoi, "/", varoi, "/")

  files_s3 <- aws.s3::get_bucket_df(bucket = bucket_name, region = bucket_region, prefix = path_aws_data) %>%
    filter(!str_detect(Key, "/$")) %>%
    filter(str_detect(Key, ".tif$")) %>%
    filter(str_detect(Key, str_c(date_list, collapse = "|"))) %>%
    pull(Key) %>%
    sort()

  dir.create(path_local_data, recursive = T, showWarnings = T)
  print("temp folder created")
  files_exist <- list.files(path_local_data, recursive = T, full.names = T, include.dirs = F)
  files_to_remove <- files_exist[!files_exist %in% files_s3]
  file.remove(files_to_remove)

  files_to_save <- files_s3[!files_s3 %in% files_exist]
  if (length(files_to_save) > 0) {
    foreach(
      f = files_to_save,
      .packages = c("aws.s3", "tidyverse")
    ) %dopar% {
      aws.s3::save_object(
        object = f,
        bucket = bucket_name,
        region = bucket_region,
        file = str_replace(f, "PhenoForecast", "data")
      )
      print(f)
    }
  }

  return(path_local_data)
}
