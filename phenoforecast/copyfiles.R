bucket_name <- "phenoobservers"
bucket_region <- "us-east-2"
files_s3 <- aws.s3::get_bucket_df(bucket = bucket_name, region = bucket_region, prefix = "PhenoForecast", max = 10) %>%
  filter(!str_detect(Key, "/$")) %>%
  pull(Key)

dir.create("data", showWarnings = F)
files_exist <- list.files("data", recursive = T, full.names = T, include.dirs = F)
files_to_remove <- files_exist[!files_exist %in% files_s3]
file.remove(files_to_remove)

files_to_save <- files_s3[!files_s3 %in% files_exist]

for (f in files_to_save) {
  aws.s3::save_object(
    object = f,
    bucket = bucket_name,
    region = bucket_region,
    file = str_replace(f, "PhenoForecast", "data")
  )
}
