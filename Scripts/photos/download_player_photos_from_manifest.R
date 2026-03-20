# Hall Ledger photo downloader from a direct URL manifest
# Downloads player images from the manifest into the local project image folder.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(httr)
})

dir.create(file.path("images", "players"), recursive = TRUE, showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

manifest_path <- "data/player_photo_manifest.csv"
log_path <- "data/player_photo_download_log.csv"
log_columns <- c("playerID", "name", "bbref_url", "image_url", "image_source", "local_path", "status", "file_name", "timestamp")

normalize_download_log <- function(log_df) {
  if (nrow(log_df) == 0) {
    return(tibble(
      playerID = character(),
      name = character(),
      bbref_url = character(),
      image_url = character(),
      image_source = character(),
      local_path = character(),
      status = character(),
      file_name = character(),
      timestamp = character()
    ))
  }

  for (col in log_columns) {
    if (!(col %in% names(log_df))) {
      log_df[[col]] <- NA_character_
    }
  }

  log_df %>%
    mutate(across(all_of(log_columns), as.character)) %>%
    mutate(
      status = case_when(
        status == "success" ~ "downloaded",
        TRUE ~ status
      ),
      local_path = if_else(
        (is.na(local_path) | local_path == "") & !is.na(file_name) & file_name != "",
        file.path("images", "players", file_name),
        local_path
      )
    ) %>%
    select(all_of(log_columns))
}

if (!file.exists(manifest_path)) {
  stop("Missing data/player_photo_manifest.csv. Run Scripts/build_player_photo_manifest.R first.")
  quit(save = "no")
}

manifest <- read_csv(manifest_path, show_col_types = FALSE)

required_columns <- c("playerID", "image_url")
missing_columns <- setdiff(required_columns, names(manifest))
if (length(missing_columns) > 0) {
  stop("Manifest is missing required columns: ", paste(missing_columns, collapse = ", "))
}

if (!("name" %in% names(manifest))) {
  manifest$name <- NA_character_
}
if (!("source_group" %in% names(manifest))) {
  manifest$source_group <- NA_character_
}
if (!("bbref_url" %in% names(manifest))) {
  manifest$bbref_url <- NA_character_
}
if (!("image_source" %in% names(manifest))) {
  manifest$image_source <- NA_character_
}

download_log <- if (file.exists(log_path)) {
  normalize_download_log(read_csv(log_path, show_col_types = FALSE))
} else {
  normalize_download_log(tibble())
}

append_log <- function(entry) {
  current_log <- if (file.exists(log_path)) {
    normalize_download_log(read_csv(log_path, show_col_types = FALSE))
  } else {
    normalize_download_log(tibble())
  }

  entry <- normalize_download_log(entry)
  write_csv(bind_rows(current_log, entry), log_path)
}

has_local_image <- function(player_id) {
  any(file.exists(file.path("images", "players", paste0(player_id, c(".jpg", ".jpeg", ".png", ".webp")))))
}

remove_existing_local_images <- function(player_id) {
  existing_files <- file.path("images", "players", paste0(player_id, c(".jpg", ".jpeg", ".png", ".webp")))
  existing_files <- existing_files[file.exists(existing_files)]
  if (length(existing_files) > 0) {
    file.remove(existing_files)
  }
}

detect_extension <- function(image_url, response) {
  clean_url <- strsplit(image_url, "\\?")[[1]][1]
  extension <- tolower(tools::file_ext(clean_url))

  if (extension %in% c("jpg", "jpeg", "png", "webp")) {
    return(extension)
  }

  content_type <- headers(response)[["content-type"]]
  if (is.null(content_type)) return("jpg")

  if (str_detect(content_type, "png")) return("png")
  if (str_detect(content_type, "webp")) return("webp")
  if (str_detect(content_type, "jpeg") || str_detect(content_type, "jpg")) return("jpg")

  "jpg"
}

queue <- manifest %>%
  distinct(playerID, .keep_all = TRUE) %>%
  filter(!is.na(playerID), playerID != "", !is.na(image_url), image_url != "") %>%
  left_join(
    download_log %>%
      filter(status == "downloaded") %>%
      distinct(playerID, image_source, .keep_all = TRUE) %>%
      transmute(
        playerID,
        downloaded_image_source = image_source,
        already_downloaded_same_source = TRUE
      ),
    by = c("playerID", "image_source" = "downloaded_image_source")
  ) %>%
  mutate(
    image_source = if_else(is.na(image_source), "", image_source),
    force_refresh = image_source == "MLB",
    already_downloaded_same_source = if_else(is.na(already_downloaded_same_source), FALSE, already_downloaded_same_source)
  ) %>%
  filter(
    !already_downloaded_same_source
  ) %>%
  rowwise() %>%
  filter(force_refresh || !has_local_image(playerID)) %>%
  ungroup()

if (nrow(queue) == 0) {
  message("No remaining manifest photos to download.")
  quit(save = "no")
}

for (i in seq_len(nrow(queue))) {
  row <- queue[i, ]
  player_id <- row$playerID
  image_url <- row$image_url
  player_name <- row$name

  message("Downloading ", player_id, " (", i, "/", nrow(queue), ")")

  response <- GET(image_url)
  if (status_code(response) != 200) {
    stop("Could not download image URL: ", image_url)
  }

  extension <- detect_extension(image_url, response)
  file_name <- paste0(player_id, ".", extension)
  destination <- file.path("images", "players", file_name)

  remove_existing_local_images(player_id)
  writeBin(content(response, as = "raw"), destination)

  append_log(tibble(
    playerID = player_id,
    name = player_name,
    bbref_url = row$bbref_url,
    image_url = image_url,
    image_source = row$image_source,
    local_path = destination,
    status = "downloaded",
    file_name = file_name,
    timestamp = as.character(Sys.time())
  ))
}

message("Finished downloading manifest photos.")
