#' Convenience function to download HadUK data from CEDA Archive
#'
#' This takes aroung 3H30M to run, so don't do it lightly.
#' The function saves two parquet files in the workding directory that contain
#' 1) Monthly maxima 2) Yearly maxima
#'
#' The function assumes there are variables CEDA_USR and CEDA_PWD in the R.environ file.
#'
#' @return A tibble containing yearly maxima for each location and each year
#' @export
download_raw_data <- function() {

  if (Sys.getenv("CEDA_USR") == "" | Sys.getenv("CEDA_PWD") == "") {
    stop("Need variables CEDA_USR and CEDA_PWD in your R.environ file.")
  }

  url <- "ftp://ftp.ceda.ac.uk/badc/ukcp18/data/land-cpm/uk/5km/rcp85/01/pr/1hr/v20210615/"

  filenames <- RCurl::getURL(
    url,
    userpwd = userpwd(),
    dirlistonly = TRUE
  )

  files <- filenames |>
    stringr::str_split_1(pattern = "\n")

  files <- files[-length(files)]

  d <- files |>
    purrr::map_dfr(process_data, .progress = TRUE)

  d |>
    arrow::write_parquet("montly_data.parquet")

  d <- d |>
    mutate(year = year(from_date)) |>
    filter(year > 1980) |>
    group_by(year, station, proj_x, proj_y, longitude, latitude) |>
    summarise(
      precip = max(precip),
      .groups = "drop"
    )

  d |>
    arrow::write_parquet("yearly_data.parquet")


  d
}

#' Helper function to process datafiles downloaded from CEDA Archive
#'
#' @param filename
#'
#' @return
process_data <- function(filename) {

  Sys.sleep(0.1)

  from_to <- stringr::str_extract_all(filename, "_[0-9]{8}-[0-9]{8}")[[1]] |>
    stringr::str_replace("_", "") |>
    stringr::str_split_1("-")

  from <- as.Date(from_to[1], format = "%Y%m%d")
  to <- from + lubridate::months(1, abbreviate = FALSE) - lubridate::days(1)

  tmp <- tempfile()

  download.file(
    make_download_path(filename),
    tmp,
    mode = "wb",
    quiet = TRUE
  )

  temp_d <- ncdf4::nc_open(tmp)

  max_pr <- ncdf4::ncvar_get(temp_d, "pr") |>
    apply(MARGIN = c(1, 2), FUN = max)

  lat <- ncdf4::ncvar_get(temp_d, "latitude")
  long <- ncdf4::ncvar_get(temp_d, "longitude")

  out <- tidyr::crossing(
    proj_x = 1:180,
    proj_y = 1:244,
    from_date = from,
    to_date = to
  ) |>
    dplyr::arrange(proj_y, proj_x) |>
    dplyr::mutate(
      precip = as.numeric(max_pr),
      longitude = as.numeric(long),
      latitude = as.numeric(lat),
      station = dplyr::row_number()
    )

  out
}


#' Helper function to insert CEDA FTP username and password into download URLs
#'
#' @param filename
#'
#' @return
make_download_path <- function(filename) {
  url |>
    stringr::str_replace("//", str_c("//", userpwd(), "@")) |>
    stringr::str_c(filename)
}

#' Helper function to get CEDA FTP log in info from R.environ
#'
#' @return
userpwd <- function() {
  stringr::str_c(Sys.getenv("CEDA_USR"), Sys.getenv("CEDA_PWD"), sep = ":")
}
