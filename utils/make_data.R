library(ncdf4)
library(tidyverse)
library(here)
library(rvest)
library(threadr)
library(config)

folder_path <- here(
  "dap.ceda.ac.uk",
  "badc",
  "ukcp18",
  "data",
  "land-cpm",
  "uk",
  "5km",
  "rcp85",
  "01",
  "pr",
  "1hr",
  "v20210615"
)

files <- folder_path |>
  list.files()

file_paths <- str_c(
  folder_path,
  files
)

file_dates <- files |>
  str_match("1hr_([0-9]{8})-([0-9]{8})")




d <- tibble(
  start_date = file_dates[, 2] |> as_factor(),
  end_date = file_dates[, 3] |> as_factor(),
  data = list(0)
)


for (i in seq_len(nrow(d))) {


  temp_d <- nc_open(file_paths[i])

  max_pr <- ncvar_get(temp_d, "pr") |>
    apply(MARGIN = c(1, 2), FUN = max)

  lat <- ncvar_get(temp_d, "latitude")
  long <- ncvar_get(temp_d, "longitude")

  dat <- crossing(
    proj_x = 1:180,
    proj_y = 1:244
  ) |>
    arrange(proj_y, proj_x) |>
    mutate(
      precip = as.numeric(max_pr),
      longitude = as.numeric(long),
      latitude = as.numeric(lat),
      station = row_number()
    )

  d$data[i] = list(dat)

  file.remove(file_path)

  if (i %% 10 == 0) write_rds(d, here("utils", "data", "hourly_maximum_montly.rds"))

  writeLines(
    str_c(
      "Finished reading file ", i, " of ", nrow(d)
    )
  )
}





