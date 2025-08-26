library(tidyverse)

source("R/FUNCTIONS.R")

# Read under
under <- read_sheet(sheet = "Underpasses", na = c("NA", "na"))

# Checking if there are datasets that filled with decimal degrees and UTM Zone
under |> 
  map(~ .x |> filter(!is.na(Latitude), !is.na(Utm_zone))) |> 
  discard(~ nrow(.x) == 0) |> 
  names()

# Checking if there are datasets that filled UTM and did not filled UTM Zone
under |> 
  map(~ .x |> filter(!is.na(X_easting), is.na(Utm_zone))) |> 
  discard(~ nrow(.x) == 0) |> 
  names()


under_df <- bind_rows(under, .id = "Dataset") |> 
  mutate(Position = "Under") |> 
  select(1:3, Position, Utm_zone, Datum)

# Read over
over <- read_sheet(sheet = "Overpasses", na = c("NA", "na"))

# Checking if there are datasets that filled with decimal degrees and UTM Zone
over |> 
  map(~ .x |> filter(!is.na(Latitude), !is.na(Utm_zone))) |> 
  discard(~ nrow(.x) == 0) |> 
  names()

# Checking if there are datasets that filled UTM and did not filled UTM Zone
over |> 
  map(~ .x |> filter(!is.na(X_easting), is.na(Utm_zone))) |> 
  discard(~ nrow(.x) == 0) |> 
  names()

over |> 
  map(~ .x |> filter(!is.na(X_easting))) |> 
  discard(~ nrow(.x) == 0) |> 
  map(~ .x |> select(ends_with("ing")))

over_df <- bind_rows(over, .id = "Dataset") |> 
  mutate(Position = "Over") |> 
  select(1:3, Position, Utm_zone, Datum)

# Finding problems in Utm_zone
under_df |> 
  bind_rows(over_df) |> 
  filter(!is.na(Utm_zone)) |> 
  filter(!str_detect(Utm_zone, "^\\d{2}[A-Za-z]$")) |> 
  count(Dataset, Position, Utm_zone) |> 
  print(n = Inf)


# Finding problems in Datum
under_df |> 
  bind_rows(over_df) |> 
  count(Dataset, Position, Datum, Utm_zone) |> 
  arrange(Datum, Dataset) |> 
  print(n = Inf)

# Distinct of different Datum + Utm_zone
uo_datum_zone <- bind_rows(under, .id = "Dataset") |> 
  bind_rows(bind_rows(over, .id = "Dataset")) |> 
  filter(!is.na(Datum)) |> 
  mutate(
    type = if_else(!is.na(Utm_zone), "Projected", "Geodetic"),
    zone = str_sub(Utm_zone, 1, 2),
    hemis = case_when(
      str_sub(Utm_zone, 3, 3) == "S" ~ "S",
      str_sub(Utm_zone, 3, 3) == "N" ~ "N",
      str_sub(Utm_zone, 3, 3) >= "N" ~ "N",
      str_sub(Utm_zone, 3, 3) < "N" ~ "S"
    )) |> 
  add_epsg()

nested_uo_datum_zone <- uo_datum_zone |> 
  split(~ epsg)

epsg_uo_datum_zone <- list()

for (epsg in names(nested_uo_datum_zone)) {
  
  message(glue::glue("Starting epsg {epsg}\n"))
  
  epsg_uo_datum_zone[[epsg]] <- nested_uo_datum_zone[[epsg]] |> 
    mutate(X = case_when(!is.na(Longitude) ~ Longitude,
                         TRUE ~ as.numeric(X_easting)),
           Y = case_when(!is.na(Latitude) ~ Latitude,
                         TRUE ~ as.numeric(Y_northing))) |> 
    sf::st_as_sf(coords = c("X", "Y"),
                 remove = FALSE,
                 crs = as.numeric(epsg)) |>
    sf::st_transform(4326) %>%
    mutate(Lat = sf::st_coordinates(.)[,2], #cria coluna com a latitude
           Long = sf::st_coordinates(.)[,1], #cria coluna com a latitude
           New_Datum = "WGS 84", #altera a coluna datum para WGS 84
           New_EPSG = 4326L)
}

epsg_uo_datum_zone_bind <- epsg_uo_datum_zone |> 
  bind_rows(.id = "epsg") |> 
  arrange(Dataset) |> 
  set_feature_from_infrastructure()

nested_epsg_uo_datum_zone_bind <- epsg_uo_datum_zone_bind |> 
  split(~ feature)

osm_result <- list()

for (feature in names(nested_epsg_uo_datum_zone_bind)) {
  cli::cli_h1("Starting feature {feature}")
  
  df <- nested_epsg_uo_datum_zone_bind[[feature]]
  
  datasets <- df |> 
    dplyr::distinct(Dataset) |> 
    dplyr::pull(Dataset)
  
  for (dataset in datasets) {
    cli::cli_h3("Starting dataset {dataset}")
    
    result <- try({ nested_epsg_uo_datum_zone_bind[[feature]] |> 
        dplyr::filter(Dataset == dataset) |> 
        calc_nearest_osm_dist(feature = feature)
    }, silent = TRUE)
    
    if (inherits(result, "try-error")) {
      cli::cli_alert_danger("Error in dataset {dataset}. Skipping to next.")
      osm_result[[feature]][[dataset]] <- "Error"
      next
    }
    else
      osm_result[[feature]][[dataset]] <- result
  }
  cli::cli_alert_success("Finishing feature {feature}")
}



a <- osm_result$railway$ReginaldoCruz$data
b <- osm_result$railway$ReginaldoCruz$bbox_buffer
c <- osm_result$railway$ReginaldoCruz$osm_lines

mapview::mapview(a) +
  mapview::mapview(b) +
  mapview::mapview(c)

x <- osm_result$man_made$Tremie_Peru_Pagoreni_Gregory
y <- osm_result$railway$Guillermo_Gil

epsg_uo_datum_zone_bind

epsg_uo_datum_zone_bind |> 
  split(~ feature)

epsg_uo_datum_zone <- list()

epsg_uo_datum_zone |> 
  bind_rows(.id = "epsg") |> 
  sf::st_drop_geometry() |> 
  count(Dataset, Infrastructure_type, epsg) |> 
  set_feature_from_infrastructure() |> 
  print(n = Inf)

bla <- epsg_uo_datum_zone[["32718"]]

df <- bla |> 
  filter(str_detect(Dataset, "Tremie"))



mapview::mapview(x, zcol = "dist_to_man_made") +
  mapview::mapview(bb) +
  mapview::mapview(osm_lines_sf)

lero <- epsg_uo_datum_zone |> 
  bind_rows(.id = "epsg") 

lero |> 
  calc_nearest_osm_dist(feature = c("man_made"))

install.packages("cli", )