# Extrai problemas de listas após utilização de funções de segurança (safely, quietly)
extract_issue <- function(x, component) {
  nms <- x %>%
    purrr::map(names) %>%
    purrr::reduce(union)

  trans <- x %>%
    purrr::transpose(.names = nms)

  name <- c("Dataset", component)

  trans %>%
    purrr::pluck(component) %>%
    plyr::ldply(., .fun = data.frame, .id = "Dataset") %>% # transforma o resultado de listas para data frame
    purrr::set_names(name) %>%
    tibble::as_tibble()
}


# Cria um ID único para colunas que possuam IDs duplicados
id_unico <- suppressMessages(
  function(x, sep = "_") {
    x %<>%
      dplyr::mutate(Cod = stringr::str_c(row.names(.), ID_Local_Camera))

    # z <- c(1:30)

    x %<>%
      janitor::get_dupes(ID_Local_Camera) %>%
      dplyr::group_by(ID_Local_Camera) %>%
      dplyr::mutate(
        ID_dup = dplyr::row_number(ID_Local_Camera),
        nome_formulario_dup = stringr::str_c(
          ID_Local_Camera,
          sep,
          LETTERS[ID_dup]
        )
      ) %>%
      dplyr::select(Cod, nome_formulario_dup) %>%
      dplyr::left_join(x, .) %>%
      dplyr::mutate(
        ID_Local_Camera = dplyr::if_else(
          !is.na(nome_formulario_dup),
          nome_formulario_dup,
          ID_Local_Camera
        )
      ) %>%
      dplyr::select(-Cod, -nome_formulario_dup)
  }
)

set_column_types <- function(sheet = sheet) {
  readxl::read_excel(
    path = "support/column_types.xlsx",
    sheet = sheet
  ) |>
    dplyr::select(2) |>
    # dplyr::slice_head(n = 36) |>
    dplyr::pull()
}

read_sheet <- function(
  path = "Example",
  sheet = NULL,
  na = "",
  results = TRUE
) {
  excel <- list.files(
    path = path,
    pattern = "^\\w.+xlsx$",
    full.names = TRUE,
    recursive = TRUE
  )

  names <- excel |>
    stringr::str_split("/|\\.") |>
    purrr::map_vec(2)

  load <- excel |>
    purrr::set_names(names)

  if (!results) {
    return(load)
  }

  column_types <- set_column_types(sheet = sheet)

  result <- load |>
    purrr::map(function(file) {
      df <- withCallingHandlers(
        readxl::read_xlsx(
          path = file,
          sheet = sheet,
          na = na,
          col_names = TRUE,
          col_types = column_types
        )
      ) |>
        janitor::remove_empty("rows")

      names(df) <- df |>
        janitor::clean_names() |>
        colnames() |>
        stringr::str_to_sentence()

      return(df)
    })

  return(result)
}

# Cria um ID único para colunas que possuam IDs duplicados
unique_id <- function(x, sep = "_") {
  x_with_id <- x |>
    tibble::rowid_to_column()

  x_with_id |>
    dplyr::group_by(Structure_id, Camera_id) |>
    dplyr::add_count(Structure_id, Camera_id, name = "double") |>
    dplyr::mutate(
      Camera_id_orig = Camera_id,
      Dup_id = dplyr::row_number(Camera_id),
      Dup_form_name = dplyr::if_else(
        condition = double == 1,
        true = Camera_id,
        false = stringr::str_c(Camera_id, sep, LETTERS[Dup_id])
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(rowid, Camera_id_orig, Dup_form_name, double) |>
    dplyr::left_join(x_with_id, ., by = "rowid") |>
    dplyr::mutate(
      Camera_id = dplyr::if_else(
        !is.na(Dup_form_name),
        Dup_form_name,
        Camera_id
      )
    ) |>
    dplyr::relocate(Camera_id_orig, .after = Camera_id) |>
    dplyr::select(-Dup_form_name, -rowid)
}

dttm_update <- function(x, date_col, time_col) {
  # transforma as strings em símbolos
  date_sym <- rlang::sym(date_col)
  time_sym <- rlang::sym(time_col)

  x |>
    dplyr::mutate(
      # cria/atualiza a coluna date_col
      !!date_sym := lubridate:::update_datetime(
        !!date_sym,
        hour = lubridate::hour(!!time_sym),
        minute = lubridate::minute(!!time_sym),
        second = lubridate::second(!!time_sym)
      )
    )
}

add_epsg <- function(df) {
  df |>
    mutate(
      epsg = case_when(
        # geodésicos
        type == "Geodetic" & Datum == "WGS84" ~ 4326L,
        type == "Geodetic" & Datum == "SIRGAS2000" ~ 4674L,
        type == "Geodetic" & Datum == "Corrego_Alegre" ~ 5524L,
        type == "Geodetic" & Datum == "SAD69" ~ 4618L,
        # projetados WGS84 / UTM
        type == "Projected" & Datum == "WGS84" & hemis == "N" ~
          32600L + as.integer(zone),
        type == "Projected" & Datum == "WGS84" & hemis == "S" ~
          32700L + as.integer(zone),
        # projetados SIRGAS2000 / UTM (sul)
        type == "Projected" & Datum == "SIRGAS2000" & hemis == "S" ~
          31960L + as.integer(zone),
        # fallback para qualquer outra combinação
        TRUE ~ NA_integer_
      )
    )
}

calc_nearest_osm_dist <- function(
  x,
  feature = c("highway", "railway", "man_made"),
  crs_metric = 3857,
  buffer = 1000,
  thresh = 50
) {
  feature <- match.arg(feature)

  # 2) Define bbox com um buffer (em graus) para não puxar todo o mundo
  bb <- x |>
    sf::st_transform(crs_metric) |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_buffer(buffer) |>
    sf::st_transform(4326) |>
    sf::st_bbox()

  # 3) Consulta OSM para linhas do tipo escolhido
  osm_query <- osmdata::opq(bbox = bb) |>
    osmdata::add_osm_feature(
      key = feature,
      # para rodovia: MOTORWAY, PRIMARY, etc.
      value = if (feature == "highway") {
        c(
          "motorway",
          "trunk",
          "primary",
          "secondary",
          "tertiary",
          "unclassified",
          "residential"
        )
      } else if (feature == "railway") {
        c("rail", "narrow_gauge", "disused", "abandoned")
      } else {
        c("pipeline", "goods_conveyor")
      }
    )

  osm_lines <- osm_query |>
    osmdata::osmdata_sf() |>
    purrr::pluck("osm_lines")

  if (is.null(osm_lines)) {
    cli::cli_alert("There are no features within the buffer")
  }

  osm_lines_sf <- osm_lines |>
    tibble::rownames_to_column("id_osm") |>
    tibble::rowid_to_column("rowid") |>
    sf::st_as_sf()

  # 4) Transforma tudo para um CRS métrico (Web Mercator ou UTM local)
  pts_m <- sf::st_transform(x, crs_metric)
  lines_m <- sf::st_transform(osm_lines_sf, crs_metric)

  # 5) Para cada ponto, encontra o índice da linha mais próxima
  idx_nearest <- sf::st_nearest_feature(pts_m, lines_m)

  df <- x |>
    dplyr::mutate(idx_nearest = idx_nearest) |>
    dplyr::inner_join(
      osm_lines_sf |> sf::st_drop_geometry(),
      by = c("idx_nearest" = "rowid")
    )

  # 6) Calcula a distância “by element” ponto ↔ sua linha mais próxima
  dists <- sf::st_distance(pts_m, lines_m[idx_nearest, ], by_element = TRUE)

  # 7) Retorna o df original acrescido de distância em metros
  final_data <- df |>
    dplyr::mutate(
      distance_to = as.numeric(dists),
      out_thresh = distance_to > thresh
    )

  # --- novo: monta o objeto S3 ---
  result <- list(
    data = final_data,

    bbox_buffer = bb |>
      sf::st_as_sfc() |>
      tibble::enframe("id", "geometry") |>
      sf::st_as_sf(),

    osm_lines = osm_lines_sf |>
      dplyr::filter(rowid %in% unique(idx_nearest))
  )
  class(result) <- "nearest_osm_dist"
  return(result)
}

set_feature_from_infrastructure <- function(df) {
  result <- df |>
    mutate(
      feature = case_when(
        is.na(Infrastructure_type) ~ NA_character_,
        Infrastructure_type %in% c("Ducto", "Gasoduto") ~ "man_made",
        str_detect(Infrastructure_type, "Ferro") ~ "railway",
        TRUE ~ "highway"
      )
    )

  return(result)
}
