source("R/FUNCTIONS.R")

ct <- read_sheet(path = "Excel",
                 sheet = "Camera_trap",
                 na = c("", "na", "NA")) |>
  purrr::map(~ .x |>
               dplyr::mutate(
                 Structure_id = as.character(stringr::str_to_upper(Structure_id)),
                 .keep = "none"
               ))
# Check if there are blank rows on camera trap sheet
ct |>
  purrr::keep(~ nrow(.x) > 0) |> 
  head(2)

# Check duplicated names between underpasses and overpasses
under <- read_sheet(path = "Excel",
                    sheet = "Underpasses",
                    na = c("", "na", "NA")) |>
  purrr::map(~ .x |>
               dplyr::mutate(
                 Structure_id = as.character(stringr::str_to_upper(Structure_id)),
                 position = "under",
                 .keep = "none")) |>
  purrr::keep(~ all(nrow(.x) > 0))

over <- read_sheet(path = "Excel",
                   sheet = "Overpasses",
                   na = c("", "na", "NA")) |>
  purrr::map(~ .x |>
               dplyr::mutate(
                 Structure_id = as.character(stringr::str_to_upper(Structure_id)),
                 position = "over",
                 .keep = "none")) |>
  purrr::keep(~ nrow(.x) > 0)

head(under[1])
head(over[1])

# The next step is to merge underpasses and overpasses Structure_id from each dataset into a single dataframe.

# Binding under and over in one only list
under_over <- list()

for (i in names(ct)) {
  exists_in_under <- i %in% names(under)
  exists_in_over <- i %in% names(over)
  
  if (exists_in_under & exists_in_over) {
    under_over[[i]] <- dplyr::bind_rows(under[[i]], over[[i]])
  }
  else if (exists_in_under & exists_in_over == FALSE) {
    under_over[[i]] <- under[[i]]
  }
  else {
    under_over[[i]] <- over[[i]]
  }
}

head(under_over[1])

# check if there are duplicated names for Structure_id
under_over |>
  dplyr::bind_rows(.id = "Dataset") |>
  dplyr::count(Dataset, Structure_id, sort = TRUE) |>
  dplyr::filter

# Check if the structures in Camera Trap sheet are in either Underpasses and Overpasses sheet and vice-versa
ct_diff_under_over <- purrr::map(names(ct), function(x){
  
  a <- ct[[x]]
  uo <- under_over[[x]]
  
  base::setdiff(a$Structure_id, uo$Structure_id) |>
    tibble::enframe(name = "Erro", value = "Structure_id") |>
    dplyr::mutate(status = "no_under_nor_over",
                  position = "in_ct")
}) |>
  purrr::set_names(names(ct)) |>
  purrr::keep(~ nrow(.x) > 0) |> # discard datasets without errors
  dplyr::bind_rows(.id = "Dataset")

ct_diff_under_over |>
  print(n = Inf)

# Now vise-versa
under_over_diff_ct <- purrr::map(names(ct), function(x){
  
  a <- ct[[x]]
  uo <- under_over[[x]]
  
  base::setdiff(uo$Structure_id, a$Structure_id) |>
    tibble::enframe(name = "Erro", value = "Structure_id") |>
    dplyr::mutate(status = "not_in_ct") |>
    dplyr::left_join(under_over[[x]], by = "Structure_id")
}) |>
  purrr::set_names(names(ct)) |>
  purrr::keep(~ nrow(.x) > 0) |>
  dplyr::bind_rows(.id = "Dataset")

dplyr::bind_rows(ct_diff_under_over, under_over_diff_ct) |>
  dplyr::arrange(Dataset) |>
  print(n = Inf)

#Check if fences are present on underpasses
fences <- read_sheet(path = "Excel",
                     sheet = "Fencing",
                     na = "NA") |>
  purrr::map(~ .x |>
               dplyr::mutate(
                 Structure_id = as.character(stringr::str_to_upper(Structure_id)),
                 position = "under",
                 .keep = "none")) |>
  purrr::keep(~ nrow(.x) > 0)

#We then check if all the Structure_id listed on the Fencing spreadsheet are comprised on the Structure_id on the Underpasses spreadsheet.
fences_diff_under <- purrr::map(names(fences), function(x){
  
  a <- fences[[x]]
  u <- under[[x]]
  
  base::setdiff(a$Structure_id, u$Structure_id) |>
    tibble::enframe(name = "Erro", value = "Structure_id") |>
    dplyr::mutate(status = "no_under",
                  position = "in_fences")
}) |>
  purrr::set_names(names(fences)) |>
  purrr::keep(~ nrow(.x) > 0) |>
  dplyr::bind_rows(.id = "Dataset")

fences_diff_under |>
  print(n = Inf)

# We then proceed on the opposite direction
under_fences_diff <- purrr::map(names(fences), function(x){
  
  a <- fences[[x]]
  u <- under[[x]]
  
  base::setdiff(u$Structure_id, a$Structure_id) |>
    tibble::enframe(name = "Erro", value = "Structure_id") |>
    dplyr::mutate(status = "no_fences",
                  position = "in_under")
}) |>
  purrr::set_names(names(fences)) |>
  purrr::keep(~ nrow(.x) > 0) |>
  dplyr::bind_rows(.id = "Dataset")

under_fences_diff |>
  print(n = Inf)

# export it all as a table
ct_diff_under_over |>
openxlsx2::write_xlsx(
  sprintf("Output/CHECK_CT_DIFF_UNDER_%s.xlsx", today()), asTable = TRUE)

fences_diff_under |>
openxlsx2::write_xlsx(
  sprintf("Output/CHECK_FENCES_DIFF_UNDER_%s.xlsx", today()), asTable = TRUE)

under_fences_diff |> 
  openxlsx2::write_xlsx(
    sprintf("Output/CHECK_STRUCTURES_UNDER_FENCES_%s.xlsx", today()), asTable = TRUE)
