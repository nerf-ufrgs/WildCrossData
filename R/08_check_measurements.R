library(tidyverse)

source("R/FUNCTIONS.R")

under <- read_sheet(sheet = "Underpasses", na = c("NA", "na"))

under |> 
  keep(~ "Structure_lenght" %in% names(.x))

all_under <- under |> 
  discard(~ nrow(.x) == 0) |> 
  map(\(x) select(x, Structure_type:Waterbody_width)) |> 
  bind_rows(.id = "Dataset")

summary(all_under)
