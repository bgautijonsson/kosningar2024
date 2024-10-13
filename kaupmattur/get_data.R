library(tidyverse)
library(hagstofa)
library(visitalaneysluverds)

url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Samfelag/launogtekjur/3_tekjur/1_tekjur_skattframtol/TEK01006.px"

heildartekjur <- hg_data(url) |> 
  filter(
    Kyn == "Allir",
    str_detect(Eining, "%|Meðaltal"),
    Aldur %in% c("16 - 24 ára", "25 - 54 ára", "55 - 74 ára", "75 ára og eldri")
  ) |> 
  collect() |> 
  janitor::clean_names() |> 
  rename(
    tekjur = 5
  ) |> 
  select(-kyn) |> 
  pivot_wider(names_from = eining, values_from = tekjur) |> 
  pivot_longer(c(contains("%")), names_to = "eining", values_to = "tekjur") |> 
  mutate(
    ar = parse_number(ar),
    eining = parse_number(eining),
    tegund = "heildartekjur"
  ) |> 
  janitor::clean_names() |> 
  rename(
    medaltal = medaltal_skilyrt
  ) |> 
  select(ar, aldur, eining, tegund, everything()) 

url <- "https://px.hagstofa.is:443/pxis/api/v1/is/Samfelag/launogtekjur/3_tekjur/1_tekjur_skattframtol/TEK01007.px"

atvinnutekjur <- hg_data(url) |> 
  filter(
    Kyn == "Allir",
    str_detect(Eining, "%|Meðaltal"),
    Aldur %in% c("16 - 24 ára", "25 - 54 ára", "55 - 74 ára", "75 ára og eldri")
  ) |> 
  collect() |> 
  janitor::clean_names() |> 
  rename(
    tekjur = 5
  ) |> 
  select(-kyn) |> 
  pivot_wider(names_from = eining, values_from = tekjur) |> 
  pivot_longer(c(contains("%")), names_to = "eining", values_to = "tekjur") |> 
  mutate(
    ar = parse_number(ar),
    eining = parse_number(eining),
    tegund = "atvinnutekjur"
  ) |> 
  janitor::clean_names() |> 
  rename(
    medaltal = medaltal_skilyrt
  ) |> 
  select(ar, aldur, eining,tegund, everything())  

d <- bind_rows(
  heildartekjur,
  atvinnutekjur
) |> 
  mutate(
    tekjur = vnv_convert(tekjur, obs_date = ar),
    medaltal = vnv_convert(medaltal, obs_date = ar)
  ) |> 
  filter(eining != 95)


write_csv(
  d,
  here::here("kaupmattur", "data", "tekjur_aldur_tiund.csv")
)
