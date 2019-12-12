USA <- readRDS("data-raw/wvs-usa-3-6.rds")
Educatrmd <- readxl::read_xlsx("data-raw/wvs-usa-education-categories.xlsx") %>% dplyr::select(1:4, -n)

USA %>%
  left_join(., Educatrmd) -> USA
