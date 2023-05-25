library(tidyverse)
library(readxl)
library(tabulizer)


# 2015 HDI Data -----------------------------------------------------------

hdi2015_1 <- rbind(
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 136) %>%
    .[[1]] %>% .[2:40, 1:7],
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 138) %>%
    .[[1]] %>% .[2:42, 1:7]
)

hdi2015_2 <- rbind(
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 137) %>%
    .[[1]] %>% .[2:39, 2:9],
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 137) %>%
    .[[1]] %>% .[40, 1:8],
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 139) %>%
    .[[1]] %>% .[2:42, 2:9]
)

hdn2015_cols1 <- extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 136) %>%
  .[[1]] %>% .[1, 1:7]

hdn2015_cols2 <- extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 137) %>%
  .[[1]] %>% .[1, 2:9]


hdi2015 <- cbind(hdi2015_1, hdi2015_2)

colnames(hdi2015) <- c(hdn2015_cols1, hdn2015_cols2)

HDI2015_PH <- as_tibble(hdi2015) %>%
  mutate(across(.cols = -Province, .fns = ~as.double(str_remove_all(.x, ","))))

rm(list = ls(pattern = "hd"))


# 2012 HDI Data -----------------------------------------------------------

hdi2012_1 <- rbind(
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 140) %>%
    .[[1]] %>% .[2:40, 1:7],
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 142) %>%
    .[[1]] %>% .[2:42, 1:7]
)

hdi2012_2 <- rbind(
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 141) %>%
    .[[1]] %>% .[2:39, 2:9],
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 141) %>%
    .[[1]] %>% .[40, 1:8],
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 143) %>%
    .[[1]] %>% .[2:42, 2:9]
)

hdn2012_cols1 <- extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 140) %>%
  .[[1]] %>% .[1, 1:7]

hdn2012_cols2 <- extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 141) %>%
  .[[1]] %>% .[1, 2:9]


hdi2012 <- cbind(hdi2012_1, hdi2012_2)

colnames(hdi2012) <- c(hdn2012_cols1, hdn2012_cols2)

HDI2012_PH <- as_tibble(hdi2012) %>%
  mutate(across(.cols = -Province, .fns = ~as.double(str_remove_all(.x, ","))))

rm(list = ls(pattern = "hd"))



# Provincial Gini Coefficients --------------------------------------------

gini <- rbind(
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 188) %>%
    .[[1]] %>% .[2:41, ],
  extract_tables("Local-Data/2020-2021_Philippine_Human_Development_Report_full.pdf", pages = 189) %>%
    .[[1]] %>% .[2:41, ]
)
colnames(gini) <- c("Province", "Gini2003", "Gini2006", "Gini2009", "Gini2012", "Gini2015")

Gini_PH <- as_tibble(gini) %>%
  mutate(across(.cols = -Province, .fns = ~as.double(str_remove_all(.x, ","))))

rm(list = ls(pattern = "gini"))

