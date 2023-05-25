library(readxl)
library(readr)

if (!dir.exists("Global-Data")) {dir.create("Global-Data")}


# WB World Development Indicators -----------------------------------------

# Download WDI Dataset
download.file(url = "http://databank.worldbank.org/data/download/WDI_csv.zip",
              destfile = "Global-Data/WDI_csv.zip",
              method = "libcurl", quiet = F)

# Extract WDI Dataset
unzip(zipfile = "Global-Data/WDI_csv.zip", exdir = "Global-Data/WDI-Data/", overwrite = T)

list.files("Global-Data/WDI-Data/")

# Load WDI Dataset
# WDI <- read_csv("International-Data/WDI-Data/WDIData.csv")


# UNDP Human Development Index --------------------------------------------

download.file(url = "https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Statistical_Annex_HDI_Table.xlsx",
              destfile = "Global-Data/HDR2122.xlsx",
              method = "libcurl", quiet = F)

#hdi_cols <- c("HDIRank", "Country", "HDI", "Note1", "LifeExpectancy", "Note2",
#              "ExpectedSchooling", "Note3", "MeanSchooling", "Note4", "GNIPC_2017PPP")

# Load HDI Dataset
# HDI2021 <- rbind(
#  read_xlsx("Global-Data/HDR2122.xlsx", range = "A9:K74", col_names = hdi_cols),
#  read_xlsx("Global-Data/HDR2122.xlsx", range = "A76:K124", col_names = hdi_cols),
#  read_xlsx("Global-Data/HDR2122.xlsx", range = "A126:K169", col_names = hdi_cols),
#  read_xlsx("Global-Data/HDR2122.xlsx", range = "A171:K202", col_names = hdi_cols)
#)

