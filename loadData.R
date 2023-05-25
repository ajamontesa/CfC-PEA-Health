library(tidyverse)
library(readxl)
library(tabulizer)

# Load World Bank WDI Dataset
print("Load World Bank WDI Dataset")

WDI <- read_csv("Global-Data/WDI-Data/WDIData.csv")


# Load UNDP HDI Dataset
print("Load UNDP HDI Dataset")

hdi_cols <- c("HDIRank", "Country", "HDI", "Note1", "LifeExpectancy", "Note2",
              "ExpectedSchooling", "Note3", "MeanSchooling", "Note4", "GNIPC_2017PPP")

HDI2021 <- rbind(
  read_xlsx("Global-Data/HDR2122.xlsx", range = "A9:K74", col_names = hdi_cols),
  read_xlsx("Global-Data/HDR2122.xlsx", range = "A76:K124", col_names = hdi_cols),
  read_xlsx("Global-Data/HDR2122.xlsx", range = "A126:K169", col_names = hdi_cols),
  read_xlsx("Global-Data/HDR2122.xlsx", range = "A171:K202", col_names = hdi_cols)
)


# Load HDN Dataset
print("Load HDN Dataset")

source("extractHDNData.R")
