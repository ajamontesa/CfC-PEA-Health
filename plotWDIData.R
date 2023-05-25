library(tidyverse)
library(ggrepel)

source("loadData.R")


# Use to find variables
WDI %>%
  distinct(`Indicator Name`) %>%
  filter(str_detect(`Indicator Name`, "(M|m)ortality"))

## Variables for plotting
## GNI per capita, Atlas method (current US$)
## Life expectancy at birth, total (years)
## Maternal mortality ratio (modeled estimate, per 100,000 live births)
## Infant mortality rate (deaths per 1,000 live births

# ASEAN Countries
countries_ASEAN <- WDI %>%
  distinct(Country = `Country Name`, CC = `Country Code`) %>%
  filter(str_detect(Country, "Phil|Viet|Indo|Thai|Malay|Sing|Camb|Myan|Lao"))

countries_ASEAN2 <- WDI %>%
  distinct(Country = `Country Name`, CC = `Country Code`) %>%
  filter(str_detect(Country, "Phil|Viet|Indo|Thai|Malay|Camb"))

# WB Income Classifications
incomeClass_2020 <- c(1045, 4095, 12695)
incomeClass_2017 <- c(995, 3895, 12055)



# Life Expectancy vs GNI per Capita ---------------------------------------

WDI_LifeExpectancy <- WDI %>%
  filter(`Indicator Name` %in% c('GNI per capita, Atlas method (current US$)',
                                 'Life expectancy at birth, total (years)')) %>%
  select(Country = `Country Name`, CC = `Country Code`, Indicator = `Indicator Name`, `2020`) %>%
  mutate(Indicator = case_when(str_detect(Indicator, "GNI") ~ "GNIPC",
                               str_detect(Indicator, "Life exp") ~ "LifeExpectancy")) %>%
  pivot_wider(names_from = Indicator, values_from = `2020`) %>%
  mutate(ASEAN = Country %in% countries_ASEAN$Country)

ggplot(data = WDI_LifeExpectancy,
       aes(x = GNIPC, y = LifeExpectancy)) +
  geom_vline(xintercept = incomeClass_2020, lty = "dashed", linewidth = 0.15) +
  geom_smooth(method = 'lm', se = F, color = "gray60") +
  geom_point(aes(color = ASEAN, size = ASEAN)) +
  geom_smooth(data = WDI_LifeExpectancy %>% filter(Country %in% countries_ASEAN$Country),
              method = 'lm', se = F, color = "red2", linewidth = 0.5) +
  geom_text_repel(data = WDI_LifeExpectancy %>% filter(Country %in% countries_ASEAN$Country),
                  aes(label = CC), size = 4, color = "red2") +
  scale_x_continuous(trans = "log", breaks = c(220, incomeClass_2020), labels = scales::dollar_format(accuracy = 1)) +
  scale_color_manual(values = c("gray60", "red2")) +
  scale_size_manual(values = c(1.5, 2)) +
  coord_cartesian(ylim = c(50, 87.5)) +
  labs(title = "Life Expectancy vs Per Capita Income", caption = "Source: World Bank Data",
       x = "GNI per capita in Current US$ (log-scale) 2020", y = "Life expectancy at birth in years 2020") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - Life Expectancy.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)



# Maternal Mortality vs GNI per Capita ------------------------------------

WDI_MaternalMortality <- WDI %>%
  filter(`Indicator Name` %in% c('GNI per capita, Atlas method (current US$)',
                                 'Maternal mortality ratio (modeled estimate, per 100,000 live births)')) %>%
  select(Country = `Country Name`, CC = `Country Code`, Indicator = `Indicator Name`, `2017`) %>%
  mutate(Indicator = case_when(str_detect(Indicator, "GNI") ~ "GNIPC",
                               str_detect(Indicator, "Maternal") ~ "MaternalMortality")) %>%
  pivot_wider(names_from = Indicator, values_from = `2017`) %>%
  mutate(ASEAN = Country %in% countries_ASEAN$Country)

ggplot(data = WDI_MaternalMortality,
       aes(x = GNIPC, y = MaternalMortality)) +
  geom_vline(xintercept = incomeClass_2017, lty = "dashed", linewidth = 0.15) +
  geom_point(aes(color = ASEAN, size = ASEAN)) +
  geom_text_repel(data = WDI_MaternalMortality %>% filter(Country %in% countries_ASEAN$Country),
                  aes(label = CC), size = 4, color = "red2") +
  scale_x_continuous(trans = "log", breaks = c(220, incomeClass_2017), labels = scales::dollar_format(accuracy = 1)) +
  scale_color_manual(values = c("gray60", "red2")) +
  scale_size_manual(values = c(1.5, 2)) +
  coord_cartesian(ylim = c(0, 1250)) +
  labs(title = "Maternal Mortality vs Per Capita Income", caption = "Source: World Bank Data",
       x = "GNI per capita in Current US$ (log-scale) 2017", y = "Maternal mortality ratio (deaths per 100,000 live births 2017") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - Maternal Mortality.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)



# Infant Mortality vs GNI per Capita --------------------------------------

WDI_InfantMortality <- WDI %>%
  filter(`Indicator Name` %in% c('GNI per capita, Atlas method (current US$)',
                                 'Mortality rate, infant (per 1,000 live births)')) %>%
  select(Country = `Country Name`, CC = `Country Code`, Indicator = `Indicator Name`, `2020`) %>%
  mutate(Indicator = case_when(str_detect(Indicator, "GNI") ~ "GNIPC",
                               str_detect(Indicator, "infant") ~ "InfantMortality")) %>%
  pivot_wider(names_from = Indicator, values_from = `2020`) %>%
  mutate(ASEAN = Country %in% countries_ASEAN$Country)

ggplot(data = WDI_InfantMortality,
       aes(x = GNIPC, y = InfantMortality)) +
  geom_vline(xintercept = incomeClass_2020, lty = "dashed", linewidth = 0.15) +
  geom_point(aes(color = ASEAN, size = ASEAN)) +
  geom_text_repel(data = WDI_InfantMortality %>% filter(Country %in% countries_ASEAN$Country),
                  aes(label = CC), size = 4, color = "red2") +
  scale_x_continuous(trans = "log", breaks = c(220, incomeClass_2020), labels = scales::dollar_format(accuracy = 1)) +
  scale_color_manual(values = c("gray60", "red2")) +
  scale_size_manual(values = c(1.5, 2)) +
  coord_cartesian(ylim = c(0, 80)) +
  labs(title = "Infant Mortality vs Per Capita Income", caption = "Source: World Bank Data",
       x = "GNI per capita in Current US$ (log-scale) 2020", y = "Infant mortality rate (deaths per 1,000 live births 2020") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - Infant Mortality.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)



# Stunting Prevalence vs GNI per Capita -----------------------------------

WDI_StuntingPrevalence <- WDI %>%
  filter(`Indicator Name` %in% c('GNI per capita, Atlas method (current US$)',
                                 'Prevalence of stunting, height for age (modeled estimate, % of children under 5)')) %>%
  select(Country = `Country Name`, CC = `Country Code`, Indicator = `Indicator Name`, `2020`) %>%
  mutate(Indicator = case_when(str_detect(Indicator, "GNI") ~ "GNIPC",
                               str_detect(Indicator, "stunt") ~ "StuntingPrevalence")) %>%
  pivot_wider(names_from = Indicator, values_from = `2020`) %>%
  mutate(ASEAN = Country %in% countries_ASEAN$Country,
         StuntingPrevalence = StuntingPrevalence/100)

ggplot(data = WDI_StuntingPrevalence,
       aes(x = GNIPC, y = StuntingPrevalence)) +
  geom_vline(xintercept = incomeClass_2020, lty = "dashed", linewidth = 0.15) +
  geom_smooth(method = 'lm', se = F, color = "gray60") +
  geom_point(aes(color = ASEAN, size = ASEAN)) +
  geom_smooth(data = WDI_StuntingPrevalence %>% filter(Country %in% countries_ASEAN$Country),
              method = 'lm', se = F, color = "red2", linewidth = 0.5) +
  geom_text_repel(data = WDI_StuntingPrevalence %>% filter(Country %in% countries_ASEAN$Country),
                  aes(label = CC), size = 4, color = "red2") +
  scale_x_continuous(trans = "log", breaks = c(220, incomeClass_2020), labels = scales::dollar_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("gray60", "red2")) +
  scale_size_manual(values = c(1.5, 2)) +
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(title = "Stunting Prevalence vs Per Capita Income", caption = "Source: World Bank Data",
       x = "GNI per capita in Current US$ (log-scale) 2020",
       y = "Prevalence of stunting, height for age\n(modeled estimate, % of children under 5) 2020") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - Prevalence of Stunting.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)



# Nutrition vs GNI per Capita ---------------------------------------------

WDI_UnderweightWasting <- WDI %>%
  filter(`Indicator Name` %in% c('Prevalence of wasting, weight for height (% of children under 5)',
                                 'Prevalence of underweight, weight for age (% of children under 5)')) %>%
  select(Country = `Country Name`, CC = `Country Code`, Indicator = `Indicator Name`, `2010`:`2020`) %>%
  mutate(Indicator = case_when(str_detect(Indicator, "wast") ~ "WastingPrevalence",
                               str_detect(Indicator, "weight") ~ "UnderweightPrevalence")) %>%
  pivot_longer(cols = `2010`:`2020`, names_to = "Year", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  group_by(Country, Indicator) %>%
  mutate(Value = case_when(Year == max(Year) ~ Value)) %>%
  ungroup() %>%
  filter(!is.na(Value)) %>%
  pivot_wider(names_from = "Indicator", values_from = "Value") %>%
  transmute(Country, CC, Year,
            WastingPrevalence = WastingPrevalence/100, UnderweightPrevalence = UnderweightPrevalence/100)

WDI_Nutrition <- left_join(WDI_LifeExpectancy, WDI_UnderweightWasting)

ggplot(data = WDI_Nutrition,
       aes(x = GNIPC, y = WastingPrevalence)) +
  geom_vline(xintercept = incomeClass_2020, lty = "dashed", linewidth = 0.15) +
  geom_point(aes(color = ASEAN, size = ASEAN)) +
  geom_text_repel(data = WDI_Nutrition %>% filter(Country %in% countries_ASEAN$Country),
                  aes(label = str_c(CC, ", ", Year)), size = 4, color = "red2") +
  scale_x_continuous(trans = "log", breaks = c(220, incomeClass_2020), labels = scales::dollar_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("gray60", "red2")) +
  scale_size_manual(values = c(1.5, 2)) +
  coord_cartesian(ylim = c(0, 0.2)) +
  labs(title = "Wasting Prevalence vs Per Capita Income", caption = "Source: World Bank Data",
       x = "GNI per capita in Current US$ (log-scale) 2020",
       y = "Prevalence of wasting, weight for height\n(% of children under 5)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - Prevalence of Wasting.png", width = 4, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)

ggplot(data = WDI_Nutrition,
       aes(x = GNIPC, y = UnderweightPrevalence)) +
  geom_vline(xintercept = incomeClass_2020, lty = "dashed", linewidth = 0.15) +
  geom_point(aes(color = ASEAN, size = ASEAN)) +
  geom_text_repel(data = WDI_Nutrition %>% filter(Country %in% countries_ASEAN$Country),
                  aes(label = str_c(CC, ", ", Year)), size = 4, color = "red2") +
  scale_x_continuous(trans = "log", breaks = c(220, incomeClass_2020), labels = scales::dollar_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("gray60", "red2")) +
  scale_size_manual(values = c(1.5, 2)) +
  coord_cartesian(ylim = c(0, 0.4)) +
  labs(title = "Underweight Prevalence vs Per Capita Income", caption = "Source: World Bank Data",
       x = "GNI per capita in Current US$ (log-scale) 2020",
       y = "Prevalence of underweight, weight for age\n(% of children under 5)") +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - Prevalence of Underweight.png", width = 4, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)



# Time Series Plots -------------------------------------------------------

WDI_TS <- WDI %>%
  filter(`Indicator Name` %in% c('GNI per capita, Atlas method (current US$)',
                                 'Life expectancy at birth, total (years)'),
         `Country Name` %in% countries_ASEAN2$Country) %>%
  select(Country = `Country Name`, CC = `Country Code`, Indicator = `Indicator Name`, `1990`:`2020`) %>%
  mutate(Indicator = case_when(str_detect(Indicator, "GNI") ~ "GNIPC",
                               str_detect(Indicator, "Life exp") ~ "LifeExpectancy")) %>%
  pivot_longer(cols = `1990`:`2020`, names_to = "Year", values_to = "Value") %>%
  pivot_wider(names_from = Indicator, values_from = Value) %>%
  mutate(Year = as.Date(str_c(Year, "-01-01")))

ggplot(data = WDI_TS,
       aes(x = Year, y = GNIPC, group = CC, color = CC)) +
  geom_path(size = 1) +
  geom_text_repel(data = filter(WDI_TS, Year == as.Date("2020-01-01")),
                  aes(label = CC), nudge_x = 1) +
  scale_y_continuous(trans = "log", breaks = c(125, 250, 500, 1000, 2000, 4000, 8000), labels = scales::comma) +
  scale_color_manual(values = c("gray30", "gray60", "gray50", "red2", "gray50", "gray30")) +
  coord_cartesian(xlim = c(as.Date("1988-01-01"), as.Date("2022-01-01"))) +
  labs(title = "Per Capita Incomes of Selected ASEAN Countries (1990-2020)",
       caption = "Source: World Bank Data",
       x = "GNI per capita in Current US$ (log-scale) 2020", y = NULL) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - ASEAN Trend Per Capita Income.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)
