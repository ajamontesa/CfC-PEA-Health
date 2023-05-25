

GBD <- read_csv("Global-Data/IHME-GBD/IHME-GBD_2019_DATA-c03c0826-1.csv") %>%
  arrange(measure_id, metric_id, sex_id, age_id, location_id, year) %>%
  mutate(year = as.Date(str_c(year, "-01-01")))


PH_WBincome <- c("Philippines", "World Bank Low Income", "World Bank Lower Middle Income",
                 "World Bank Upper Middle Income", "World Bank High Income")

PH_Comparison <- c("Philippines", "Viet Nam", "Indonesia", "World Bank Lower Middle Income")

causes1 <- c("Cardiovascular diseases", "Neoplasms", "Diabetes and kidney diseases",
             "Chronic respiratory diseases", "Digestive diseases")

causes2 <- c("Respiratory infections and tuberculosis", "Enteric infections", "Other infectious diseases",
             "HIV/AIDS and sexually transmitted infections", "Neglected tropical diseases and malaria")

causes3 <- c("Maternal and neonatal disorders", "Self-harm and interpersonal violence", "Unintentional injuries",
             "Transport injuries", "Nutritional deficiencies", "Substance use disorders")

GBD %>% distinct(measure_name)

GBD %>% filter(location_name == "Philippines",
               year == as.Date("2019-01-01"),
               measure_name == "Deaths") %>%
  arrange(desc(val)) %>%
  select(cause_name, val) %>% head(20)

GBD %>%
  filter(measure_name == "Deaths",
         #measure_name == "DALYs (Disability-Adjusted Life Years)",
         location_name %in% PH_Comparison) %>%
  filter(cause_name %in% causes1) %>%
  mutate(cause_name = factor(cause_name, levels = causes1)) %>%
  ggplot(aes(x = year, y = val, color = location_name, lty = location_name)) +
  geom_path() +
  facet_wrap(~cause_name, scales = "free_y") +
  scale_color_manual(values = c("gray50", "red", "gray30", "black")) +
  scale_linetype_manual(values = c(1, 1, 1, 2)) +
  labs(title = "Mortality Rates of Selected Non-Communicable Diseases",
       x = NULL, y = "Deaths per 100,000 population",
       color = NULL, lty = NULL) +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - Burden of Disease 02.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)


GBD %>%
  filter(measure_name == "Deaths",
         #measure_name == "DALYs (Disability-Adjusted Life Years)",
         location_name %in% PH_Comparison) %>%
  filter(cause_name %in% causes2) %>%
  mutate(cause_name = factor(cause_name, levels = causes2)) %>%
  ggplot(aes(x = year, y = val, color = location_name, lty = location_name)) +
  geom_path() +
  facet_wrap(~cause_name, scales = "free_y") +
  scale_color_manual(values = c("gray50", "red", "gray30", "black")) +
  scale_linetype_manual(values = c(1, 1, 1, 2)) +
  labs(title = "Mortality Rates of Selected Communicable Diseases",
       caption = "Source: Global Burden of Disease Study (2019)",
       x = NULL, y = "Deaths per 100,000 population",
       color = NULL, lty = NULL) +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - Burden of Disease 01.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)


GBD %>%
  filter(measure_name == "Deaths",
         #measure_name == "DALYs (Disability-Adjusted Life Years)",
         location_name %in% PH_Comparison) %>%
  filter(cause_name %in% causes3) %>%
  mutate(cause_name = factor(cause_name, levels = causes3)) %>%
  ggplot(aes(x = year, y = val, color = location_name, lty = location_name)) +
  geom_path() +
  facet_wrap(~cause_name, scales = "free_y") +
  scale_color_manual(values = c("gray50", "red", "gray30", "black")) +
  scale_linetype_manual(values = c(1, 1, 1, 2)) +
  labs(title = "Mortality Rates of Selected Diseases",
       caption = "Source: Global Burden of Disease Study (2019)",
       x = NULL, y = "Deaths per 100,000 population",
       color = NULL, lty = NULL) +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 16))

ggsave("Plots/Global - Burden of Disease 03.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)
