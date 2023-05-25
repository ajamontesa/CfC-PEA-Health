library(tidyverse)
library(ggrepel)

source("extractHDNData.R")



# 2015 Life Expectancy vs Per Capita Income -------------------------------

ggplot(data = HDI2015_PH,
       aes(x = `Per Capita Income\r(NCR 2015 PPP\rPesos) 2015`,
           y = `Life Expectancy at\rbirth (years) 2015`)) +
  geom_smooth(method = "lm", se = F, color = "gray25") +
  geom_point() +
  geom_text_repel(aes(label = Province), size = 3) +
  scale_x_continuous(trans = "log", breaks = c(25e3, 35e3, 50e3, 70e3, 100e3),
                     labels = scales::comma(c(25e3, 35e3, 50e3, 70e3, 100e3))) +
  coord_cartesian(ylim = c(60, 75), xlim = c(NA, 100e3)) +
  labs(title = "Life Expectancy vs Per Capita Income",
       subtitle = "Provinces of the Philippines",
       caption = "Source: Philippine Human Development Network",
       x = "Per Capita Income (NCR 2015 PPP Pesos) 2015",
       y = "Life Expectancy at birth (years) 2015") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12))

ggsave("Plots/Local - Life Expectancy 2015.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)
ggsave("Plots/Local - Life Expectancy 2015 v2.png", width = 4, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)



# 2012 Life Expectancy vs Per Capita Income -------------------------------

ggplot(data = HDI2012_PH,
       aes(x = `Per Capita Income\r(NCR 2015 PPP\rPesos) 2012`,
           y = `Life Expectancy at\rbirth (years) 2012`)) +
  geom_smooth(method = "lm", se = F, color = "gray25") +
  geom_point() +
  geom_text_repel(aes(label = Province), size = 3) +
  scale_x_continuous(trans = "log", breaks = c(25e3, 35e3, 50e3, 70e3, 100e3),
                     labels = scales::comma(c(25e3, 35e3, 50e3, 70e3, 100e3))) +
  coord_cartesian(ylim = c(60, 75), xlim = c(NA, 100e3)) +
  labs(title = "Life Expectancy vs Per Capita Income",
       subtitle = "Provinces of the Philippines",
       caption = "Source: Philippine Human Development Network",
       x = "Per Capita Income (NCR 2015 PPP Pesos) 2012",
       y = "Life Expectancy at birth (years) 2012") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12))

ggsave("Plots/Local - Life Expectancy 2012.png", width = 6, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)
ggsave("Plots/Local - Life Expectancy 2012 v2.png", width = 4, height = 4, units = "in", dpi = 300, bg = "white", scale = 1.5)
