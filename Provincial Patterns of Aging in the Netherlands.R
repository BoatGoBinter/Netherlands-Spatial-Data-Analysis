library(tidyverse)
library(sf)
library(tmap)
library(readr)

pop_csv <- "https://drive.google.com/uc?export=download&id=18K6cuNFoosLUOn5nVbWw-ubpXZRxtlCJ"
prov_geojson <- "https://drive.google.com/uc?export=download&id=1UMOJRm82Y8Xrbc1BQsHoxCNVpbx9Xe1Q"

p_pop <- read_csv(pop_csv)
p_map <- st_read(prov_geojson)

p_pop_clean <- p_pop |>
  rename(
    provincienaam = Provincienaam,
    pct_0_15 = `% 0 tot 15 jaar`,
    pct_15_25 = `% 15 tot 25 jaar`,
    pct_25_45 = `% 25 tot 45 jaar`,
    pct_45_65 = `% 45 tot 65 jaar`,
    pct_65_plus = `% 65 jaar of ouder`
  ) |>
  mutate(
    across(
      c(pct_0_15, pct_15_25, pct_25_45, pct_45_65, pct_65_plus),
      ~ parse_number(.x)
    )
  )

p_pop_clean <- p_pop_clean |>
  mutate(
    prov_join = as.character(provincienaam),
    prov_join = ifelse(prov_join == "Fryslân (Friesland)", "Friesland", prov_join)
  )

# join key for friesland
p_map_clean <- p_map |>
  mutate(
    prov_join = as.character(prov_name),
    prov_join = ifelse(prov_join == "Fryslân", "Friesland", prov_join)
  )

# join and reproject
nl_map <- p_map_clean |>
  left_join(p_pop_clean, by = "prov_join") |>
  st_transform(28992)

age_vars <- c("pct_0_15", "pct_15_25", "pct_25_45", "pct_45_65", "pct_65_plus")

summary(p_pop_clean[age_vars])

p_pop_clean |>
summarise(
mean_0_15 = mean(pct_0_15),
sd_0_15 = sd(pct_0_15),
mean_15_25 = mean(pct_15_25),
sd_15_25 = sd(pct_15_25),
mean_25_45 = mean(pct_25_45),
sd_25_45 = sd(pct_25_45),
mean_45_65 = mean(pct_45_65),
sd_45_65 = sd(pct_45_65),
mean_65plus = mean(pct_65_plus),
sd_65plus = sd(pct_65_plus)
)

#long format
p_long <- p_pop_clean |>
  pivot_longer(
    cols = all_of(age_vars),
    names_to = "variable",
    values_to = "percent"
  )

pdist_barplot <-
  ggplot(p_pop_clean, aes(x = reorder(provincienaam, pct_65_plus), 
                      y = pct_65_plus)) +
  geom_col(fill = "orange", color = "black") +
  coord_flip() +
  labs(
    title = "Elderly Population (65+) in the Netherlands by Province",
    x = "Province",
    y = "% 65+ years old",
    caption = "Credit: BoatGoBinter"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.caption = element_text(hjust = 1, size = 10, face = "italic")
    )
pdist_barplot

tmap_options(component.autoscale = FALSE)
eldpop_map <-
  tm_shape(nl_map) +
    tm_polygons(
      fill = "pct_65_plus",
      fill.scale = tm_scale_intervals(
        style = "quantile",
        n = 5,
        values = "brewer.yl_or_rd"
      ),
      fill.legend = tm_legend(title = "% Elderly Population")
    ) +
    tm_title(
      "Elderly Population (65+) in the Netherlands by Province
      Created by BoatGoBinter",
      size = 0.9,
    ) +
    tm_layout(
      legend.outside = TRUE,
      frame = FALSE
    )
eldpop_map

dutchpop_overtime <- "https://drive.google.com/uc?export=download&id=148JuGrVSeZo3CJoNAratM9wh1M9TyI2P"
popovertime <- read_csv(dutchpop_overtime)

popovertime <- popovertime |>
  rename(
    year = TIME_PERIOD,
    pct_65_plus = OBS_VALUE
  ) |>
  mutate(
    year = as.numeric(year),
    pct_65_plus = as.numeric(pct_65_plus)
  )

model <- lm(pct_65_plus ~ year, data = popovertime)

summary(model)

aging_pop_trend <-
ggplot(popovertime, aes(x = year, y = pct_65_plus)) +
  geom_point(size = 3, color = "darkorange") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
   scale_x_continuous(breaks = unique(popovertime$year))+
  labs(
    title = "Trend of Aging Population (65+) in the Netherlands (2013–2024) by BoatGoBinter",
    x = "Year",
    y = "% of Population Aged 65+"
  ) +
  theme_minimal()
aging_pop_trend

p_pop_clean <- p_pop_clean |>
  mutate(
    pct_65_plus = as.numeric(pct_65_plus),
    pct_65_plus_dec = pct_65_plus / 100
  )

avg_65_dec <- mean(p_pop_clean$pct_65_plus_dec)
avg_65_dec

if (!dir.exists("figures")) dir.create("figures")

w_in <- 9.6
h_in <- 5.4
dpi_val <- 200

ggsave(
  filename = file.path("figures", "pdist_barplot.png"),
  plot     = pdist_barplot,
  width    = w_in,
  height   = h_in,
  dpi      = dpi_val
)

tmap_save(
  tm       = eldpop_map,
  filename = file.path("figures", "eldpop_map.png"),
  width    = w_in,
  height   = h_in,
  dpi      = dpi_val,
  units    = "in"
)

ggsave(
  filename = file.path("figures", "aging_pop_trend.png"),
  plot     = aging_pop_trend,
  width    = w_in,
  height   = h_in,
  dpi      = dpi_val
)
