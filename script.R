load('level_1_metadata.RData')

# libraries
library(ggplot2)
library(dplyr)
library(sapfluxnetQC1)
library(viridis)
library(leaflet)

## ggplot themes
bar_minimal_theme <- function(base_size = 10, base_family = "sans") {
  half_line <- base_size/2
  theme(line = element_line(colour = "#5C97BF", size = 1,
                            linetype = 1, lineend = "butt"),
        rect = element_rect(fill = NA, colour = "#5C97BF",
                            size = 1, linetype = 1),
        text = element_text(family = base_family, face = "plain",
                            colour = "#5C97BF", size = base_size,
                            lineheight = 0.9, hjust = 0.5,
                            vjust = 0.5, angle = 0,
                            margin = margin(), debug = FALSE),
        axis.line = element_blank(),
        # axis.line.x = element_line(),
        # axis.line.y = element_line(),
        axis.text = element_text(size = rel(0.8)),
        axis.text.x = element_text(margin = margin(t = 0.8 * half_line*2.5),
                                   vjust = 1),
        axis.text.y = element_text(margin = margin(r = 0.8 * half_line*2),
                                   hjust = 1),
        axis.ticks.y = element_line(colour = "#5C97BF", size = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.length = unit(-half_line, "pt"),
        axis.title.x = element_text(margin = margin(t = 0.8 * half_line,
                                                    b = 0.8 * half_line/2)),
        axis.title.y = element_text(angle = 90,
                                    margin = margin(r = 0.8 * half_line,
                                                    l = 0.8 * half_line/2)),
        legend.background = element_rect(colour  = NA, fill = ),
        legend.spacing = unit(1, "pt"),
        legend.key = element_rect(colour = NA),
        legend.key.size = unit(1, "lines"),
        legend.key.height = NULL,
        legend.key.width = NULL,
        legend.text = element_text(size = rel(0.8)),
        legend.text.align = NULL,
        legend.title = element_text(hjust = 0.5),
        legend.title.align = 0,
        legend.position = "right",
        legend.direction = NULL,
        legend.justification = "top",
        legend.box = NULL,
        panel.background = element_blank(),
        panel.border = element_blank(),
        # panel.grid = element_blank(),
        # panel.grid.major = element_line(colour = "black", size = rel(0.3),
        #                                 linetype = 2),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "#5C97BF", size = .8,
                                          linetype = 1),
        panel.spacing = unit(half_line, "pt"),
        panel.spacing.x = NULL,
        panel.spacing.y = NULL,
        panel.ontop = FALSE,
        strip.background = element_rect(size = rel(0.3)),
        strip.text = element_text(colour = "grey10", size = rel(0.8)),
        strip.text.x = element_text(margin = margin(t = half_line,
                                                    b = half_line)),
        strip.text.y = element_text(angle = -90,
                                    margin = margin(l = half_line, r = half_line)),
        strip.switch.pad.grid = unit(0.1, "cm"),
        strip.switch.pad.wrap = unit(0.1, "cm"),
        plot.background = element_blank(),
        plot.title = element_text(size = rel(1.2),
                                  margin = margin(b = half_line * 1.2)),
        plot.margin = margin(half_line, half_line, half_line, half_line),
        
        complete = TRUE)
}

total <- length(plant_md[['pl_sens_meth']])
plant_md %>%
  group_by(pl_sens_meth) %>%
  summarise(perc = (n()/total)*100) %>%
  ggplot(aes(x = pl_sens_meth, y = perc, fill = pl_sens_meth)) +
  geom_bar(stat = 'identity') +
  scale_fill_viridis(discrete = TRUE) +
  labs(x = 'Method', y = '% of total plants') +
  bar_minimal_theme(base_size = 22) +
  theme(
    legend.position = 'none'
  )


table <- data.frame(
  Metadata = c('Site', 'Stand', 'Species', 'Plant', 'Environmental', 'Total'),
  Items = c(20, 16, 4, 24, 16, 80)
)

knitr::kable(table, 'latex')

# map
load('preliminary_data_fixed.RData')

prelim_map_data <- preliminary_survey_fixed %>%
  select(id, latitude, longitude) %>%
  mutate(id = as.character(id), type = 'survey')

actual_map_data <- site_md %>%
  select(si_code, si_lat, si_long) %>%
  rename(id = si_code, latitude = si_lat, longitude = si_long) %>%
  mutate(type = 'actual')

map_data <- bind_rows(prelim_map_data, actual_map_data)

# map

# blues
leaflet() %>%
  addTiles(urlTemplate = 'http://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}',
           attribution = 'Tiles &copy; Esri &mdash; Esri, DeLorme, NAVTEQ',
           options = tileOptions(noWrap = FALSE)) %>%
  setView(lng = 60, lat = 10, zoom = 2) %>%
  addCircleMarkers(data = prelim_map_data, lng = ~longitude, lat = ~latitude,
                   radius = 5, fillColor = '#1B346C', stroke = FALSE,
                   fillOpacity = 1) %>%
  addCircleMarkers(data = actual_map_data, lng = ~longitude, lat = ~latitude,
                   radius = 5, fillColor = '#D35400', stroke = FALSE,
                   fillOpacity = 1) %>%
  # addLegend('bottomleft', pal = palette, values = color_data,
  #           title = point_color, layerId = 'legend_color', opacity = 0.8)
  addLegend(
    position = 'bottomleft',
    colors = c('#1B346C', '#01ABE9'),
    labels = c('Expected', 'Already received'), opacity = 1,
    title = 'Data sets'
  )
