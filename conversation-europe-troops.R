
library(data.table)
library(tidyverse)
library(troopdata)
library(ggplot2)
library(showtext)
library(sysfonts)
library(patchwork)
library(rnaturalearth)
library(gganimate)
library(sf)

sysfonts::font_add_google("Oswald", family = "oswald")
showtext::showtext_auto()

basesize <- 30
# Regular plot theme
theme_flynn <- function(){ 
  
  theme_linedraw(base_size = basesize, base_family = "oswald") %+replace% 
    
    theme(plot.title = element_text(face = "bold", size = basesize * 1.3, hjust = 0, margin = margin(t = 0, b = 0.3, l = 0, r = 0, unit = "cm")),
          plot.subtitle = element_text(size = basesize),
          plot.caption = element_text(face = "italic", size = basesize * 0.6),
          strip.background = element_rect(fill = "gray80", color = "black"),
          strip.text = element_text(color = "black", face = "bold", margin = margin(t = 0.2, b = 0.2, l = 0.2, r = 0.2, unit = "cm")),
          panel.grid.major = element_line(color = "gray70", size = 0.15),
          panel.grid.minor = element_line(color = "gray90", size = 0.1),
          axis.title = element_text(face = "bold", size = basesize),
          axis.title.y = element_text(angle = 90, margin = margin(t = 0, r = 0.5, b = 0, l = 0, unit = "cm")),
          axis.title.x = element_text(margin = margin(t = 0.5, r = 0, b = 0, l = 0, unit = "cm")),
          axis.ticks = element_line(size = 0.1),
          axis.ticks.length = unit(0.1, "cm"),
          legend.title = element_text(face = "bold", hjust = 0, margin = margin(t = 0, b = 0, l = 0, r = 0, unit = "cm")),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
          legend.margin=margin(t=-10, b=0, r=0, l=0),
          legend.box.margin=margin(-10,-10,-10,-10))
}

# Map version of theme
theme_flynn_map <- function(){
  
  theme_void(base_family = "oswald") %+replace% 
    
    theme(plot.title = element_text(face = "bold", size = 18, hjust = 0, margin = margin(t = 0, b = 0.3, l = 0, r = 0, unit = "cm")),
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0, b = 0.3, l = 0, r = 0, unit = "cm")),
          plot.caption = element_text(face = "italic", size = 8, hjust = 1, margin = margin(t = 0.2, unit = "cm")),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #axis.title = element_text(face = "bold", size = 0),
          #axis.title.y = element_text(margin = margin(t = 0, r = 0.5, b = 0, l = 0, unit = "cm")),
          #axis.title.x = element_text(margin = margin(t = 0.5, r = 0, b = 0, l = 0, unit = "cm")),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          legend.key.height = unit(0.6, "cm"),
          legend.key.width = unit(2.5, "cm"))
}


#### Map Animation ####

map.list <- list()

basemap <- ne_countries(returnclass = "sf") #%>% 
  #mutate(year = list(1989:2021)) %>% 
  #unnest(year)

troopdata <- troopdata::troopdata %>% 
  filter(!grepl(".*United States.*", countryname))

date <- as.Date("1989-01-01")

for (i in 1:32){
  
  map.list[[i]] <- cshapes::cshp(date = date) %>% 
    mutate(iso3c = countrycode::countrycode(country_name, "country.name", "iso3c"),
           year = i + 1989) 
  
  date <- date + 365
  
}


#### Export European totals
#### 
troops.time <- troopdata %>% 
  mutate(region2 = countrycode::countrycode(ccode, "cown", "continent"),
         region2 = ifelse(ccode %in% c(260, 265, 315, 345, 347), "Europe", region2)) %>% 
  dplyr::filter(region2 == "Europe") %>% 
  group_by(year) %>% 
  dplyr::summarise(trooptotal = sum(troops, na.rm = TRUE)) 

fwrite(troops.time, here::here("troops-europe-totals.csv"))


map.df <- rbindlist(map.list, idcol = TRUE) %>% 
  left_join(troopdata, by = c("iso3c", "year")) %>% 
  left_join(troops.time, by = c("year"))




m1 <- ggplot() +
  geom_sf(data = basemap, aes(geometry = geometry), fill = "gray90", color = "gray90", size = 0.1) +
  geom_sf(data = map.df, aes(geometry = geometry, fill = troops), color = "black", size = 0.1) +
  theme_void() +
  theme(text = element_text(family = "oswald"),
        plot.title = element_text(size = 35, face = "bold"),
        plot.subtitle = element_text(size = 26, face = "bold", hjust = 0),
        legend.title = element_text(size = 26, face = "bold"),
        legend.text = element_text(size = 20, margin = margin(l = -0.9, unit = "cm")),
        panel.border = element_rect(color = "black", fill = NA, size = 0.2),
        legend.key.height = unit(1, "cm"),
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, unit = "cm")) +
  coord_sf(xlim = c(-14, 41.5),
                  ylim = c(33.8, 61.1)) +
  viridis::scale_fill_viridis(option = "magma", direction = -1, begin = 0.1, end = 0.9, na.value = "gray90", breaks = c(0, 50, 500, 5000, 50000, 500000), limits = c(0, 500000), trans = "log1p", label = scales::comma_format()) +
  #coord_sf(crs = sf::st_crs("ESRI:54030")) +
  labs(fill = "Troops",
       x = "",
       y = "",
       title = "The Changing US Military Footprint in Europe: {closest_state}",
       subtitle = glue::glue("Approximate Personnel: {map.df$trooptotal}"))


m1.anam <- m1 + 
  transition_states(
    states = year,
    transition_length = 0.0
  )

animate(m1.anam, fps = 10, duration = 20, height = 6, width = 8, units = "in", res = 200, renderer = gifski_renderer())

anim_save(here::here("troop-map-gif.gif"), fps = 10, duration = 20, height = 6, width = 8, units = "in", res = 200, renderer = gifski_renderer())



