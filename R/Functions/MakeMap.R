#### MAKE MAP ####

MakePrettyMap <- function(CountryList){
  
  # Get coordinates
  coords <- map_df(CountryList, ~ mutate(.x$meta, Gradient = as.character(Gradient))) %>% 
    group_by(Country) %>% 
    summarise(Elevation = mean(Elevation),
              Latitude = mean(Latitude),
              Longitude = mean(Longitude)) %>% 
    mutate(Region = recode(Country, "CH" = "China", "CO" = "Colorado", "NO" = "Norway", "PE" = "Peru", "SV" = "Svalbard"))

  # Get Wolrdclim elevation data
  elev <- raster::getData('worldclim', var='alt', res=2.5) # can play with 10 for lower resolution map
  
  #### WORLDMAP ####
  elev.spdf <- as(elev, "SpatialPixelsDataFrame")
  elev.df <- as_tibble(elev.spdf) %>% 
    # replace all values > 6000 with 6000 (everything above is not interesting)
    mutate(alt = ifelse(alt > 6000, 6000, alt)) %>% 
    filter(x > -130 & x < 130 & y > -25 & y < 85)
  
  # plot world map
  PFTCMap <- ggplot() + 
    geom_raster(data = elev.df, aes(x=x, y=y, fill = alt)) + 
    coord_equal() +
    labs(x="", y = "", fill = "") + 
    scale_y_continuous(limits = c(-25, 85), expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_fill_viridis_c(breaks = seq(0, 6000, 2000), labels = paste(seq(0, 6000, 2000), "m")) +
    geom_point(data = coords, aes(x = Longitude, y = Latitude), colour  = "orange2", size = 3) +
    geom_text(data = coords, aes(x = Longitude, y = Latitude, label = Region),hjust = -0.2, vjust = -0.2, colour = "orange2") +
    theme_bw() +
    theme(legend.position = c(0.005, 0.005), 
          legend.justification = c(0, 0), 
          panel.border = element_blank(),
          legend.title = element_blank())
  
  return(PFTCMap)
}


# ### MAKE MAP 2
# library("maps")
# library("mapdata")
# 
# # Get coordinates
# coords <- map_df(CountryList, ~ mutate(.x$meta, Gradient = as.character(Gradient))) %>% 
#   group_by(Country) %>% 
#   summarise(Elevation = mean(Elevation),
#             Latitude = mean(Latitude),
#             Longitude = mean(Longitude))
# 
# world <- map_data("world") %>% 
#   filter(long > -130 & long < 130 & lat > -25 & lat < 85)
# main.plot <- ggplot() + 
#   geom_polygon(data = world, aes(x = long, y = lat, group = group)) + 
#   coord_fixed(1.3) +
#   geom_point(data = coords, aes(x = Longitude, y = Latitude), colour  = "#D55E00", size = 3) +
#   labs(x = "", y = "") +
#   theme_bw() +
#   theme(legend.position = "none",
#         panel.border = element_blank(),
#         legend.title = element_blank())
# 
# 
# divdata <- Diversity %>% 
#   group_by(Country, Gradient, Site, Elevation) %>%
#   summarise(Richness = mean(Richness),
#             Evenness = round(mean(Evenness), 2)) %>%
#   mutate(CG = paste(Country, Gradient, sep = ""))
# 
# no <- divdata %>% filter(Country == "NO")
# sv <- divdata %>% filter(Country == "SV")
# pe <- divdata %>% filter(Country == "PE")
# co <- divdata %>% filter(Country == "CO")
# 
# inset.ch <- ggplot(divdata %>% filter(Country == "CH"), aes(x = Elevation, y = Richness)) +
#   geom_smooth(method = "lm", se = FALSE, colour = "orange2") +
#   geom_smooth(aes(x = Elevation, y = Evenness * 15), method = "lm", se = FALSE, linetype = "dashed", colour = "plum4") +
#   scale_y_continuous(limits = c(0, 20), breaks = c(0, 20), sec.axis = sec_axis(~./15, breaks = c(0, 1))) +
#   scale_x_continuous(breaks = c(3000, 4000), sec.axis = sec_axis(~./1, breaks = c(3000, 4000), labels = c("Richness", "Evenness"))) +
#   labs(x = "", y = "") +
#   theme(legend.position = "none",
#         panel.background = element_rect(fill = "slategray3"),
#         panel.border = element_blank(),
#         panel.grid = element_blank(),
#         axis.text.x.top = element_text(colour = c("orange2", "plum4")),
#         axis.text.y.left = element_text(colour = "orange2"),
#         axis.text.y.right = element_text(colour = "plum4"),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank(),
#         plot.margin = unit(c(0.3,0.3,0,0), "cm"),
#         plot.background = element_rect(fill = "slategray3", colour = "grey26", size = 1))
# 
# inset.no <- inset.ch %+% filter(no) +
#   scale_x_continuous(breaks = c(500, 1000), sec.axis = sec_axis(~./1, breaks = c(500, 1000), labels = c("Richness", "Evenness"))) +
#   scale_y_continuous(limits = c(0, 20), breaks = c(0, 20), sec.axis = sec_axis(~./15, breaks = c(0, 1)))
# 
# inset.sv <- inset.ch %+% filter(sv) +
#   scale_y_continuous(limits = c(0, 20), breaks = c(0, 20), sec.axis = sec_axis(~./15, breaks = c(0, 1))) +
#   scale_x_continuous(breaks = c(0, 250), sec.axis = sec_axis(~./1, breaks = c(0, 250), labels = c("Richness", "Evenness")))
# 
# inset.pe <- inset.ch %+% filter(pe) +
#   scale_y_continuous(limits = c(0, 15), breaks = c(0, 15), sec.axis = sec_axis(~./15, breaks = c(0, 1))) +
#   scale_x_continuous(limits = c(3000, 4000), breaks = c(3000, 4000), sec.axis = sec_axis(~./1, breaks = c(3000, 4000), labels = c("Richness", "Evenness")))
# 
# inset.co <- inset.ch %+% filter(co) +
#   scale_y_continuous(limits = c(0, 20), breaks = c(0, 20), sec.axis = sec_axis(~./15, breaks = c(0, 1))) +
#   scale_x_continuous(breaks = c(2500, 3500), sec.axis = sec_axis(~./1, breaks = c(3000, 4000), labels = c("Richness", "Evenness")))
# 
# 
# vp_main.plot <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
# vp_ch <- viewport(width = 0.16, height = 0.14, x = 0.88, y = 0.42)
# vp_no <- viewport(width = 0.16, height = 0.14, x = 0.45, y = 0.62)
# vp_sv <- viewport(width = 0.16, height = 0.14, x = 0.71, y = 0.8)
# vp_pe <- viewport(width = 0.16, height = 0.14, x = 0.37, y = 0.35)
# vp_co <- viewport(width = 0.16, height = 0.14, x = 0.14, y = 0.65)
# 
# grid.newpage()
# print(main.plot, vp = vp_main.plot)
# print(inset.ch, vp = vp_ch)
# print(inset.no, vp = vp_no)
# print(inset.sv, vp = vp_sv)
# print(inset.pe, vp = vp_pe)
# print(inset.co, vp = vp_co)
