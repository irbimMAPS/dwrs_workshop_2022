# timeseries plot ---
defo_ts=ggplot()+ 
  theme_bw() +
  theme( plot.title = element_text(hjust = 0),
         axis.text = element_text(size = text_size), 
         title = element_text(size = text_size),
         axis.title = element_text(size = 16),
         legend.title = element_blank(),
         legend.text = element_text(size = text_size),
         legend.position = "bottom",
         #legend.position = "none",
         #legend.title = element_text(size=text_size),
         #legend.key.width=unit(1,"cm"), 
         #legend.key.height = unit(1,"cm"),
         axis.text.x = element_text(angle = 90))+
  coord_flip()

# maps plot ---

defo_map=ggplot() + 
  geom_sf(data = dep, colour = NA, fill = "gray80", alpha = 0.7) +
  geom_sf(data = land, fill = "gray33", colour = "gray33") +
  geom_sf(data = gsa, fill = NA, colour = "gray33", size = 0.5) +
  coord_sf(xlim=c(lims[[1]], lims[[3]]), ylim=c(lims[[2]], lims[[4]]) ) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        #legend.key.width=unit(0.5,"cm"), 
        legend.title = element_text(size=text_size),
        legend.key = element_rect(colour = "white"), 
        legend.justification = 'left', 
        legend.position = 'bottom', 
        legend.box.just = 'left',
        legend.text = element_text(size = text_size),
        #axis.text.y = element_text(angle = 90, size=text_size, hjust =1, vjust = 0.5),
        axis.text.y = element_text(size=text_size),
        axis.text = element_text(size = text_size), 
        title = element_text(size = text_size),
        axis.title = element_text(size = text_size)) +
  xlab("") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "br", height = unit(.5, "cm"), width = unit(.5, "cm"))
