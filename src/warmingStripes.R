#packages
library(pacman)
pacman::p_load(tidyverse, here, lubridate, RColorBrewer, cowplot)

#import the annual temperatures
temp_dyce <- read_csv("https://data.giss.nasa.gov/tmp/gistemp/STATIONS/tmp_UKM00003091_14_0_1/station.csv")

#select only the annual temperature and year column
temps <- temp_dyce %>%
  select(YEAR, metANN) %>%
  rename(temp = metANN) %>%
  mutate(temp = na_if(temp, 999.9),
         date = ymd(str_c(YEAR, "01-01", sep = "-"))) %>%
  select(date, temp) %>%
  filter(year(date) >= 1881)

theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, face = "bold")
  )

# Apply a circular crop to an image
cropCircular <- function(imageOriginal, xOffset, yOffset, zoom) {
  # get height, width and crop longer side to match shorter side
  imageInfo <- magick::image_info(imageOriginal)
  minSide <- round(min(imageInfo$width, imageInfo$height) / zoom)
  
  imageSquare <- magick::image_crop(imageOriginal, geometry=paste0(minSide, "x", minSide, "+", xOffset, "+", yOffset), repage=TRUE)
  
  # create a new image with white background and black circle
  mask <- magick::image_draw(magick::image_blank(minSide, minSide))
  symbols(minSide/2, minSide/2, circles=(minSide/2)-3, bg='black', inches=FALSE, add=TRUE)
  dev.off()
  
  # create an image composite using both images
  imageCircular <- magick::image_composite(imageSquare, mask, operator='copyopacity')
  
  # set background as white
  imageCircular <- magick::image_background(imageCircular, 'white')
  
  return(imageCircular)
}

# read profile image
image <- magick::image_read(here::here("data", "Chris Woods Sunnies.jpg"))

# apply circular crop
circlularImage <- cropCircular(image, 0, 75, 1)
circlularImage
# Convert to Raster
rasterImg <- grid::rasterGrob(circlularImage, interpolate = FALSE) 

col_strip <- brewer.pal(11, "RdBu")

# Create Main Plot
main <- ggplot(temps,
       aes(x = date, y = 1, fill = temp))+
  geom_tile()+
  scale_x_date(date_breaks = "10 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "Dyce, UK 1881-2021",
       caption = "Data: GISS Surface Temperature Analysis")+
  theme_strip + 
  theme(legend.position = "none")

ggsave("UK Warming Stripes.jpg", plot = main, path = here::here("output"),device = "jpg", dpi=320, width = 19.2, height = 10.8, units = "cm")


# Create Circular Version
ring <- ggplot(temps, aes(ymax=5, ymin=3.7, xmax=date + years(1), xmin=date, fill=temp)) +
  geom_rect() +
  scale_fill_gradientn(colors = rev(col_strip))+
  theme_strip + 
  theme(legend.position = "none",
        axis.text.x=element_blank(),) +
  ylim(c(0, 5)) +
  coord_polar()

# Overlay circular plot over profile image
g <-ggdraw() +
## tweak this to fit your plot
  draw_grob(rasterImg, 0.2, 0.2, 0.6, 0.6) +
  draw_plot(ring)
g

ggsave("Business Analyst (Data).jpg", plot = g, path = here::here("output"),device = "jpg", dpi=320, width = 10, height = 10, units = "cm")

