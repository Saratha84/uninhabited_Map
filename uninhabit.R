libs<-c(
  "tidyverse","terra","giscoR"
)

installed_libs<-libs%in%rownames(
  installed.packages()
)

if(any(installed_libs==F)){
  install.packages(
    libs[!installed_libs]
  )
}
invisible(
  lapply(
    libs,library,character.only=T
    )
)

url <- "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2030_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2030_GLOBE_R2023A_4326_30ss_V1_0.zip"
file_name <- "GHS_POP_E2030_GLOBE_R2023A_4326_30ss_V1_0.zip"
file_path <- file.path(getwd(), file_name)

# Check if the file already exists before downloading
if (!file.exists(file_path)) {
  download.file(url = url, destfile = file_path)
} else {
  message("File already exists. Skipping download.")
}

unzip(file_name)
raster_name<-gsub(
  ".zip",".tif",
  file_name
)

pop<-terra::rast(raster_name)


get_country_borders<-function(){
  country<-giscoR::gisco_get_countries(
    country = "LK",
    resolution = "3"
  )
  return(country)
}

country<-get_country_borders()

sri_lanka_pop<-terra::crop(
  pop,
  terra::vect(country),
  snap="in",
  mask=T
)

sri_lanka_pop_df<-as.data.frame(
  sri_lanka_pop,
  xy=T,na.rm=T
)
head(sri_lanka_pop_df)
names(sri_lanka_pop_df)[3]<-"val"

sri_lanka_pop_df<-sri_lanka_pop_df|>
  dplyr::mutate(
    cat=dplyr::if_else(
      val>0,"Yes","No"
    )
  )
sri_lanka_pop_df$cat<-as.factor(
  sri_lanka_pop_df$cat
)

cols<-c("#283747", "#9b59b6" )
plot_width <- 10  # Change this value as needed
plot_height <- 16  # Change this value as needed
p<-ggplot()+
  geom_raster(
    data=sri_lanka_pop_df,
    aes(x=x,
        y=y,
        fill=cat
        )
  )+
  scale_fill_manual(
    name="Is there human presence?",
    values=cols,
    na.value="#283747"
  )+
  guides(
    fill=guide_legend(
      direction = "horizontal",
      keyheight = unit(5,"mm"),
      keywidth = unit(15,"mm"),
      label.position = "bottom",
      label.hjust = .5,
      nrow = 1,
      byrow = T
    )
  )+
  theme_bw()+
  theme(
    legend.position = "top",
    legend.title = element_text(
      size = 14,color="grey10"
    ),
    legend.text = element_text(
      size = 14,color="grey10"
    ),
    plot.caption = element_text(
      size = 10,color="grey10",
      hjust = .20,vjust =30
    ),
    plot.margin = unit(
      c(
        t=0,b=-1,
        l=-1,r=-1
      ),"lines"
    )
  )+
  labs(
    title="",
    caption="Data:Global Human Settlement layer at 30m"
  )
ggsave("output_plot.png", p, width = plot_width, height = plot_height, units = "in")















