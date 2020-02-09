setwd("C:/Users/htunlong/OneDrive - UGent/Research/Postdoc/Papers/Bibliometric paper/Revision_analysis/Lakes/Lakes/Lake_country/Line plot")

#### import libraries ####
# data wrangling

library(tidyverse)
library(reshape)
library(data.table)
library(xlsx)
library(gridExtra)
library(grid)
library(chron)
library(devtools)
library(rscopus)
library(rlist)
library(rgeos)
library(future)
library(parallel)
library(doParallel)

# data visualization 

library(GGally)
library(RColorBrewer)
library(proj4)
library(leaflet)
library(leaflet.minicharts)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(corrplot)
library(mice)
library(VIM)
library(ggmosaic)
library(esquisse)
library(bibliometrix)
library(ggwordcloud)
library(colorspace)
library(rworldmap)
library(countrycode)
library(fmsb)

#### SDG 6 year####

temp <- list.files(pattern="*.csv")
myfiles <- lapply(temp, read.csv, sep = "\t")

temp2 <- str_match(string = temp, pattern = "\\_(.*?)\\.")[,2]
temp3 <- str_match(string = temp, pattern = "(.*?)\\_")[,2]
temp3 <- paste(temp3, "s", sep = "")


myfiles <- map2(myfiles, temp2, function(x, y) {x$Country <- y; return(x)})
myfiles <- map2(myfiles, temp3, function(x, y) {x$Type <- y; return(x)})


sdg6 <- rbindlist(myfiles)

factor_convert <- function(x){
    x <- x %>% filter(YEAR < 2019 & YEAR > 1954)
    x$YEAR <- as.factor(x$YEAR)
    x$Country <- as.factor(x$Country)
    x$Type <- as.factor(x$Type)
    return(x)
}

sdg6 <- factor_convert(sdg6)

ggsave("lake_country.tiff", ggplot(sdg6, aes(x = YEAR, y = n, color = Country, group = Country)) +
           geom_point(size = 1) +
           geom_line(size = 1.5) +
           theme_bw() +
           xlab("Year") +
           ylab("Number of publications") +
           scale_x_discrete(breaks = seq(1955,2020, 10)) +
           facet_wrap(.~Type, scales = "free") +
           scale_color_brewer(palette = 'Set1') +
           theme(text=element_text(family = "Arial", size = 14)) +
           # theme(axis.text.x = element_text(size = 14)) +
           # theme(axis.text.y = element_text(size = 14)) +
           theme(axis.title = element_text(size = 14)) +
           theme(legend.title = element_blank()) +
           theme(legend.text = element_text(size = 14)),
       units = 'cm', height = 20, width = 30, dpi = 300
)
