# import libraries ####
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
library(feather)
library(usethis)

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
library(usethis)

# Import datasets  ####

temp <- list.files(pattern="*.csv")
myfiles <- lapply(temp, read_csv)
colnames_chosen <- colnames(myfiles[[1]])[c(1, 3:5, 12, 13, 15, 17, 18, 19, 33:36, 42:45, 46, 47)]

# choose the useful columns
myfiles <- lapply(myfiles, subset, select = colnames_chosen)

factor_convert <- function(x){
    x$`Source title` <- as.factor(x$`Source title`)
    # x$`Language of Original Document`<- as.factor(x$`Language of Original Document`)
    x$`Document Type` <- as.factor(x$`Document Type`)
    x$`Access Type` <- as.factor(x$`Access Type`)
    x$`Abbreviated Source Title` <- as.factor(x$`Abbreviated Source Title`)
    x <- x %>% filter(Year < 2019 & Year > 1955)
}

myfiles <- lapply(myfiles, factor_convert)

temp2 <- str_match(string = temp, pattern = "\\_(.*?)\\.")[,2]
temp3 <- str_match(string = temp, pattern = "(.*?)\\_")[,2]
temp3 <- paste(temp3, "s", sep = "")
myfiles <- map2(myfiles, temp2, function(x, y) {x$Country <- y; return(x)})
myfiles <- map2(myfiles, temp3, function(x, y) {x$Type <- y; return(x)})
names(myfiles) <- str_match(string = temp, pattern = "(.*?)\\.")[,2]

period_division <- function(x, y, z){x <- x %>% filter(Year < y & Year > z)}

myfiles1 <- lapply(myfiles, period_division, y = 2020, z = 2009)
myfiles2 <- lapply(myfiles, period_division, y = 2010, z = 1999)
myfiles3 <- lapply(myfiles, period_division, y = 2000, z = 1900)

# Number and top of keywords ----

KW <- function(x){
    if (nrow(x) == 0){
        return(x)
    } else {
        keyword <- strsplit(x$`Author Keywords`, "; ")
        for (i in 1:length(keyword)){
            keyword[i] <- as.data.frame(matrix(as.data.frame(keyword[i])))
        }
        keyword2 <- rbindlist(keyword)
        colnames(keyword2)[1]<- "keyword"
        keyword2<- keyword2[complete.cases(keyword2),]
        keyword2$keyword <- str_to_title(keyword2$keyword)
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Lakes', "Lake")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Reservoirs', "Reservoir")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Ponds', "Pond")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Wetlands', "Wetland")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Floods', "Flood")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Dams', "Dam")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Nutrients', "Nutrient")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Modelling', "Modeling")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Models', "Model")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Metals', "Metal")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Diatoms', "Diatom")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Macrophytes', "Macrophyte")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Sediments', "Sediment")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Levels', "Level")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Stabilisation', "Stabilization")
        keyword2$keyword <- str_replace_all(keyword2$keyword, 'Lake Taihu', "Taihu Lake")
        keyword3 <- keyword2 %>%
            dplyr::group_by(keyword) %>% 
            dplyr::summarise(n=n()) %>% 
            dplyr::arrange(desc(n)) 
        return(keyword3)
    }
}

myplot <- lapply(myfiles, KW)
myplot1 <- lapply(myfiles1, KW)
myplot2 <- lapply(myfiles2, KW)
myplot3 <- lapply(myfiles3, KW)


myplot <- map2(myplot, temp2, function(x, y) {x$Country <- y; return(x)})
myplot1 <- map2(myplot1, temp2, function(x, y) {x$Country <- y; return(x)})
myplot2 <- map2(myplot2, temp2, function(x, y) {x$Country <- y; return(x)})
myplot3 <- map2(myplot3, temp2, function(x, y) {x$Country <- y; return(x)})

myplot <- map2(myplot, temp3, function(x, y) {x$Type <- y; return(x)})
myplot1 <- map2(myplot1, temp3, function(x, y) {x$Type <- y; return(x)})
myplot2 <- map2(myplot2, temp3, function(x, y) {x$Type <- y; return(x)})
myplot3 <- map2(myplot3, temp3, function(x, y) {x$Type <- y; return(x)})

myplot_all <- lapply(myplot, function(x) x <- x[1:40,])
myplot_all1 <- lapply(myplot1, function(x) x <- x[1:40,])
myplot_all2 <- lapply(myplot2, function(x) x <- x[1:40,])
myplot_all3 <- lapply(myplot3, function(x) x <- x[1:40,])

myplot_all_v2 <- rbindlist(myplot_all)
myplot_all1_v2 <- rbindlist(myplot_all1)
myplot_all2_v2 <- rbindlist(myplot_all2)
myplot_all3_v2 <- rbindlist(myplot_all3)

# print the graphs

plot_kw <- function(x){
    ggplot(x, aes(label = keyword, size = n, x = Type, color = Type)) +
        geom_text_wordcloud_area(shape = "diamond") +
        scale_size_area(max_size = 15) +
        scale_x_discrete(breaks = NULL) +
        facet_wrap(.~Country, scales = "free") +
        theme_minimal()
}

myfigure <- split(myplot_all_v2, myplot_all_v2$Country)
myfigure1 <- split(myplot_all1_v2, myplot_all_v2$Country)
myfigure2 <- split(myplot_all2_v2, myplot_all_v2$Country)
myfigure3 <- split(myplot_all3_v2, myplot_all_v2$Country)

myfigure <- lapply(myfigure, plot_kw)
myfigure1 <- lapply(myfigure1, plot_kw)
myfigure2 <- lapply(myfigure2, plot_kw)
myfigure3 <- lapply(myfigure3, plot_kw)


lapply(names(myfigure), function(x) ggsave(filename=paste(x,".jpeg",sep=""), plot=myfigure[[x]], units = 'cm', height = 50*1/3, width = 75*1/2, dpi = 300))
lapply(names(myfigure1), function(x) ggsave(filename=paste(x,"_1.jpeg",sep=""), plot=myfigure1[[x]], units = 'cm', height = 50*1/3, width = 75*1/2, dpi = 300))
lapply(names(myfigure2), function(x) ggsave(filename=paste(x,"_2.jpeg",sep=""), plot=myfigure2[[x]], units = 'cm', height = 50*1/3, width = 75*1/2, dpi = 300))
lapply(names(myfigure3), function(x) ggsave(filename=paste(x,"_3.jpeg",sep=""), plot=myfigure3[[x]], units = 'cm', height = 50*1/3, width = 75*1/2, dpi = 300))