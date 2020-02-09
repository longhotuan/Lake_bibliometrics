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
colnames_chosen <- colnames(myfiles[[1]])[c(2, 4:6, 13, 14, 16, 18, 19, 20, 31:34, 39:42, 43, 44)]

# choose the useful columns
myfiles <- lapply(myfiles, subset, select = colnames_chosen)

factor_convert <- function(x){
    x$`Source title` <- as.factor(x$`Source title`)
    # x$`Language of Original Document`<- as.factor(x$`Language of Original Document`)
    x$`Document Type` <- as.factor(x$`Document Type`)
    x$`Access Type` <- as.factor(x$`Access Type`)
    x$`Abbreviated Source Title` <- as.factor(x$`Abbreviated Source Title`)
    x <- x %>% filter(Year < 2020)
}

myfiles <- lapply(myfiles, factor_convert)
names(myfiles) <- str_split_fixed(temp, "\\.", 2)[,1]

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

myplot_all <- lapply(myplot, function(x) x <- x[1:40,])
myplot_all1 <- lapply(myplot1, function(x) x <- x[1:40,])
myplot_all2 <- lapply(myplot2, function(x) x <- x[1:40,])
myplot_all3 <- lapply(myplot3, function(x) x <- x[1:40,])

myplot_all <- lapply(names(myplot_all), function(x){
    if (str_detect(x,"lake")){
        myplot_all[[x]]$type <- "lake"
        
    } else if (str_detect(x,"Reservoir")){
        myplot_all[[x]]$type <- "reservoir"
        
    }; return(myplot_all[[x]])
})
myplot_all1 <- lapply(names(myplot_all1), function(x){
    if (str_detect(x,"lake")){
        myplot_all1[[x]]$type <- "lake"
        
    } else if (str_detect(x,"Reservoir")){
        myplot_all1[[x]]$type <- "reservoir"
        
    }; return(myplot_all1[[x]])
})
myplot_all2 <- lapply(names(myplot_all2), function(x){
    if (str_detect(x,"lake")){
        myplot_all2[[x]]$type <- "lake"
        
    } else if (str_detect(x,"Reservoir")){
        myplot_all2[[x]]$type <- "reservoir"
        
    }; return(myplot_all2[[x]])
})
myplot_all3 <- lapply(names(myplot_all3), function(x){
    if (str_detect(x,"lake")){
        myplot_all3[[x]]$type <- "lake"
        
    } else if (str_detect(x,"Reservoir")){
        myplot_all3[[x]]$type <- "reservoir"
        
    }; return(myplot_all3[[x]])
})

names(myplot_all) <- names(myplot_all1) <- names(myplot_all2) <-  names(myplot_all3) <-str_split_fixed(temp, "\\.", 2)[,1]

myplot_all_v2 <- map2(myplot_all[1:(length(myplot_all)/2)],myplot_all[(length(myplot_all)/2+1):length(myplot_all)], function(x,y) bind_rows(x,y))
myplot_all1_v2 <- map2(myplot_all1[1:(length(myplot_all1)/2)],myplot_all1[(length(myplot_all1)/2+1):length(myplot_all1)], function(x,y) bind_rows(x,y))
myplot_all2_v2 <- map2(myplot_all2[1:(length(myplot_all2)/2)],myplot_all2[(length(myplot_all2)/2+1):length(myplot_all2)], function(x,y) bind_rows(x,y))
myplot_all3_v2 <- map2(myplot_all3[1:(length(myplot_all3)/2)],myplot_all3[(length(myplot_all3)/2+1):length(myplot_all3)], function(x,y) bind_rows(x,y))
names(myplot_all_v2) <- names(myplot_all1_v2) <- names(myplot_all2_v2) <- names(myplot_all3_v2) <- c("China", "US")

# print the graphs

plot_kw <- function(x){
    ggplot(x, aes(label = keyword, size = n, x = type, color = type)) +
        geom_text_wordcloud_area(shape = "diamond") +
        scale_size_area(max_size = 20) +
        scale_x_discrete(breaks = NULL) +
        theme_minimal()
}

myfigure <- lapply(myplot_all_v2, plot_kw)
myfigure1 <- lapply(myplot_all1_v2, plot_kw)
myfigure2 <- lapply(myplot_all2_v2, plot_kw)
myfigure3 <- lapply(myplot_all3_v2, plot_kw)

lapply(names(myfigure), function(x) ggsave(filename=paste(x,".jpeg",sep=""), plot=myfigure[[x]], units = 'cm', height = 50*1/3, width = 75*1/2, dpi = 300))
lapply(names(myfigure1), function(x) ggsave(filename=paste(x,"_1.jpeg",sep=""), plot=myfigure1[[x]], units = 'cm', height = 50*1/3, width = 75*1/2, dpi = 300))
lapply(names(myfigure2), function(x) ggsave(filename=paste(x,"_2.jpeg",sep=""), plot=myfigure2[[x]], units = 'cm', height = 50*1/3, width = 75*1/2, dpi = 300))
lapply(names(myfigure3), function(x) ggsave(filename=paste(x,"_3.jpeg",sep=""), plot=myfigure3[[x]], units = 'cm', height = 50*1/3, width = 75*1/2, dpi = 300))

