library(tidyverse)
library(sp)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(mapproj)
library(rvest)
library(httr)

setwd("~/corona19")
#shp file reading
sido <- readOGR("TL_SCCO_CTPRVN.shp")

summary(sido)


#proj 인자 설정
from_crs <- CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 +k=0.9996 +x_0=1000000 +y_0=2000000
                +ellps=GRS80 +units=m")

to_crs <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")



#좌표계로 변환
shp_sido <- spTransform(sido, to_crs)

summary(shp_sido)

slotNames(shp_sido) # d/t S4 class

###
shp_sido@data

slotNames(shp_sido@data)

data_sido <- shp_sido@data
str(data_sido)

# shp -> dataframe
shp_result <- fortify(shp_sido)
str(shp_result)
head(shp_result)

Name <- cbind(id = row.names(data_sido), sidoname = as.character(data_sido$CTP_KOR_NM))
str(Name); head(Name)

x <- as.data.frame(Name)
x$id <- as.character(x$id)
str(x); head(x)

# 최종 dataframe 생성
result <- left_join(shp_result, x)
str(result); head(result)

#웹 크롤러
url <- "https://www.cdc.go.kr/board/board.es?mid=a20501000000&bid=0015&list_no=366494&act=view"


tar <- read_html(url) %>%
  html_nodes("p span") %>%
  html_text()
#44, 167
tar <- tar[44:167]

tar_var <- tar[3:23] 

tar_var[1] <-paste0(tar_var[1], tar_var[2])
tar_var[4] <- paste0(tar_var[4], tar_var[5])
tar_var[17] <-paste0(tar_var[17], tar_var[18])
tar_var[20] <- paste0(tar_var[20], tar_var[21])

tar_var <- tar_var[nchar(tar_var) == 2]

tar_var2 <- tar[90:106]
tar_var2[3] <- str_remove(tar_var2[3], ",")
tar_var2[15] <- str_remove(tar_var2[15], ",")
tar_var2[12] <-as.double(str_sub(tar_var2[12],1,2)) + as.double(str_sub(tar_var2[12],3,4))

tar_var2 <- as.double(tar_var2)

corona_number <- tibble(id = as.character(0:16), sido = tar_var, number = tar_var2)



result <- left_join(result, corona_number)
add_text <- aggregate(cbind(long,lat) ~ sido, data = result, FUN=mean)
add_text$number <- corona_number$number[match(add_text$sido, corona_number$sido)]


ggplot(data = result, aes(x = long, y = lat,  group = group, fill = number)) +
  geom_polygon() +
  coord_map() + 
  scale_fill_continuous(low = "pink", high = "black")

ggplot(data = result, aes(x = long, y = lat,  group = group, fill = sidoname)) +
  geom_polygon(col = "white") +
  geom_text(aes(x= long, y = lat, label = text), data =add_text) +
  coord_map() +
  theme_map() 


ggplot() +
  geom_polygon(data = result, aes(x = long, y = lat,  group = group, fill = sidoname),
               col = "white") +
  geom_text(data = add_text, aes(x= long, y = lat, label = number), size = 5) +
  scale_fill_hue(l=65) +
  coord_map() +
  theme_map() +
  theme(legend.position = "none", title = element_blank(), axis.text = element_blank())


