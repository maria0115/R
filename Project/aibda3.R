## Finding Teen Market Segments
# Exploring and preparing the data
teens <- read.csv("./datasets/snsdata.csv")
str(teens)

# look at missing data for female variable
table(teens$gender)
table(teens$gender, useNA = "ifany")

# look at missing data for age variable
summary(teens$age)

# eliminate age outliers
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)

summary(teens$age)

# reassign missing gender values to "unknown"
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# check our recoding work
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

# finding the mean age by cohort
mean(teens$age) # doesn't work
mean(teens$age, na.rm = TRUE) # works

# age by cohort
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# create a vector with the average age for each gradyear, repeated by person
ave_age <- ave(teens$age, teens$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))


teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

# check the summary results to ensure missing values are eliminated
summary(teens$age)

# create a z-score standardized data frame for easier interpretation
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))

# compare the data before and after the transformation
summary(interests$basketball)
summary(interests_z$basketball)

# create the clusters using k-means
set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)

# look at the size of the clusters
teen_clusters$size

# look at the cluster centers
teen_clusters$centers

# apply the cluster IDs to the original data frame
teens$cluster <- teen_clusters$cluster

# look at the first five records
teens[1:5, c("cluster", "gender", "age", "friends")]

# mean age by cluster
aggregate(data = teens, age ~ cluster, mean)

# proportion of females by cluster
aggregate(data = teens, female ~ cluster, mean)

# mean number of friends by cluster
aggregate(data = teens, friends ~ cluster, mean)


## 공공데이터 활용

# 미세먼지 XML 문서 출력
install.packages("XML")
install.packages("ggplot2")
library(XML)
library(ggplot2)
api <- "http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst"
api_key <- "g096ZBtifZ1PgsemrJxwCNIhW4r4gv2ohQvgSk5udZpqFH54or%2Fv9YqWc8ruDvoddJ63HUZSisAnEhKAsAFSEw%3D%3D"

numOfRows <- 10
pageNo <- 1
itemCode <- "PM10"
dataGubun <- "HOUR"
searchCondition <- "MONTH"
url <- paste(api,
             "?serviceKey=", api_key,
             "&returnType=xml", 
             "&numOfRows=", numOfRows,
             "&pageNo=", pageNo,
             "&itemCode=", itemCode,
             "&dataGubun=", dataGubun,
             "&searchCondition=", searchCondition,
             sep="")
url             
xmlFile <- xmlParse(url)
xmlRoot(xmlFile)

# XML 문서를 데이터 프레임으로 변환
df <- xmlToDataFrame(getNodeSet(xmlFile, "//items/item"))
df

# 미세먼지 농도의 그래프
ggplot(data=df, aes(x=dataTime, y=seoul)) +
  geom_bar(stat="identity", fill="green")

# 라벨 수정
ggplot(data=df, aes(x=dataTime, y=seoul)) +
  geom_bar(stat="identity", fill="green") +
  theme(axis.text.x=element_text(angle=90)) +
  labs(title="시간대별 서울지역의 미세먼지 농도 변화", x = "측정일시", y = "농도")

# 막대 색
ggplot(data=df, aes(x=dataTime, y=seoul, fill=dataTime)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90)) +
  labs(title="시간대별 서울지역의 미세먼지 농도 변화", x = "측정일시", y = "농도") +
  scale_fill_manual(values = rainbow(10))

# 범례 삭제
ggplot(data=df, aes(x=dataTime, y=seoul, fill=dataTime)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90),
        legend.position="none") +
  labs(title="시간대별 서울지역의 미세먼지 농도 변화", x = "측정일시", y = "농도") +
  scale_fill_manual(values = rainbow(10))

# 가로 막대 출력
ggplot(data=df, aes(x=dataTime, y=seoul, fill=dataTime)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") +
  labs(title="시간대별 서울지역의 미세먼지 농도 변화", x = "측정일시", y = "농도") +
  scale_fill_manual(values = rainbow(10)) +
  coord_flip()

# 지역별 미세먼지 농도 비교: 지도

# 미세먼지 XML 문서 출력
#install.packages("XML")
# install.packages("ggplot2")
install.packages("ggmap")
# library(XML)
# library(ggplot2)
library(ggmap)
api <- "http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureSidoLIst"
api_key <- "g096ZBtifZ1PgsemrJxwCNIhW4r4gv2ohQvgSk5udZpqFH54or%2Fv9YqWc8ruDvoddJ63HUZSisAnEhKAsAFSEw%3D%3D"
numOfRows <- 10
pageNo <- 1
sidoName <- "서울"
searchCondition <- "DAILY"

url <- paste(api,
             "?serviceKey=", api_key,
             "&returnType=xml",
             "&numOfRows=", numOfRows,
             "&pageNo=", pageNo,
             "&sidoName=", sidoName,
             "&searchCondition=", searchCondition,
             sep="")
url
xmlFile <- xmlParse(url)
xmlRoot(xmlFile)

# 특정 시간대의 지역별 미세먼지 농도 추출
df <- xmlToDataFrame(getNodeSet(xmlFile, "//items/item"))
df

pm <- df[1, 4:20]
pm

# 지역별 미세먼지 농도의 지도 출력
register_google(key = "AI...8")
cities <- c("서울시", "부산시", "대구시", "인천시", "광주시",
            "대전시", "울산시", "경기도", "강원도", "충청북도",
            "충청남도", "전라북도", "전라남도", "경상북도", "경상남도",
            "제주시", "세종시")
gc <- geocode(enc2utf8(cities))
gc

df2 <- data.frame(지역명=cities,
                     미세먼지=t(pm),
                     경도=gc$lon,
                     위도=gc$lat,
                     stringsAsFactors = F)
df2

names(df2)[2] <- "미세먼지"
df2

str(df2)

df2[,2] <- as.numeric(df2[,2])
cen <- as.numeric(geocode(enc2utf8("전라북도")))
map <- get_googlemap(center=cen, zoom=7)

ggmap(map) +
  geom_point(data=df2,
             aes(x=경도, y=위도),
             color=rainbow(length(df2$미세먼지)),
             size=df2$미세먼지 * 0.3,
             alpha=0.5)
