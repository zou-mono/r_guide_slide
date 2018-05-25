
### -------------------------------------------------------------------
### 示例1：时间价值分析
### -------------------------------------------------------------------
library(mlogit)

setwd("~/Documents/tex_projects/r_guide_slide")

data <- read.table("data/VoTSample2.csv", sep = ",", header = TRUE)

data <-
  subset(data,
         Time_car < 1000 & Time_bus < 1000 & 
           Time_metro < 1000 & Time_taxi < 1000 & 
           !is.na(Cost_car) & !is.na(Cost_metro) &
           !is.na(Cost_bus) & !is.na(Cost_taxi))

levels(data$mode) <- c("bus", "metro", "taxi", "car")
colnames(data)[which(names(data) == "Time_car")] <- "Time.car"
colnames(data)[which(names(data) == "Time_bus")] <- "Time.bus"
colnames(data)[which(names(data) == "Time_metro")] <- "Time.metro"
colnames(data)[which(names(data) == "Time_taxi")] <- "Time.taxi"
colnames(data)[which(names(data) == "Cost_car")] <- "Cost.car"
colnames(data)[which(names(data) == "Cost_bus")] <- "Cost.bus"
colnames(data)[which(names(data) == "Cost_metro")] <- "Cost.metro"
colnames(data)[which(names(data) == "Cost_taxi")] <- "Cost.taxi"

lst.data <- split(data, list(data$purpose, data$isCar))
lst.res <- lapply(lst.data, function(x) {
  logit_data <-
    mlogit.data(
      data = x,
      shape = "wide",
      varying = 4:11,
      choice = "mode"
    )
  mlogit(mode ~ Time + Cost | 0, logit_data)
})


logit_data <-
  mlogit.data(
    data = lst.data[[1]],
    shape = "wide",
    varying = 4:11,
    alt.var = "alt",
    choice = "mode"
  )

res <- mlogit(mode ~ Time + Cost | 0, logit_data, weights = weight)
summary(res)

r <- do.call("rbind",lst.res)
r <- data.frame(r)
r <- do.call("rbind",r$coefficients)



### -------------------------------------------------------------------
### 示例2：数据透视表
### -------------------------------------------------------------------
library(reshape2)
library(plyr)
#原始数据
head(airquality,20)

#把数据集airquality揉成按month、day和type字段组成的新数据框,并且val字段保存数值
mdata <- melt(airquality, id=c("Month", "Day"),variable.name = "type",value.name = "val")
str(mdata)

#也可以指定需要measure的变量
mdata2 <- melt(airquality, id=c("Month", "Day"),measure = c("Ozone","Wind"),variable.name = "type",value.name = "val")
str(mdata2)

#去除na值
mdata <- mdata[!is.na(mdata$val),]

#计算不同类型变量月平均值
monmean <- dcast(mdata, Month ~type ,mean)

#对每类变量的不重复值进行计算
count(mdata,vars="type")
