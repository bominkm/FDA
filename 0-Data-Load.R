library(data.table)
library(ggplot2)
library(fda)
library(tidyverse)
library(showtext)
library(refund)
library(plot3D)
library(funFEM)
library(rgdal)
library(gam)
library(dplyr)
library(reshape2)
library(scales)

font_add_google("Noto Sans KR", "NSK")
showtext_auto()

## 경로 설정 
# setwd("C:/")

## 강수량 데이터
weather <- read.csv("강수량_함수.csv", fileEncoding = "euc-kr")
rownames(weather) <- seq(1,354)
weather <- weather[-307,]
weather <- weather[,-1]

## 배달데이터
delivery <- read.csv("배달주문건수_함수.csv", fileEncoding = "euc-kr")
rownames(delivery) <- seq(1,353)
delivery <- delivery[,-1]

## 스케일링
for(i in 1:17){
  delivery[,i] <- scale(delivery[,i])
}

## 지도 데이터
map <- readOGR("TL_SCCO_CTPRVN.shp")
