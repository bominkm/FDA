### 날씨 데이터 ###
# 지역명
강릉$Area = '강원'; 경기$Area = '경기'; 경남$Area = '경남'; 경북$Area = '경북'; 광주$Area = '광주'
대구$Area = '대구'; 대전$Area = '대전'; 부산$Area = '부산'; 서울$Area = '서울'; 세종$Area = '세종'
울산$Area = '울산'; 인천$Area = '인천'; 전남$Area = '전남'; 전북$Area = '전북'; 제주$Area = '제주'
충남$Area = '충남'; 충북$Area = '충북'
# 데이터 합치기
data <- rbind(강릉,경기,경남,경북,광주,대구,대전,부산,서울,세종,울산,인천,전남,전북,제주,충남,충북)
names(data) <- c("Date","Area.num","Precipitation","Area")
# 결측치 채우기
data[is.na(data)] <- 0
# 데이터 재구조화
df_cast <- dcast(data, Date ~ Area, value.var = 'Precipitation')
row.names(df_cast) <- seq(1,355)
df_ftn <- df_cast[c(-355),c(-1)] # 2020-07-05 제거
df_match <- df_ftn[c(-307),] # 2020-05-18 제거
write.csv(df_ftn, '강수량_함수.csv', fileEncoding='euc-kr')


### 배달 데이터 ###
# 기본 데이터
colnames(delivery) <- c("Date", "Area", "Delivery")
group_delivery <- delivery %>% group_by(Date) %>% summarize(Delivery_max=max(Delivery), Delivery_mean=mean(Delivery), Delivery_sum=sum(Delivery))
Y_max <- group_delivery$Delivery_max 
Y_mean <- group_delivery$Delivery_mean
Y_sum <- group_delivery$Delivery_sum
group_delivery$Date <- as.Date(group_delivery$Date)
data$zero <- ifelse(data$Precipitation==0,1,0)
df_zero <- data%>%group_by(Area) %>% summarise(sum_zero = sum(zero)) %>% mutate(sum_nonzero = 353-sum_zero)
df_plot <- melt(df_zero)
# 함수형 데이터
df_del <- fdelivery[,-1]
names(fdelivery)[1] <- c("Date")
# 지역별로 스케일링
df_copy <- fdelivery
df_copy[,2] <- scale(df_copy[,2])
df_copy[,3] <- scale(df_copy[,3])
df_copy[,4] <- scale(df_copy[,4])
df_copy[,5] <- scale(df_copy[,5])
df_copy[,6] <- scale(df_copy[,6])
df_copy[,7] <- scale(df_copy[,7])
df_copy[,8] <- scale(df_copy[,8])
df_copy[,9] <- scale(df_copy[,9])
df_copy[,10] <- scale(df_copy[,10])
df_copy[,11] <- scale(df_copy[,11])
df_copy[,12] <- scale(df_copy[,12])
df_copy[,13] <- scale(df_copy[,13])
df_copy[,14] <- scale(df_copy[,14])
df_copy[,15] <- scale(df_copy[,15])
df_copy[,16] <- scale(df_copy[,16])
df_copy[,17] <- scale(df_copy[,17])
df_copy[,18] <- scale(df_copy[,18])
df_scale <- df_copy[,-1]
df_del_plot <- melt(df_copy, id.vars='Date')
df_copy$mean<-apply(df_copy[,-1],1,mean)

### 지도 데이터 ###
df_map_info <- map@data
df_map <- fortify(map)
df_map$sido<-ifelse(df_map$id==0,'강원도',ifelse(df_map$id==1,'경기도',ifelse(df_map$id==2,'경상남도',ifelse(df_map$id==3,'경상북도',ifelse(df_map$id==4,'광주',ifelse(df_map$id==5,'대구',ifelse(df_map$id==6,'대전',ifelse(df_map$id==7,'부산',ifelse(df_map$id==8,'서울',ifelse(df_map$id==9,'세종',ifelse(df_map$id==10,'울산',ifelse(df_map$id==11,'인천',ifelse(df_map$id==12,'전라남도',ifelse(df_map$id==13,'전라북도',ifelse(df_map$id==14,'제주도',ifelse(df_map$id==15,'충청남도','충청북도'))))))))))))))))
