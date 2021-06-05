### 날씨 데이터 ###
# 전체
ggplot(data, aes(Date, Precipitation, group=Area, color=Area),alpha=0.5)+geom_line()+scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
# 시도별
ggplot(data, aes(Date, Precipitation, group=Area, color=Area),alpha=0.5)+geom_line()+scale_x_date(date_breaks = "3 months" , date_labels = "%b-%y")+facet_wrap(~Area)
# 시도별 강수일수
ggplot(df_plot, aes(variable,value))+geom_bar(stat='identity', aes(fill=ifelse(variable=='sum_zero','red','blue')))+facet_wrap(~Area)+labs(fill = 'zero여부')


###함수형 배달 데이터###
# 평균
plot(df_copy$mean, type='l')
# 전체 
ggplot(df_del_plot1, aes(Date, value, group=variable, color=variable),alpha=0.5)+geom_line()+scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
# 전체와 평균 비교
ggplot(df_del_plot2, aes(Date, value, group=variable),alpha=0.5)+geom_line(aes(group=variable,color=ifelse(variable=='mean','#F8766D','grey')))+scale_x_date(date_breaks = "months" , date_labels = "%b-%y")
# 시도별
ggplot(df_del_plot1, aes(Date, value, group=variable, color=variable),alpha=0.5)+geom_line()+scale_x_date(date_breaks = "3 months" , date_labels = "%b-%y")+facet_wrap(~variable)
