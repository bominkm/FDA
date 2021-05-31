# 디렉토리 지정
setwd('/Users/bomin/user/mymac/대학원/21-1 자료분석특론/프로젝트/데이터')

# 날씨 데이터
강릉 = read.table('강수량_강릉.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
경기 = read.table('강수량_경기.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
경남 = read.table('강수량_경남.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
경북 = read.table('강수량_경북.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
광주 = read.table('강수량_광주.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
대구 = read.table('강수량_대구.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
대전 = read.table('강수량_대전.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
부산 = read.table('강수량_부산.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
서울 = read.table('강수량_서울.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
세종 = read.table('강수량_세종.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
울산 = read.table('강수량_울산.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
인천 = read.table('강수량_인천.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
전남 = read.table('강수량_전남.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
전북 = read.table('강수량_전북.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
제주 = read.table('강수량_제주.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
충남 = read.table('강수량_충남.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')
충북 = read.table('강수량_충북.csv', fileEncoding='euc-kr', skip = 7, header = TRUE, sep = ',')

# 배달 데이터
delivery <- read.csv('시간별배달데이터.csv',fileEncoding = "euc-kr")
fdelivery <- read.csv('최종_배달_fda.csv',fileEncoding = "euc-kr")

# 지도 데이터
map <- readOGR("TL_SCCO_CTPRVN.shp")
