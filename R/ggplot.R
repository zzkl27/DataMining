# install.packages("tidyverse")
library("tidyverse")

#### ================= < 데이터 가져오기 > ===================== ####
house_price <- read_csv("../../DATA/kaggle/HousePropertySalesTimeSeries/raw_sales.csv")
house_price
house_price$bedrooms <- factor(house_price$bedrooms, levels=c(0,1,2,3,4,5)) # 명목 변수
house_price$postcode <- factor(house_price$postcode) # 명목 변수
house_price$propertyType <- factor(house_price$propertyType) # 명목 변수
summary(house_price)
# ----- < 변수 설명 > ---- #
# datasold : 판매 날짜
# postcode : 4자리 우편 번호(집이 있는 지역과 연관있음)
# price : 판매 가격
# propertyType : 판매 유형(house/unit)
# bedrooms : 침실 수

#### ======================= < 기본 사용법 > ======================= ####
# ---- [1] 산점도 ---- # 
ggplot(data=house_price) + 
  geom_point(aes(x=datesold, y=price, color=bedrooms)) 
# x축 : 시간, y축 : 가격
# color : bedrooms 별로 색상 지정

date_house_sale_count <- house_price %>% group_by(datesold) %>% count(name='count')
ggplot(data=date_house_sale_count) + 
  geom_line(aes(x=datesold, y=count, color="red"))


