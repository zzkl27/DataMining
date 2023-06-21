library(stringr)
library(tidyverse)

if(tail(strsplit(getwd(), split="/")[[1]],n=1) != "study"){
  setwd("study") # 작업 공간 변경
}
sprintf("현재 작업 공간 : %s", getwd())

if(!dir.exists("RDATA")){
  dir.create("RDATA")
}

#### ================= < 데이터 설명 > ===================== ####
# ----- < 데이터 설명 > ---- #
# url : https://www.kaggle.com/datasets/arashnic/book-recommendation-dataset
# 책 추천 데이터
# books : 책 정보
# users : 사용자 정보
# raings : 책의 평점

# ----- < 변수 설명 > ---- #
# < books > # 
# ISBN(id) : 책의 고유 id
# Book-Title(title) : 책 이름
# Book-Author(author) : 책 저자
# Year-Of-Publication(year) : 발행 년도
# Publisher(pub) : 출판사
# Image-URL-S(제거) : 책 이미지(작은 크기)
# Image-URL-M(제거) : 책 이미지(중간 크기)
# Image-URL-L(제거) : 책 이미지(큰 크기)

# < users > # 
# User-ID(id) : 유저 고유 id
# Location(loc) : 유저의 지역
# Age(age) : 나이

# < ratings > # 
# User-ID(user_id) : 유저 고유 id(user의 User-ID와 동일)
# ISBN(book_id) : 책 고유 id(books의 ISBN와 동일)
# Book-Rating(rating) : 유저의 책 평가. [0,10]까지의 평가 기준으로 10일 수록 좋은 평가

# ---- < 저장된 데이터 가져오기 > ---- #
if(file.exists("RDATA/BookRecommentation.Rdata")){
  load("RDATA/BookRecommentation.Rdata")
  print("사용할 데이터 읽음")
}else{
  print("사용할 데이터를 읽어야함.")
}

#### ================= < 데이터 가져오기 > ===================== ####
# book데이터 읽기
# book의 속성준 텍스트 형 데이터가 csv에서 문제가 발생
# org_books <- read_csv("../../DATA/kaggle/BookRecommentation/Books.csv")
file_lines <- readLines("../../../DATA/kaggle/BookRecommentation/Books.csv")
check_csv <- function(line){
  # csv데이터의 한 행(줄)을 받아 ""의 개수가 짝수이면 ,로 데이터 구분
  comma_locations <- str_locate_all(line, ",")[[1]][,1]
  
  result = vector(mode='character')
  start = 1
  for(end in comma_locations){
    sub_str <- str_sub(line, start=start, end=end-1)
    
    if(str_count(sub_str, "\"")%%2 == 0){
      result <- append(result, sub_str)
      start = end+1
    }
  }
  
  if(end != length(line)){
    sub_str <- str_sub(line, start=start)
    result <- append(result, sub_str)
  }
  result
}
results <- lapply(X=file_lines, FUN=check_csv)
rm(file_lines) # 변수 제거

header <- results[[1]] # data.frame header 명
values <- results[2:length(results)] # data.frame 값
rm(results) # 변수 제거

org_books <- data.frame(array(vector(mode='character'), dim=c(length(values),8)))
names(org_books) <- header

for(idx in seq(1,length(values))){
  line <- values[[idx]]
  org_books[idx,] <- line
}
rm(header, values, idx, line, check_csv)

org_books <- tibble(org_books)
org_ratings <- read_csv("../../../DATA/kaggle/BookRecommentation/Ratings.csv")
org_users <- read_csv("../../../DATA/kaggle/BookRecommentation/Users.csv")

# ----- < 데이터 타입 설정 및 이름 변경 > ---- #
books <- org_books %>% 
  select(c("ISBN", "Book-Title", "Book-Author", "Year-Of-Publication", "Publisher")) %>% 
  rename(id=ISBN, title="Book-Title", author="Book-Author", year="Year-Of-Publication", pub="Publisher") %>% 
  mutate(pub = factor(pub), year=as.integer(year))
  
users <- org_users %>% 
  select(c("User-ID", "Location", "Age")) %>% 
  rename(id="User-ID", loc="Location", age="Age") %>% 
  mutate(age = as.integer(age), loc=factor(loc))

ratings <- org_ratings %>% 
  select(c("User-ID", "ISBN", "Book-Rating")) %>% 
  rename(user_id="User-ID", book_id="ISBN", rating="Book-Rating") %>% 
  mutate(rating = as.integer(rating))

rm(org_books, org_users, org_ratings)

# ---- < 데이터 저장 > ---- #

save.image("RDATA/BookRecommentation.Rdata")

# save(books, file="RDATA/BookRecommentation_books.Rdata")
# save(users, file="RDATA/BookRecommentation_users.Rdata")
# save(ratings, file="RDATA/BookRecommentation_ratings.Rdata")

#### ================= < 데이터 탐색 > ===================== ####
summary(books)
summary(users)
summary(ratings)

