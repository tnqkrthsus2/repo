# 줌 수업의 날짜별 학생들의 참석 시간이 기록된 csv 파일 사용
data <- read.csv("part.csv",header=TRUE)
# 불필요한 변수 제거
data <- data[c(1,8,10,11,12)]
# 변수명 변경
names(data) <- c("week","sn","start.time","end.time","minute")
library(dplyr)
# 한 개의 수업에 여러번의 접속 기록이 있는 학생의 접속 시간을 합한다.
new.data <- data %>%
  group_by(week,sn) %>%
  summarise(total=sum(minute))
#매 수업별 접속 시간의 평균의 90% 미달하는 학생을 지각으로 처리
mean(new.data$total)
week.mean <- new.data %>% group_by(week) %>%
  summarise(mean=mean(total))
minium <- week.mean
minium$mean <- minium$mean*0.9
minium$mean[1]
new.data <- merge(new.data,minium,by="week")
new.data <- new.data %>%
  mutate("attend"= ifelse(total>=mean,"Y","N"))
library(ggplot2)
attend.data <- new.data %>% group_by(week,attend) %>%
  summarise(n=n())
#매 수업 별 학생들의 출결 현황을 시각화
y.attend <- subset(attend.data,attend=="Y")
n.attend <- subset(attend.data,attend=="N")
ggplot(n.attend,aes(x=week,y=n)) +
  geom_bar(stat='identity')
ggplot(attend.data,aes(x=week,y=n,fill=attend)) +
  geom_bar(stat='identity')
