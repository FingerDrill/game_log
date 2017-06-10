library(readxl)
library(dplyr)
library(ggplot2)
library(Hmisc)
library(lubridate)


# data load ---------------------------------------------------------------

rm(list=ls())
data <- read_excel("./gamevil/dataset_log.xlsx")


# dataset information -------------------------------------------------------

str(data)
describe(data)

# country NA값 594개
data %>% filter(is.na(country)) %>% nrow()



# preprocessing -----------------------------------------------------------

# country 값이 1~92까지 있음,
# 편의상 NA를 99로 처리함.
data <- 
  data %>% 
    mutate(country=replace(country, is.na(country), 99))

# 팩터화
data$market   <- as.factor(data$market)
data$country  <- as.factor(data$country)

# 날짜 형변환
data$log_date <- as.Date(data$log_date)

# 유저별 플레이 횟수, 내림차순
user_Nplay <-
  data %>% 
    group_by(user_id) %>% 
    summarise(play_count=n()) %>% 
    arrange(desc(play_count))

# 원본 별도백업
original <- data

# 플레이 날짜 횟수
(data %>% 
    group_by(user_id) %>% 
    summarise( unique_date = length(unique(log_date))) %>% 
    filter(unique_date>15) %>% 
    nrow())


# 실패한 시도 : 플레이 많은 유저=고과금 유저 라는 가정이 틀린듯. -------------------------------
# # 예상과는 달리, 과금이 많을수록 실패비율이 적어지지 않음(반대에 가까움)
# 
# 
# # 각 과금구간별 유저 수 계산
# (n_high  = round(nrow(user_Nplay)*0.164/100))
# (n_mid   = round(nrow(user_Nplay)*0.902/100))
# (n_low   = round(nrow(user_Nplay)*3.454/100))
# (n_free = nrow(user_Nplay) - n_high - n_mid - n_low)  # 95.48%
# 
# 
# # 각 과금구간별 유저 리스트
# user_high   <- user_Nplay[1:n_high,]
# user_mid    <- user_Nplay[n_high+(1:n_mid),]
# user_low    <- user_Nplay[(n_high+n_mid)+(1:n_low),]
# user_free   <- user_Nplay[(n_high+n_mid+n_low)+(1:n_free),]
# 
# 
# # 각 과금 구간에 속하는 유저들의 플레이 데이터 파티셔닝
# data <- 
#   data %>% mutate(group=ifelse(user_id %in% user_high$user_id, "high", NA))
# data <- 
#   data %>% mutate(group=ifelse(user_id %in% user_mid$user_id, "mid", group))
# data <- 
#   data %>% mutate(group=ifelse(user_id %in% user_low$user_id, "low", group))
# data <- 
#   data %>% mutate(group=ifelse(user_id %in% user_free$user_id, "free", group))
# 
# 
# # 스테이지별 스코어 비율
# scoreRatioByStage <- 
#   ggplot(data) + 
#     geom_bar(aes(x=stage, fill=factor(score)), position="fill") +
#     scale_x_continuous(breaks=c(1:30), limits=c(0, 31)) +
#     facet_wrap(~group)
# print(scoreRatioByStage)


# 그래프 사이즈값 ----------------------------------------------------------------
# 참고로, 가로 36이면 ppt 한페이지에 가득 차는데, 18로 하면 두장 넣기에 공간이 모자란다.
plotsize.width  <- 16
plotsize.height <- 9
fontsize.small  <- 8
fontsze.medium  <- 12
fontsize.big    <- 13



# 전체 그래프 생성 ---------------------------------------------------------------

# stage별 score 비율
plot_scoreByStage <- 
  ggplot(data) +
    geom_bar(aes(x=stage, fill=factor(score))) +
    scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
    theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
          legend.text=element_text(size=fontsize.small), legend.title=element_text(size=fontsize.big),
          plot.title=element_text(size=fontsize.big)) +
    scale_fill_manual(values=c("#47aa5e", "#fad875", "#f6b24a", "#f38c3a")) +
    labs(fill="스코어", title="[ 스테이지별 플레이 횟수 및 스코어 분포 ]", y="플레이 횟수", x="스테이지")
print(plot_scoreByStage)
ggsave("./gamevil/images/plot_scoreByStage.png", plot=plot_scoreByStage,
       width=plotsize.width, height=plotsize.height, units="cm")

# stage별 score 비율
plot_scoreRatioByStage <- 
  ggplot(data) +
  geom_bar(aes(x=stage, fill=factor(score)), position="fill") +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        legend.text=element_text(size=fontsize.small), legend.title=element_text(size=fontsize.big),
        plot.title=element_text(size=fontsize.big)) +
  scale_fill_manual(values=c("#47aa5e", "#fad875", "#f6b24a", "#f38c3a")) +
  labs(fill="스코어", title="[ 스테이지별 스코어 비율 ]", y="스코어 비율", x="스테이지")
print(plot_scoreRatioByStage)
ggsave("./gamevil/images/plot_scoreRatioByStage.png", plot=plot_scoreRatioByStage,
       width=plotsize.width, height=plotsize.height, units="cm")


# play n histogram
plot_play_count <- 
  ggplot(user_Nplay) +
  geom_histogram(aes(x=play_count), binwidth=1, fill="#7cb5ec", alpha=0.75) +
  scale_x_continuous( breaks=c(1:40), limits=c(0, 41), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big)) +
  labs(x="플레이 횟수", y="유저수", title="[ 플레이 횟수별 유저수 분포 ]")
print(plot_play_count)
ggsave("./gamevil/images/plot_play_count.png", plot=plot_play_count,
       width=plotsize.width+5, height=plotsize.height, units="cm")



# 이탈, 매니아 관련 변수 생성 ---------------------------------------------------------


# 이탈 예측이 아닌, 이탈로 이어지는 높은 난이도의 스테이지를 찾는게 목적.
# 높지만 이탈로 보기에 확실한 수치인 10일을 기준으로 하여 변수 생성
lastApril   <- ymd("2017-04-30")
firstApril  <- ymd("2017-04-01")


# 유저별 플레이 날짜간 차이 계산
# user_id, log_date에 대한 오름차순 정렬하여
asc_data <- 
  data %>% arrange(user_id, log_date)
# 직전 행과 계산을 위한 데이터셋 생성
prev_rows <-
  bind_rows( data.frame(user_id=NA, log_date=NA, 
                        market=NA, country=NA, stage=NA, score=NA), asc_data)
prev_rows <- prev_rows[-nrow(prev_rows), ]
# 직전행과 user_id가 같으면 날짜 차이 계산, 다르면 0
asc_data <- 
  asc_data %>% 
  mutate(daydiff=ifelse(user_id==prev_rows$user_id, 
                        as.numeric(log_date-prev_rows$log_date), 0))
asc_data[1, 7] <- 0


# 마지막 플레이 이후 미 플레이 기간
# 모든 날짜와 1일, 30일과의 차이 계산후 나중에 max나 min으로 처리.
asc_data <- 
  asc_data %>% 
    mutate(aft_last=as.numeric(lastApril-log_date),
           bfr_first=as.numeric(log_date-firstApril))


# 유저별로 변수값 계산
features <- 
  asc_data %>% 
    group_by(user_id) %>% 
    summarise(daydiff=max(daydiff), aft_last=min(aft_last), 
              bfr_first=min(bfr_first), day_n=length(unique(log_date))) %>% 
    select(user_id, daydiff, aft_last, bfr_first, day_n)

# user_Nplay 테이블과 조인
user_Nplay <- inner_join(user_Nplay, features, by="user_id")


# 플레이/반복 횟수가 많지 않으므로, 이탈에는 높은 기준, 매니아에는 낮은 기준 적용
# daydiff 또는 aft_last가 20초과면 이탈로 판단하는 변수 churn 생성
# 매니아는 8일 이내의 접속간격 및 6일 이상의 접속일을 가진 유저
# 이탈/매니아를 배타적 조건으로 만들위 위해 매니아에 aft_last<=20 조건 추가
user_Nplay <- 
  user_Nplay %>% 
    mutate(mania = ifelse(daydiff<8 & day_n>6 & aft_last<=20, 1, 0),
           churn = ifelse(daydiff>20 | aft_last>20, 1, 0))


# 매니아 유저 목록
user_mania    <- user_Nplay %>% filter(mania==1) %>% select(user_id) %>% unlist()
# 이탈 유저 목록
user_churn  <- user_Nplay %>% filter(churn==1) %>% select(user_id) %>% unlist()




# 매니아 유저 그래프 : 횟수 - 비율 - 이상/보스 제외 -----------------------------------------
plot_mania_n <- 
  ggplot(data %>% filter(user_id %in% user_mania)) +
  geom_bar(aes(x=stage, fill=factor(score))) +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        legend.text=element_text(size=fontsize.small), legend.title=element_text(size=fontsize.big),
        plot.title=element_text(size=fontsize.big)) +
  scale_fill_manual(values=c("#47aa5e", "#fad875", "#f6b24a", "#f38c3a")) +
  labs(fill="스코어", title="[ 스테이지별 플레이 횟수 및 스코어 분포(매니아) ]", y="플레이 횟수", x="스테이지")
print(plot_mania_n)
ggsave("./gamevil/images/plot_mania_n.png", plot=plot_mania_n,
       width=plotsize.width, height=plotsize.height, units="cm")

plot_mania_r <- 
  ggplot(data %>% filter(user_id %in% user_mania)) +
  geom_bar(aes(x=stage, fill=factor(score)), position="fill") +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        legend.text=element_text(size=fontsize.small), legend.title=element_text(size=fontsize.big),
        plot.title=element_text(size=fontsize.big)) +
  scale_fill_manual(values=c("#47aa5e", "#fad875", "#f6b24a", "#f38c3a")) +
  labs(fill="스코어", title="[ 스테이지별 스코어 비율(매니아) ]", y="스코어 비율", x="스테이지")
print(plot_mania_r)
ggsave("./gamevil/images/plot_mania_r.png", plot=plot_mania_r,
       width=plotsize.width, height=plotsize.height, units="cm") 

plot_mania_r_subset <-  
  ggplot(data %>% 
           filter(user_id %in% user_mania, !stage %in% c(4, 11, 12, 14, 16, 18, 26, 27, 5, 10, 15, 20, 25, 30, 22, 23, 24))) +
  geom_bar(aes(x=factor(stage), fill=factor(score)), position="fill") +
  scale_x_discrete( expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        legend.text=element_text(size=fontsize.small), legend.title=element_text(size=fontsize.big),
        plot.title=element_text(size=fontsize.big)) +
  scale_fill_manual(values=c("#47aa5e", "#fad875", "#f6b24a", "#f38c3a")) +
  labs(fill="스코어", title="[ 스테이지별 스코어 비율(매니아) ]", y="스코어 비율", x="스테이지")
print(plot_mania_r_subset)
ggsave("./gamevil/images/plot_mania_r_subset.png", plot=plot_mania_r_subset,
       width=plotsize.width, height=plotsize.height, units="cm") 

temp_mania <- 
  data %>% 
    filter(user_id %in% user_mania, 
           !stage %in% c(4, 11, 12, 14, 16, 18, 26, 27, 5, 10, 15, 20, 25, 30, 22, 23, 24))
ratio_mania <- 
  temp_mania %>% group_by(stage) %>% 
  summarise(fail_n=sum(score==0), count=n() ) %>% 
  mutate(fail_ratio= round((fail_n/count)*100, digits=1)) %>% 
  select(-fail_n, -count)
reg_mania <- lm(fail_ratio~stage, ratio_mania)

plot_reg_mania <- 
  ggplot(ratio_mania, aes(x=stage, y=fail_ratio)) + 
  geom_point() +
  geom_smooth(method=lm) +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  scale_y_continuous( breaks=seq(0, 100, 10), limits=c(0, 100), expand=c(0, 0) ) +
  labs(title="[ 스테이지별 실패율(매니아) ]", y="실패율", x="스테이지")
print(plot_reg_mania)
ggsave("./gamevil/images/plot_reg_mania.png", plot=plot_reg_mania,
       width=plotsize.width, height=plotsize.height, units="cm") 





# 이탈 유저 그래프 : 횟수 - 비율 - 이상/보스 제외 ------------------------------------------

plot_churn_n <- 
  ggplot(data %>% filter(user_id %in% user_churn)) +
  geom_bar(aes(x=stage, fill=factor(score))) +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        legend.text=element_text(size=fontsize.small), legend.title=element_text(size=fontsize.big),
        plot.title=element_text(size=fontsize.big)) +
  scale_fill_manual(values=c("#47aa5e", "#fad875", "#f6b24a", "#f38c3a")) +
  labs(fill="스코어", title="[ 스테이지별 플레이 횟수 및 스코어 분포(이탈) ]", y="플레이 횟수", x="스테이지")
print(plot_churn_n)
ggsave("./gamevil/images/plot_churn_n.png", plot=plot_churn_n,
       width=plotsize.width, height=plotsize.height, units="cm")

plot_churn_r <- 
  ggplot(data %>% filter(user_id %in% user_churn)) +
  geom_bar(aes(x=stage, fill=factor(score)), position="fill") +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        legend.text=element_text(size=fontsize.small), legend.title=element_text(size=fontsize.big),
        plot.title=element_text(size=fontsize.big)) +
  scale_fill_manual(values=c("#47aa5e", "#fad875", "#f6b24a", "#f38c3a")) +
  labs(fill="스코어", title="[ 스테이지별 스코어 비율(이탈) ]", y="스코어 비율", x="스테이지")
print(plot_churn_r)
ggsave("./gamevil/images/plot_churn_r.png", plot=plot_churn_r,
       width=plotsize.width, height=plotsize.height, units="cm")

plot_churn_r_subset <-  
  ggplot(data %>% 
           filter(user_id %in% user_churn, !stage %in% c(4, 11, 12, 14, 16, 18, 26, 27, 5, 10, 15, 20, 25, 30, 22, 23, 24))) +
  geom_bar(aes(x=factor(stage), fill=factor(score)), position="fill") +
  scale_x_discrete( expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        legend.text=element_text(size=fontsize.small), legend.title=element_text(size=fontsize.big),
        plot.title=element_text(size=fontsize.big)) +
  scale_fill_manual(values=c("#47aa5e", "#fad875", "#f6b24a", "#f38c3a")) +
  labs(fill="스코어", title="[ 스테이지별 스코어 비율(이탈) ]", y="스코어 비율", x="스테이지")
print(plot_churn_r_subset)
ggsave("./gamevil/images/plot_churn_r_subset.png", plot=plot_churn_r_subset,
       width=plotsize.width, height=plotsize.height, units="cm") 

temp_churn <- 
  data %>% 
  filter(user_id %in% user_churn, 
         !stage %in% c(4, 11, 12, 14, 16, 18, 26, 27, 5, 10, 15, 20, 25, 30, 22, 23, 24))
ratio_churn <- 
  temp_churn %>% group_by(stage) %>% 
  summarise(fail_n=sum(score==0), count=n() ) %>% 
  mutate(fail_ratio= round((fail_n/count)*100, digits=1)) %>% 
  select(-fail_n, -count)
reg_churn <- lm(fail_ratio~stage, ratio_churn)

plot_reg_churn <- 
  ggplot(ratio_churn, aes(x=stage, y=fail_ratio)) + 
  geom_point() +
  geom_smooth(method=lm) +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  scale_y_continuous( breaks=seq(0, 100, 10), limits=c(0, 100), expand=c(0, 0) ) +
  labs(title="[ 스테이지별 실패율(이탈) ]", y="실패율", x="스테이지")
print(plot_reg_churn)
ggsave("./gamevil/images/plot_reg_churn.png", plot=plot_reg_churn,
       width=plotsize.width, height=plotsize.height, units="cm") 






# 재도전 관련 ------------------------------------------------------------------

# 재도전 총 횟수 / 보상목적(3성인데 재도전) / 클리어를 위한 재도전(0성 재도) 파악
replay_features <- data
# 각 조건별 도전 횟수 카운팅
replay_features <- 
  replay_features %>% 
    group_by(user_id, stage) %>% 
    mutate(replay = sum(score!=0),
           replay_3 = sum(score==3), 
           replay_0 = sum(score==0)) %>% 
    arrange(user_id, stage)
# 재도전 횟수(도전횟수-1) 계산
replay_features <- 
  replay_features %>% 
    mutate(replay = ifelse(replay!=0, replay-1, 0),
           replay_3 = ifelse(replay_3!=0, replay_3-1, 0),
           replay_0 = ifelse(replay_0!=0, replay_0-1, 0))
replay_features <- 
  replay_features %>% 
    group_by(user_id, stage) %>% 
    summarise(replay=max(replay), replay_3=max(replay_3), replay_0=max(replay_0))
replay_features_all <- 
  replay_features %>% 
    filter(replay>0 | replay_3>0 | replay_0>0) %>% 
    group_by(stage) %>% 
    summarise(replay=sum(replay), replay_3=sum(replay_3), replay_0=sum(replay_0))
replay_features_mania <- 
  replay_features %>% 
    filter(user_id %in% user_mania & (replay>0 | replay_3>0 | replay_0>0) ) %>% 
    group_by(stage) %>% 
    summarise(replay=sum(replay), replay_3=sum(replay_3), replay_0=sum(replay_0))
replay_features_churn <- 
  replay_features %>% 
  filter(user_id %in% user_churn & (replay>0 | replay_3>0 | replay_0>0) ) %>% 
  group_by(stage) %>% 
  summarise(replay=sum(replay), replay_3=sum(replay_3), replay_0=sum(replay_0))

  
  
  

# 전체 대상 재도전 그래프
plot_replay <- 
  ggplot(replay_features_all) +
  geom_bar(aes(x=stage, y=replay), stat="identity", fill="#7cb5ec", alpha=0.75) + 
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        plot.title=element_text(size=fontsize.big)) +
  labs(title="[ 스테이지별 재도전 횟수 ]", y="재도전 횟수", x="스테이지")
print(plot_replay)
ggsave("./gamevil/images/plot_replay.png", plot=plot_replay,
       width=plotsize.width, height=plotsize.height, units="cm")


plot_replay_3 <- 
  ggplot(replay_features) +
  geom_bar(aes(x=stage, y=replay_3), stat="identity", fill="#7cb5ec", alpha=0.75) + 
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        plot.title=element_text(size=fontsize.big)) +
  labs(title="[ 스테이지별 3성 클리어 재도전 횟수 ]", y="재도전 횟수", x="스테이지")
print(plot_replay_3)
ggsave("./gamevil/images/plot_replay_3.png", plot=plot_replay_3,
       width=plotsize.width, height=plotsize.height, units="cm")


plot_replay_0 <- 
  ggplot(replay_features) +
  geom_bar(aes(x=stage, y=replay_0), stat="identity", fill="#7cb5ec", alpha=0.75) +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        plot.title=element_text(size=fontsize.big)) +
  labs(title="[ 스테이지별 실패 재도전 횟수 ]", y="재도전 횟수", x="스테이지")
print(plot_replay_0)
ggsave("./gamevil/images/plot_replay_0.png", plot=plot_replay_0,
       width=plotsize.width, height=plotsize.height, units="cm")



# 매니아 대상 재도전 그래프
plot_replay_3_mania <- 
  ggplot(replay_features_mania) +
  geom_bar(aes(x=stage, y=replay_3), stat="identity", fill="#7cb5ec", alpha=0.75) + 
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        plot.title=element_text(size=fontsize.big)) +
  labs(title="[ 스테이지별 3성 재도전 횟수(매니아) ]", y="재도전 횟수", x="스테이지")
print(plot_replay_3_mania)
ggsave("./gamevil/images/plot_replay_3_mania.png", plot=plot_replay_3_mania,
       width=plotsize.width, height=plotsize.height, units="cm")


plot_replay_0_mania <- 
  ggplot(replay_features_mania) +
  geom_bar(aes(x=stage, y=replay_0), stat="identity", fill="#7cb5ec", alpha=0.75) +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        plot.title=element_text(size=fontsize.big)) +
  labs(title="[ 스테이지별 실패 재도전 횟수(매니아) ]", y="재도전 횟수", x="스테이지")
print(plot_replay_0_mania)
ggsave("./gamevil/images/plot_replay_0_mania.png", plot=plot_replay_0_mania,
       width=plotsize.width, height=plotsize.height, units="cm")

# 이탈 대상 재도전 그래프
plot_replay_3_churn <- 
  ggplot(replay_features_churn) +
  geom_bar(aes(x=stage, y=replay_3), stat="identity", fill="#7cb5ec", alpha=0.75) + 
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        plot.title=element_text(size=fontsize.big)) +
  labs(title="[ 스테이지별 3성 재도전 횟수(이탈) ]", y="재도전 횟수", x="스테이지")
print(plot_replay_3_churn)
ggsave("./gamevil/images/plot_replay_3_churn.png", plot=plot_replay_3_churn,
       width=plotsize.width, height=plotsize.height, units="cm")


plot_replay_0_churn <- 
  ggplot(replay_features_churn) +
  geom_bar(aes(x=stage, y=replay_0), stat="identity", fill="#7cb5ec", alpha=0.75) +
  scale_x_continuous( breaks=c(1:30), limits=c(0, 31), expand=c(0, 0) ) +
  theme(axis.text=element_text(size=fontsize.small), axis.title=element_text(size=fontsize.big), 
        plot.title=element_text(size=fontsize.big)) +
  labs(title="[ 스테이지별 실패 재도전 횟수(이탈) ]", y="재도전 횟수", x="스테이지")
print(plot_replay_0_churn)
ggsave("./gamevil/images/plot_replay_0_churn.png", plot=plot_replay_0_churn,
       width=plotsize.width, height=plotsize.height, units="cm")


