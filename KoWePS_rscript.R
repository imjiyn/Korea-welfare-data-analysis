install.packages("ggplot2")
install.packages("foreign")
install.packages(c("dplyr","hflights"))
install.packages("readxl")
library(ggplot2)
library(foreign)
library(dplyr)
library(hflights)
library(readxl)

#####1) load data and change variable names
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                         to.data.frame = T)

welfare <- raw_welfare
head(welfare)
View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

welfare <- rename(welfare, 
                  sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7)

#####2) income by gender (sex)
##1. check variable sex and pre-process
class(welfare$sex)
table(welfare$sex)

welfare$sex <- ifelse(welfare$sex == 1, 
                      "male",
                      "female")
table(welfare$sex)
table(is.na(welfare$sex))
qplot(welfare$sex)

##2. check variable income and pre-process
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0,1000)

income <- ifelse(welfare$income %in% c(0,9999),
                         NA,
                         welfare$income)
table(is.na(income))

##3. income by gender
sex_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))
sex_income
ggplot(data=sex_income, 
       aes(x=sex,y=mean_income)) + geom_col()

#####3) income by age
##1. create age derivative variable
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
table(is.na(welfare$birth))
welfare$birth <- ifelse(welfare$birth == 9999,
                        NA,
                        welfare$birth)
welfare$age <- 2015-welfare$birth+1
summary(welfare$age)
qplot(welfare$age)

##2. income by age
age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age) %>% 
  summarise(mean_income = mean(income))
head(age_income)
ggplot(data=age_income, 
       aes(x=age, y=mean_income)) + geom_line()

##3. income by age group
#compute new column by mutate()
welfare <- welfare %>% 
  mutate(ageg = ifelse(age<30,"young",
                       ifelse(age<=59,"middle","old")))
table(welfare$ageg)
qplot(welfare$ageg)

ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(ageg) %>% 
  summarise(mean_income = mean(income))
ageg_income
ggplot(data=ageg_income, 
       aes(x=ageg, y=mean_income)) +
  geom_col() +
  scale_x_discrete(limits = c("young","middle","old"))
#scale_x_discrete(limits=c()) order category

#####4) income by gender and age
sex_ageg_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex,ageg) %>% 
  summarise(mean_income = mean(income))
sex_ageg_income
ggplot(data=sex_ageg_income,
       aes(x=ageg, y=mean_income, fill=sex)) +
  geom_col(position="dodge") +
  scale_x_discrete(limits=c("young","middle","old"))
#ages(fill=) separates color
#position parameter "dodge" separates columns

#####5) income by gender and age
sex_age_income <- welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex,age) %>% 
  summarise(mean_income=mean(income))
ggplot(data=sex_age_income,
       aes(x=age,y=mean_income,col=sex))+
  geom_line()

#####6) income by job
##1. pre-process code_job variable
class(welfare$code_job)
table(welfare$code_job)

list_job <- read_excel("Koweps_Codebook.xlsx",
                       col_names=T, sheet=2)
head(list_job)
welfare <- left_join(welfare,list_job, by=c("code_job"="code_job"))
welfare %>% 
  filter(!is.na(code_job)) %>% 
  select(code_job,job) %>% 
  head(10)

##2. income by job
job_income <- welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
head(job_income)

##3. top 10 job
top10 <- job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)
top10

ggplot(data=top10,
       aes(x=reorder(job,mean_income),
           y=mean_income)) +
  geom_col()+
  coord_flip()
#reorder() treats 1st argument as categorical var and reorder based on values of 2nd var
#coord_flip() flips the plot 90°

bottom10 <- job_income %>% 
  arrange(mean_income) %>% 
  head(10)
bottom10
ggplot(data=bottom10,
       aes(x=reorder(job,-mean_income),
           y=mean_income)) +
  geom_col() +
  coord_flip()
#put minus (-) sign in reorder() for bottom 10

#####7) job distribution by gender
job_male <- welfare %>% 
  filter(!is.na(job) & sex == "male") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_male
job_female <- welfare %>% 
  filter(!is.na(job) & sex == "female") %>% 
  group_by(job) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)
job_female

ggplot(data=job_male, 
       aes(x=reorder(job,n), y=n)) +
  geom_col() +
  coord_flip()
ggplot(data=job_female,
       aes(x=reorder(job,n), y=n)) +
  geom_col() +
  coord_flip()

#####8) divorce rate by religion
##1. pre-process religion variable
class(welfare$religion)
table(welfare$religion)
welfare$religion <- ifelse(welfare$religion == 1,
                           "yes","no")
table(welfare$religion)
qplot(welfare$religion)

##2. pre-process marriage variable
class(welfare$marriage)
table(welfare$marriage)
welfare$marriage_group <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$marriage_group)
qplot(welfare$marriage_group)

##3. divorce rate based on religion
religion_marriage <- welfare %>% 
  filter(!is.na(marriage_group)) %>% 
  group_by(religion, marriage_group) %>% 
  summarise(n=n()) %>% 
  mutate(per= round(n/sum(n)*100,1))
religion_marriage
#mutate() calculated percentage 
divorce <- religion_marriage %>% 
  filter(marriage_group=="divorce") %>% 
  select(religion, per)
divorce
ggplot(data=divorce,
       aes(x=religion, y=per)) +
  geom_col()

#####9) marriage by age group and religion
##1. marriage by age group
ageg_marriage <- welfare %>% 
  filter(!is.na(marriage_group)) %>% 
  group_by(ageg, marriage_group) %>% 
  summarise(n=n()) %>% 
  mutate(per = round(n/sum(n)*100,1))
ageg_marriage

##2. divorce rate by age group
ageg_divorce <- ageg_marriage %>% 
  filter(marriage_group == "divorce") %>% 
  select(ageg,per)
ageg_divorce
ggplot(data=ageg_divorce,
       aes(x=ageg, y=per))+
  geom_col()+
  scale_x_discrete(limits = c("young","middle","old"))

##3. marriage by age group and religion
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(marriage_group)) %>% 
  count(ageg, religion, marriage_group) %>% 
  group_by(ageg, religion) %>% 
  mutate(per = round(n/sum(n)*100,1))
ageg_religion_marriage

##4. divorce rate by age group and religion
ageg_religion_divorce <- ageg_religion_marriage %>% 
  filter(marriage_group=="divorce") %>% 
  select(ageg, religion, per)
ageg_religion_divorce
ggplot(data=ageg_religion_divorce,
       aes(x=ageg, y=per, fill=religion)) +
  geom_col(position="dodge") +
  scale_x_discrete(limits=c("young","middle","old"))

#####10) age group by region
##1. pre-process code_region variable
class(welfare$code_region)
table(welfare$code_region)

list_region <- data.frame(code_region = c(1:7),
                          region=c("서울",
                                   "수도권(인천/경기)",
                                   "부산/경남/울산",
                                   "대구/경북",
                                   "대전/충남",
                                   "강원/충북",
                                   "광주/전남/전북/제주도"))
list_region

welfare <- left_join(welfare, list_region, by=c("code_region"="code_region"))
welfare %>% 
  select(code_region, region) %>% 
  head

##2. age group by region
region_ageg <- welfare %>% 
  group_by(region, ageg) %>% 
  summarise(n=n()) %>% 
  mutate(per = round(n/sum(n)*100,1))
head(region_ageg)

ggplot(data = region_ageg,
       aes(x=region, y=per, fill=ageg)) +
  geom_col() +
  coord_flip()

##3. order by desc number of "old" age group
list_order_old <- region_ageg %>% 
  filter(ageg == "old") %>% 
  arrange(per)
list_order_old

#make region order variable according to the order of table above
region_order <- list_order_old$region
region_order

ggplot(data=region_ageg,
       aes(x=region, y=per, fill=ageg)) +
  geom_col() +
  scale_x_discrete(limits = region_order) +
  coord_flip()

##4. arrange colors in order
head(region_ageg)
class(region_ageg$ageg)

region_ageg$ageg <- factor(region_ageg$ageg,
                           level = c("old","middle","young"))
class(region_ageg$ageg)
levels(region_ageg$ageg)
#change ageg var to factor type
#level parameter sets the order

ggplot(data=region_ageg,
       aes(x=region, y=per, fill=ageg))+
  geom_col()+
  scale_x_discrete(limits = region_order)+
  coord_flip()
