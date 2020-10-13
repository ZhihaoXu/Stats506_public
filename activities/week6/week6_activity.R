library(tidyverse)
demo = read_delim("nhanes_demo.csv", delim = ",")
oheden = read_delim("nhanes_ohxden.csv", delim = ",")
## Part 1
dfp1 = demo %>% 
  left_join(oheden, by='SEQN') %>%
  select(id = SEQN, gender = RIAGENDR,
         age = RIDAGEYR, exam_status = RIDSTATR,
         ohe_status = OHDDESTS, college = DMDEDUC2) %>%
  mutate(
    under_20 = ifelse(age<20,'age<20','age>20'),
    ohx = ifelse((exam_status==2)&(ohe_status==1),"complete","missing"),
    ohx = ifelse(is.na(ohx),"missing",ohx),
    college = recode (college,
      `4` = "some college",
      `5` = "college graduate",
      .default = "No college/<20"
    ),
    college = ifelse(is.na(college),"No college/<20",college),
  ) %>%
  filter(exam_status==2)

## Part 2
dfp2_under20 = dfp1 %>%
  group_by(ohe_status, under_20) %>%
  summarise(under_20_n = n()) %>%
  spread(key = under_20, value = under_20_n) %>%
  drop_na()

dfp2_gender = dfp1 %>%
  group_by(ohe_status, gender) %>%
  summarise(gender_n = n())%>%
  spread(key = gender, value = gender_n) %>%
  drop_na()

dfp2_college = dfp1 %>%
  group_by(ohe_status, college) %>%
  summarise(college_n = n())%>%
  spread(key = college, value = college_n) %>%
  drop_na()

dfp2_under20 %>%
  left_join(dfp2_gender, by = 'ohe_status') %>%
  left_join(dfp2_college, by = "ohe_status")


