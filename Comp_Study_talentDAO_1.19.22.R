rm(list=ls())

#loads required packages for this analysis
required.packages <- c("tidyverse","apaTables","psych","readxl","skimr", "stringr", "car", "multcomp")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(psych)
library(skimr)
library(stringr)
library(car)
library(multcomp)
recode <- dplyr::recode
select <- dplyr::select


#Set your working directory for R to know where the data file lives.
setwd("~/Dropbox/TalentDAO/")

##Read data into a Dataframe
data <-read.csv("Compensation Survey (020) (Responses) - Compensation Survey Response_Raw.csv", header = TRUE, na.strings = "")

##### Review data and variable names
str(data)
glimpse(data)

##rename to shorter variables
data <- data %>% 
  rename(dao_type = What.DAO.types.have.you.joined.in.any.role..e.g...guest..contributor..member..,
         primary_gov_tool = What.is.the.PRIMARY.governance..i.e...decision.making..tool.used.by.the.DAO.you.contribute.to.the.most.,
         dao_size = How.many.people..in.any.role..are.in.the.DAO.you.contribute.to.the.most...Estimates.are.accepted..,
         contrib_hours = How.many.hours.per.week..on.average..do.you.work.for.the.DAO.you.contribute.to.the.most.,
         dao_count_contrib = How.many.DAOs.do.you.contribute.to.in.an.average.week.,
         paysat1 = Given.this.DAO.s.payment.method..how.would.you.feel.about.the.overall.level.of.pay.,
         paysat2 = How.would.you.feel.about.this.DAO.s.pay.structure.,
         paysat3 = How.would.you.feel.about.how.often.pay.is.distributed.,
         paysat4_stay = To.what.extent.do.you.feel.that.the.pay.structure.is.a.reason.to.continue.working.for.this.DAO.,
         orgcommit1 = Based.on.this.DAO.s.pay.structure..how.committed.would.you.be.to.the.DAO.,
         orgcommit2 = Based.on.this.DAO.s.pay.structure..to.what.extent.would.you.care.about.the.DAO.,
         orgcommit3 = Based.on.this.DAO.s.pay.structure..how.dedicated.would.you.be.to.the.DAO.,
         distribjust1 = To.what.extent.does.the.pay.structure.reflect.the.effort.you.have.put.into.your.work.,
         distribjust2 = To.what.extent.is.the.pay.structure.appropriate.for.the.work.you.completed.,
         distribjust3 = To.what.extent.does.the.pay.structure.reflect.the.work.you.have.contributed.to.the.DAO.,
         distribjust4 = To.what.extent.is.the.pay.structure.justified..given.you.delivered.it.on.time.and.met.all.requirements.,
         extrinsic1 = It.is.important.to.me.to.achieve.financial.success.in.my.career.,
         extrinsic2 = It.is.important.for.me.to.be.seen.by.others.as.a.success.in.my.career.,
         extrinsic3 = I.want.to.be.seen.as.a.powerful.individual.in.the.DAO.I.primarily.contribute.to.,
         extrinsic4 = I.want.a.career.that.gives.me.high.social.status.,
         intrinsic1 = It.is.important.for.me.to.continue.to.learn.and.grow.over.the.course.of.my.career.,
         intrinsic2 = It.is.important.that.my.career.offers.me.opportunities.for.interesting.work.,
         intrinsic3 = I.want.to.gain.experience.through.a.wide.variety.of.jobs.or.work.assignments.,
         intrinsic4 = It.is.important.for.me.to.develop.my.technical.functional.skills.over.the.course.of.my.career.,
         intrinsic5 = I.want.to.have.a.positive.impact.on.other.people.or.social.problems.through.my.work.,
         pay_cadence = Given.that.I.complete.a.project.on.time.and.meet.the.requirements..I.would.prefer.to.be.paid.________.,
         gender = What.gender.do.you.identify.as.,
         age = What.is.your.age.,
         ethnicity = Please.specify.your.ethnicity.,
         income = What.is.your.annual.household.income.,
         location = Where.is.your.home.located.)

glimpse(data)

data %>% count(dao_type)
data %>% count(dao_size)
data %>% count(contrib_hours)
data %>% count(dao_count_contrib)
data %>% count(paysat1)
data %>% count(paysat2)
data %>% count(paysat3)
data %>% count(paysat4_stay)
data %>% count(orgcommit1)
data %>% count(orgcommit2)
data %>% count(orgcommit3)


###### Recode to research question groups


## here I was trying to count categories within a multiple response variable but couldn't get it to work
# dao_count <- str_split(data$dao_type, ",")
# sum(str_count(dao_count, "NFT"))
# lev <- unique(unlist(dao_count))
# resp_dummy <- lapply(dao_count, function(x) table(factor(x, levels=lev)))
# data <- with(data, data.frame(do.call(rbind, resp_dummy), 1:noCol))
# dao_count <- as.data.frame(dao_count)
# dao_count %>%  count(across(sum(str_count(dao_count, "NFT"))))


# create categories for dao_size,  # of daos contributed to, and contributed hours
data$dao_size_cat <-ifelse(data$dao_size == "0-100" | data$dao_size == '100-500', "Small", 
                           ifelse(data$dao_size == "500-1,000" | data$dao_size == "1000-5,000" | data$dao_size == "5,000+", "Large", NA))

data$dao_count_contrib_cat <- ifelse(data$dao_count_contrib == 1, "1", 
                           ifelse(data$dao_count_contrib == 2, "2", ifelse(data$dao_count_contrib == 3 | data$dao_count_contrib == 4, "3+", NA)))

data$dao_count_contrib_cat <- factor(data$dao_count_contrib_cat) #convert to factor for data viz purposes
str(data$dao_count_contrib_cat)

table(data$contrib_hours)
data$contrib_hours_cat <- ifelse(data$contrib_hours == "<1-5 hours/week" | data$contrib_hours == "6-10 hours/week" | data$contrib_hours == "11-15 hours/week" | 
                                   data$contrib_hours =="16-20 hours/week", "Part time",
                                 ifelse(data$contrib_hours == "20-30 hours/week" | data$contrib_hours == "30+hours/week", "Full time", NA))


## clean open-ended responses for primary governance tool and demographics 
data %>% count(primary_gov_tool)
data <- data %>% mutate(primary_gov_tool_cat = recode(primary_gov_tool,  "At the moment discord." = "Manual Voting",
                                              "Centralised Team" = "Manual Voting",
                                              "Sociocracy / we vote with our hands together in real time. " = "Manual Voting",
                                              "Sociocracy-style consent decision-making" = "Manual Voting",
                                              "quorum voting online" = "Manual Voting",
                                              "that i contribute to, none here yet really " = "Manual Voting",
                                              "Early stages at Coordinape- No governance yet" = "Manual Voting",
                                              "MakerDao builds our own voting systems :)" = "Homegrown",
                                              "MKR Governance" = "Homegrown",
                                              "Native decentralized governance to MakerDAO (MIPs)" = "Homegrown",
                                              "Qi DAO" = "Other",
                                              "https://1hive.org/" = "Other",
                                              "i am new and just understanding the world of blockchain" = "Don't know",
                                              "I've only been contributing to DAOs for two weeks and am not sure which of these governance tools is being used the most." = "Don't know",
                                              "I don't contribute to any DAOs." = NA_character_))
table(data$primary_gov_tool_cat)


data %>% count(gender)
data <- data %>% mutate(gender_cat = recode(gender,  "Autigender [neurogender]" = "Other",
                                                      "gender non conforming" = "Other",
                                                      "nonbinary" = "Other",
                                                      "Nonbinary - Agender" = "Other",
                                                      "The options you have provided are sex but you have asked for gender.  I am confused." = "Other",
                                                      "Two spirit" = "Other",
                                                      "Prefer not to say" = NA_character_))
table(data$gender_cat)

data %>% count(ethnicity)
data <- data %>% mutate(ethnicity_cat = recode(ethnicity,  "Arab" = "Other",
                                            "Black AND Indigenous and Northwestern European" = "Black or African-American",
                                            "Visible minority " = "Other",
                                            "Greek American" = "White or Caucasian",
                                            "Jewish" = "White or Caucasian",
                                            "Scandinavian" = "White or Caucasian",
                                            "Mauritian" = "Black or African-American",
                                            "Middle Eastern" = "Other",
                                            "PLEASE MAKE THIS TO WHERE YOU CAN SELECT MULTIPLE. making somone pick...none of my identities dominate over the other inside me, that only happens in the \"real\" world" = "Two or More",
                                            "Prefer not to say" = NA_character_))

table(data$ethnicity_cat)

##create binary ethnicity for broader group comparisons
data <- data %>% mutate(minority = recode(ethnicity_cat, "White or Caucasian" = "Majority",
                                          .default = "Minority"))
table(data$minority)


data %>% count(location)

## create binary income variable for simpler group comparisons
data %>% count(income)
data <- data %>% mutate(income_cat = dplyr::recode(income,"Less than $25,000" = "< $100,000",
                                                   "$25,000 - $50,000" = "< $100,000",
                                                   "$50,000 - $100,000" = "< $100,000",
                                                   "$100,000 - $200,000" = "$100,000+",
                                                   "More than $200,000" = "$100,000+",
                                                   "Prefer not to say" = NA_character_))
data %>% count(income_cat)


#recode likert scale items to numeric
data$paysat1 <-ifelse(data$paysat1 == "Very Dissatisfied", 1, ifelse(data$paysat1 == "Dissatisfied", 2, 
                                                                     ifelse(data$paysat1 == "Neither Satisfied nor dissatisfied", 3,
                                                                            ifelse(data$paysat1 == "Satisfied", 4,
                                                                                   ifelse(data$paysat1 == "Very Satisfied", 5, NA)))))
data$paysat2 <-ifelse(data$paysat2 == "Very Dissatisfied", 1, ifelse(data$paysat2 == "Dissatisfied", 2, 
                                                                     ifelse(data$paysat2 == "Neither Satisfied nor dissatisfied", 3,
                                                                            ifelse(data$paysat2 == "Satisfied", 4,
                                                                                   ifelse(data$paysat2 == "Very Satisfied", 5, NA)))))
data$paysat3 <-ifelse(data$paysat3 == "Very Dissatisfied", 1, ifelse(data$paysat3 == "Dissatisfied", 2, 
                                                                     ifelse(data$paysat3 == "Neither Satisfied nor dissatisfied", 3,
                                                                            ifelse(data$paysat3 == "Satisfied", 4,
                                                                                   ifelse(data$paysat3 == "Very Satisfied", 5, NA)))))
data$orgcommit1 <-ifelse(data$orgcommit1 == "Not at all", 1, ifelse(data$orgcommit1 == "Slightly", 2, 
                                                                     ifelse(data$orgcommit1 == "Moderately", 3,
                                                                            ifelse(data$orgcommit1 == "Quite a bit", 4,
                                                                                   ifelse(data$orgcommit1 == "Extremely", 5, NA)))))
data$orgcommit2 <-ifelse(data$orgcommit2 == "Not at all", 1, ifelse(data$orgcommit2 == "Slightly", 2, 
                                                                    ifelse(data$orgcommit2 == "Moderately", 3,
                                                                           ifelse(data$orgcommit2 == "Quite a bit", 4,
                                                                                  ifelse(data$orgcommit2 == "Extremely", 5, NA)))))
data$orgcommit3 <-ifelse(data$orgcommit3 == "Not at all", 1, ifelse(data$orgcommit3 == "Slightly", 2, 
                                                                    ifelse(data$orgcommit3 == "Moderately", 3,
                                                                           ifelse(data$orgcommit3 == "Quite a bit", 4,
                                                                                  ifelse(data$orgcommit3 == "Extremely", 5, NA)))))
#save coded data
data_coded <- data

## Review missingness in likert items
anyNA(data_coded[,c("paysat1","paysat2", "paysat3","orgcommit1", "orgcommit2", "orgcommit3", "distribjust1", "distribjust2", "distribjust3", "distribjust4",
                    "extrinsic1", "extrinsic2","extrinsic3","extrinsic4","intrinsic1","intrinsic2",
                    "intrinsic3", "intrinsic4", "intrinsic5")])

## Counts missingness in the paysat, org commit, and distrib justice variables and flags any cases with only 1 or 2 missing responses depending on scale
data_coded <- data_coded %>% mutate(paysat_missing = rowSums(is.na(select(., one_of(c('paysat1', 'paysat2', 'paysat3'))))),
                                    orgcommit_missing = rowSums(is.na(select(., one_of(c('orgcommit1', 'orgcommit2', 'orgcommit3'))))),
                                    distribjust_missing = rowSums(is.na(select(., one_of(c('distribjust1', 'distribjust2', 'distribjust3', 'distribjust4'))))),
                                    extrinsic_missing = rowSums(is.na(select(., one_of(c("extrinsic1", "extrinsic2","extrinsic3","extrinsic4"))))),
                                    intrinsic_missing = rowSums(is.na(select(., one_of(c("intrinsic1","intrinsic2","intrinsic3", "intrinsic4", "intrinsic5")))))) %>% 
  filter(paysat_missing < 2 & orgcommit_missing < 2 & distribjust_missing < 3 & extrinsic_missing < 3 & intrinsic_missing < 4)

# Impute missing values with item average
data_coded <- data_coded %>% mutate_at(vars(paysat1, paysat2, paysat3, 
                              orgcommit1, orgcommit2, orgcommit3, 
                              distribjust1, distribjust2, distribjust3, distribjust4,
                              extrinsic1, extrinsic2,extrinsic3,extrinsic4,
                              intrinsic1,intrinsic2,intrinsic3, intrinsic4,intrinsic5),
            ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

#check for any missingness
anyNA(data_coded[,c("paysat1","paysat2", "paysat3","orgcommit1", "orgcommit2", "orgcommit3", "distribjust1", "distribjust2", "distribjust3", "distribjust4",
                    "extrinsic1", "extrinsic2","extrinsic3","extrinsic4","intrinsic1","intrinsic2",
                    "intrinsic3", "intrinsic4", "intrinsic5")])

### Save cleaned data
data_clean <- data_coded

write.csv(data_clean,"Comp Study Data Clean.csv", row.names = FALSE)


##########   tart observed group mean comparisons

### Compute sum and mean scale scores
data_clean <-read.csv("Comp Study Data Clean.csv", header = TRUE, na.strings = "")

data_clean <- data_clean %>% mutate(paysat_total = paysat1 + paysat2 + paysat3,
                        orgcommit_total = orgcommit1 + orgcommit2 + orgcommit3,
                        distribjust_total = distribjust1 + distribjust2 + distribjust3 + distribjust4,
                        extrinsic_total = extrinsic1 + extrinsic2 + extrinsic3 + extrinsic4,
                        intrinsic_total = intrinsic1 + intrinsic2 + intrinsic3 + intrinsic4 + intrinsic5)

data_clean <- data_clean %>% rowwise %>% mutate(paysat_mean = mean(c(paysat1, paysat2, paysat3)),
                        orgcommit_mean = mean(c(orgcommit1, orgcommit2, orgcommit3)),
                        distribjust_mean = mean(c(distribjust1, distribjust2, distribjust3, distribjust4)),
                        extrinsic_mean = mean(c(extrinsic1, extrinsic2,extrinsic3,extrinsic4)),
                        intrinsic_mean = mean(c(intrinsic1,intrinsic2,intrinsic3, intrinsic4,intrinsic5)))


#Research questions:

#Omnibus differences in pay sat, org commit, and distributive justice by governance type condition
table(data_clean$Condition)
omnibus_mean <- data_clean %>% group_by(Condition) %>% summarise(across(c(distribjust_mean, paysat_mean, orgcommit_mean), mean))
omnibus_mean_long <- gather(omnibus_mean, value_type, value, distribjust_mean:orgcommit_mean)
omnibus_mean_long$value_type <- factor(omnibus_mean_long$value_type, levels = c("distribjust_mean", "paysat_mean", "orgcommit_mean"))

omnibus_mean_long %>% 
  ggplot(aes(Condition, value, fill = value_type)) + 
  geom_col(position = position_dodge(), width = .8) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5)) +
  geom_text(aes(label=round(value, 1)), position=position_dodge(width = .8),vjust=-0.25) +
  scale_fill_grey(start = 0.75, end = 0.25,
                  name=NULL,
                  breaks=c("distribjust_mean","paysat_mean","orgcommit_mean"),
                    labels=c("Distributive justice mean",
                    "Pay satisfaction mean",
                    "Org. commitment mean")) +
  labs(x=NULL,y="Overall Means") +
  guides(fill=guide_legend(title=NULL)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_line(color = 'lightgray', size = 0.25))


##Calculate ANCOVA for omnibus mean differences test with control variables
data_clean %>% select(Condition, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition) %>% skim()
boxplot(distribjust_mean ~ Condition, data = data_clean)
dao_size_cat_test <- chisq.test(table(data_clean$Condition, data_clean$dao_size_cat)) #test covariate and treatment are independent
dao_size_cat_test

skim(data_clean$extrinsic_mean) #3.5 = 50th percentile
data_clean$extrinsic_cat <- ifelse(data_clean$extrinsic_mean <= 3.50, "Lower Extrinsic", "Higher Extrinsic")
motiv_cat_test <- chisq.test(table(data_clean$Condition, data_clean$extrinsic_cat)) #test covariate and treatment are independent
motiv_cat_test

income_cat_test <- chisq.test(table(data_clean$Condition, data_clean$income_cat)) #test covariate and treatment are independent
income_cat_test


distrib_model <- aov(distribjust_mean ~ extrinsic_cat + dao_size_cat + income_cat + minority + Condition, data = data_clean) 
summary(distrib_model, type="III")
postHocs_distribjust <- TukeyHSD(distrib_model, "Condition")
postHocs_distribjust

paysat_model <- aov(paysat_mean ~ extrinsic_cat + dao_size_cat + income_cat + minority + Condition, data = data_clean) 
summary(paysat_model, type="III")

orgcommit_model <- aov(orgcommit_mean ~ extrinsic_cat + dao_size_cat + income_cat + minority + Condition, data = data_clean) 
summary(orgcommit_model, type="III")
postHocs_orgcommit <- TukeyHSD(orgcommit_model, "Condition")
postHocs_orgcommit

#test for homogeneity of variance using Levene's Test?



###################### Within Condition group comparisons #################################


##### R2 - Difference from larger (500+) and smaller DAOs (<500)
data_clean %>% select(Condition, dao_size_cat, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition, dao_size_cat) %>% skim()

table(data_clean$Condition, data_clean$dao_size_cat)

largevsmall_mean <- data_clean %>% group_by(Condition, dao_size_cat) %>% summarise(across(c(distribjust_mean, paysat_mean, orgcommit_mean), mean))
largevsmall_mean_long <-  largevsmall_mean %>% filter(!is.na(dao_size_cat)) %>% gather(., value_type, value, distribjust_mean:orgcommit_mean)
largevsmall_mean_long$value_type <- factor(largevsmall_mean_long$value_type, levels = c("distribjust_mean", "paysat_mean", "orgcommit_mean"))

#by each condition
largevsmall_mean_long %>% 
ggplot(aes(dao_size_cat, value, fill = value_type)) + 
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(~Condition) +
  ylim(0,5.0) +
  geom_text(aes(label=round(value, 1)), position=position_dodge(width=0.5),vjust=-0.25) +
  scale_fill_grey(start = 0.75, end = 0.25,
                  breaks = c("distribjust_mean", "paysat_mean", "orgcommit_mean"),
                  labels = c("Distributive Justice Mean", 
                             "Pay Satisfaction Mean", 
                             "Organizational Committment Mean")) +
  labs(x=NULL, y="Large v Small DAO Mean Comparisons by Condition") +
  guides(fill=guide_legend(title=NULL)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_line(color = 'lightgray', size = 0.25))


bountyonly <- data_clean %>% filter(Condition == 'Bounty')
t.test(orgcommit_mean ~ dao_size_cat, data = bountyonly)

hourlyyonly <- data_clean %>% filter(Condition == 'Hourly')
t.test(distribjust_mean ~ dao_size_cat, data = hourlyyonly)


###### R3 - Differences between higher contributors versus lower contributors
data_clean %>% select(Condition, dao_count_contrib_cat, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition, dao_count_contrib_cat) %>% skim()

table(data_clean$Condition, data_clean$dao_count_contrib_cat)

morevfewercontrib_mean <- data_clean %>% group_by(Condition, dao_count_contrib_cat) %>% summarise(across(c(distribjust_mean, paysat_mean, orgcommit_mean), mean))
morevfewercontrib_mean_long <-  morevfewercontrib_mean %>% filter(!is.na(dao_count_contrib_cat)) %>% gather(., value_type, value, distribjust_mean:orgcommit_mean)
morevfewercontrib_mean_long$value_type <- factor(morevfewercontrib_mean_long$value_type, levels = c("distribjust_mean", "paysat_mean", "orgcommit_mean"))

#by each condition
morevfewercontrib_mean_long %>% 
  ggplot(aes(dao_count_contrib_cat, value, fill = value_type)) + 
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(~Condition) +
  ylim(0,5.0) +
  geom_text(aes(label=round(value, 1)), position=position_dodge(width=0.5),vjust=-0.25) +
  scale_fill_grey(start = 0.75, end = 0.25,
                  breaks = c("distribjust_mean", "paysat_mean", "orgcommit_mean"),
                  labels = c("Distributive Justice Mean", 
                             "Pay Satisfaction Mean", 
                             "Organizational Committment Mean")) +
  labs(x=NULL, y="# of DAO Contribution Group Mean Comparisons by Condition") +
  guides(fill=guide_legend(title=NULL)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_line(color = 'lightgray', size = 0.25))

#Compute ANOVA For 3 group differences on PaySat in Bounty Condition
dao_contrib_cat_paysat_model <- aov(paysat_mean ~ dao_count_contrib_cat, data = bountyonly) 
summary(dao_contrib_cat_paysat_model, type="III")

salaryonly <- data_clean %>% filter(Condition == 'Salary')
dao_contrib_cat_orgcommit_model_salary <- aov(orgcommit_mean ~ dao_count_contrib_cat, data = salaryonly)
summary(dao_contrib_cat_orgcommit_model_salary, type="III")



###### R4 - Differences between FT contributors versus PT contributors
data_clean %>% select(Condition, contrib_hours_cat, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition, contrib_hours_cat) %>% skim()

table(data_clean$Condition, data_clean$contrib_hours_cat)

fullvpart_mean <- data_clean %>% group_by(Condition, contrib_hours_cat) %>% summarise(across(c(distribjust_mean, paysat_mean, orgcommit_mean), mean))
fullvpart_mean_long <-  fullvpart_mean %>% filter(!is.na(contrib_hours_cat)) %>% gather(., value_type, value, distribjust_mean:orgcommit_mean)
fullvpart_mean_long$value_type <- factor(fullvpart_mean_long$value_type, levels = c("distribjust_mean", "paysat_mean", "orgcommit_mean"))

#by each condition
fullvpart_mean_long %>% 
  ggplot(aes(contrib_hours_cat, value, fill = value_type)) + 
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(~Condition) +
  ylim(0,5.0) +
  geom_text(aes(label=round(value, 1)), position=position_dodge(width=0.5),vjust=-0.25) +
  scale_fill_grey(start = 0.75, end = 0.25,
                  breaks = c("distribjust_mean", "paysat_mean", "orgcommit_mean"),
                  labels = c("Distributive Justice Mean", 
                             "Pay Satisfaction Mean", 
                             "Organizational Committment Mean")) +
  labs(x=NULL, y="Full-time v Part-time DAO Contributor Group Means by Condition") +
  guides(fill=guide_legend(title=NULL)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_line(color = 'lightgray', size = 0.25))

#Differences between contributors more familiar with compensation structures and tools than less familiar  (anyone identifying Coordinape in primary_gov_tool?) -- don't have the sample size for just a 'primary_gov_tool = Coordinape' test

#### R5 - Difference from more and less extrinsically motivated people
skim(data_clean$extrinsic_mean) #3.5 = 50th percentile
data_clean$extrinsic_cat <- ifelse(data_clean$extrinsic_mean <= 3.48, "Lower Extrinsic", "Higher Extrinsic")
table(data_clean$extrinsic_cat)

data_clean %>% select(Condition, extrinsic_cat, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition, extrinsic_cat) %>% skim()

table(data_clean$Condition, data_clean$extrinsic_cat)

highex_v_lowex_mean <- data_clean %>% group_by(Condition, extrinsic_cat) %>% summarise(across(c(distribjust_mean, paysat_mean, orgcommit_mean), mean))
highex_v_lowex_mean_long <-  highex_v_lowex_mean %>% filter(!is.na(extrinsic_cat)) %>% gather(., value_type, value, distribjust_mean:orgcommit_mean)
highex_v_lowex_mean_long$value_type <- factor(highex_v_lowex_mean_long$value_type, levels = c("distribjust_mean", "paysat_mean", "orgcommit_mean"))

#by each outcome
highex_v_lowex_mean_long %>% 
  ggplot(aes(extrinsic_cat, value, fill = value_type)) + 
  geom_col(position = position_dodge(), width = 0.5) +
  facet_wrap(~Condition) +
  ylim(0,5.0) +
  geom_text(aes(label=round(value, 1)), position=position_dodge(width=0.5),vjust=-0.25) +
  scale_fill_grey(start = 0.75, end = 0.25,
                  breaks = c("distribjust_mean", "paysat_mean", "orgcommit_mean"),
                  labels = c("Distributive Justice Mean", 
                             "Pay Satisfaction Mean", 
                             "Org. Commitment Mean")) +
  labs(x=NULL, y="High v Low Extrinsic Motivation Mean Comparisons by Condition") +
  guides(fill=guide_legend(title=NULL)) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10),
        legend.key.size = unit(10, "pt"),
        panel.grid.major.y = element_line(color = 'lightgray', size = 0.25))

##Demographic differences
#gender
data_clean %>% select(Condition, gender_cat, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition, gender_cat) %>% skim()

table(data_clean$Condition, data_clean$gender_cat)

#race/ethnicity
data_clean %>% select(Condition, ethnicity_cat, paysat_total, orgcommit_total, distribjust_total) %>% group_by(Condition, ethnicity_cat) %>% skim()

data_clean %>% select(Condition, ethnicity_cat, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition, ethnicity_cat) %>% skim()

table(data_clean$Condition, data_clean$ethnicity_cat)

#race/ethnicity minority v. majority
data_clean %>% select(Condition, minority, paysat_total, orgcommit_total, distribjust_total) %>% group_by(Condition, minority) %>% skim()

data_clean %>% select(Condition, minority, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition, minority) %>% skim()

table(data_clean$Condition, data_clean$minority)

#age
data_clean %>% select(Condition, age_cat, paysat_total, orgcommit_total, distribjust_total) %>% group_by(Condition, age_cat) %>% skim()

data_clean %>% select(Condition, age_cat, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition, age_cat) %>% skim()

table(data_clean$Condition, data_clean$age_cat)


#### Testing hypotheses and path models to show mediating effect of pay satisfaction between distributional justice and org commitment

## Overal path model without group effects
## "With a categorical moderator, the moderated mediation effect concerns the difference in the indirect effect between groups." (Ryu et al.: https://www.frontiersin.org/articles/10.3389/fpsyg.2017.00747/full)
## "In SEM, the mediation effect can be specified as an indirect effect (Alwin and Hauser, 1975; Bollen, 1987) such as “the indirect effect of an independent variable (X) on a dependent variable (Y) via a mediator (M)” in which X affects M, which in turn affects Y."

data_clean %>% select(Condition, paysat_mean, orgcommit_mean, distribjust_mean) %>% group_by(Condition) %>% lowerCor()

