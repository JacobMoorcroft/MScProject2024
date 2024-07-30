#set reproducible seed for analyses
set.seed(123)

#clean environment
rm(list=ls())

#necessary packages: should install be required, please remove annotations and run these two lines :)
#libraries<-c("here", "Matrix", "lme4", "lmerTest", "ggplot2", "GGally", "tidyverse", "finalfit", "car", "performance")
#install.packages(libraries, repos="http://cran.rstudio.com")

library(here)          #for loading in raw data
library(Matrix)        #for LMEM estimation*
library(lme4)          #*
library(lmerTest)      #*
library(ggplot2)       #for LMEM visualisations**
library(GGally)        #**
library(tidyverse)     #for all visualisations
library(finalfit)      #for missingness visualisation
library(car)           #for multicollinearity checks
library(performance)   #for model evaluation


############################################################ LOADING IN RAW DATA ##################################################################################

#demographic data, cognition-related beliefs (CRBs), & inclusions filter
home_survey<-read.csv(here("raw_data","survey_home.csv"))
lab_survey<-read.csv(here("raw_data","survey_lab.csv"))
filter<-read.csv(here("raw_data","tracker_inclusions.csv"))

#training and near transfer assessments
simple_data<-read.csv(here("raw_data","assessment_training_simple.csv"))
choice_data<-read.csv(here("raw_data","assessment_training_choice.csv"))
switch_data<-read.csv(here("raw_data","assessment_training_switch.csv"))
dual_data<-read.csv(here("raw_data","assessment_training_dual.csv"))

#far transfer measures
wm_updating<-read.csv(here("raw_data","assessment_wm_updating.csv"))
wm_binding<-read.csv(here("raw_data","assessment_wm_binding.csv"))
wm_reproduction<-read.csv(here("raw_data","assessment_wm_reproduction.csv"))
reas_rapm<-read.csv(here("raw_data","assessment_reas_rapm.csv"))
reas_lettersets<-read.csv(here("raw_data","assessment_reas_lettersets.csv"))
reas_paperfolding<-read.csv(here("raw_data","assessment_reas_paperfolding.csv"))





############################################################## AGGREGATING DATA ###################################################################################

#extracting and aggregating data on demographics and CRBs

sex_age_grit<-home_survey %>%
  select(code,group,site,demo.sex,demo.age.years,demo.age.group,demo.gender,grit)

gse_tis<-lab_survey %>%
  select(code,sessionId,gse,tis) %>%
  filter(sessionId==1) #only session 1 data used for mindset and self-efficacy; as Grit was only measured pre-study, this ensured all CRBs scores were pre-training

crb<-inner_join(sex_age_grit,
                gse_tis %>% select(code,gse,tis),by="code")

#extracting processing speed measures

process<-function(data, suffix) {data %>%
    select(code,assessment,material,speed) %>%
    spread(key=material, value=speed) %>%
    rename(sessionId=assessment,
           !!paste0(suffix, "_draw_rt"):=drawings,
           !!paste0(suffix, "_numb_rt"):=numbers,
           !!paste0(suffix, "_shap_rt"):=shapes)}

simple_rt<-process(simple_data, "simp")
choice_rt<-process(choice_data, "choi")
switch_data<-switch_data %>% rename(speed=speed.switch)
switch_rt<-process(switch_data, "swit")
dual_data$speed<-pmax(dual_data$speed.response1,dual_data$speed.response2) #slower RT preferenced, as this represented RT after classifying stimulus on both dimensions 
dual_rt<-process(dual_data, "dual")

#extracting reasoning and working memory measures

f_t_dfs<-list(reas_ls=reas_lettersets,reas_pf=reas_paperfolding,reas_ra=reas_rapm,
              work_bi=wm_binding,work_re=wm_reproduction,work_up=wm_updating)

f_t_outcomes<-list(reas_ls="assessment.reas.lettersets.score",
                   reas_pf="assessment.reas.paperfolding.score",
                   reas_ra="assessment.reas.rapm.score",
                   work_bi="wm.binding.dprime",
                   work_re="wm.reproduction.error",
                   work_up="wm.updating.accuracy")

process2<-function(data, f_t_outcome) {data %>% #create function to process all far transfer data into a format which can be aggregated
    select(code,assessment,!!sym(f_t_outcome)) %>%
    rename(sessionId=assessment)}

processed_ft<-map2(f_t_dfs, f_t_outcomes, process2)
names(processed_ft)<-names(f_t_dfs)
reas_ls<-processed_ft$reas_ls
reas_pf<-processed_ft$reas_pf
reas_ra<-processed_ft$reas_ra
work_bi<-processed_ft$work_bi
work_re<-processed_ft$work_re
work_up<-processed_ft$work_up

#aggregation of all raw data
raw_data<-crb %>%
  full_join(simple_rt, by=c("code")) %>%
  full_join(choice_rt, by=c("code","sessionId")) %>%
  full_join(switch_rt, by=c("code","sessionId")) %>%
  full_join(dual_rt, by=c("code","sessionId")) %>%
  full_join(reas_ls, by=c("code","sessionId")) %>%
  full_join(reas_pf, by=c("code","sessionId")) %>%
  full_join(reas_ra, by=c("code","sessionId")) %>%
  full_join(work_bi, by=c("code","sessionId")) %>%
  full_join(work_up, by=c("code","sessionId")) %>%
  full_join(work_re, by=c("code","sessionId"))




################################################################ CLEANING DATA ####################################################################################

raw_data%>%
  count(sessionId) #there is an inconsistent amount of participants at each time-point (attrition): n=422 pretest, n=398 posttest, n=388 follow-up, n=4 unlabelled

#first filter - only participants who had completed at least six training sessions were retained
inclusions<-filter$code[filter$t6Complete == "Y"]

processed_data<-raw_data %>%
  filter(code %in% inclusions)

processed_data$sessionId=recode(processed_data$sessionId, "follow-up" = "followup") #corrects errors otherwise produced by '-' in 'follow-up'

#check for missing data
table(is.na(processed_data)) #229 missing observations
pps_missing_data<-unique(processed_data$code[!complete.cases(processed_data)]) #32 participants have at least one missing observation

#missing values map
filepath=here("figs/missing_value_plot.png")
png(filepath,width=800,height=570)
processed_data %>%
  missing_plot()+labs(x="Observation", y="Variable", title="Missing Values Map for Necessary Variables") + theme(plot.title=element_text(hjust=0.5, size=16, face="bold")) +
  scale_y_discrete(labels=c("Working Memory: reproduction error","Working Memory: updating accuracy","Working Memory: discrimination parameter","Reasoning: matrix reasoning",
                            "Reasoning: paperfolding","Reasoning: lettersets","Dual: shapes","Dual: numbers","Dual: drawings","Switching: shapes","Switching: numbers","Switching: drawings",
                            "Choice: shapes","Choice: numbers","Choice: drawings","Simple: shapes","Simple: numbers","Simple: drawings","Assessment Time-Point","Mindset","Self-Efficacy",
                            "Grit","Gender","Age-group","Age","Sex","Cognitive Site","Training Group","Participant Code"))
dev.off() #missingness is limited to a small amount of participants and does not appear to be systematic to any group, stimuli, material or otherwise included variable

processed_data<-processed_data[!processed_data$code %in% pps_missing_data, ] #removed all data for participants with missing values

#additional check for missing sessions for each participant
processed_data%>% count(sessionId) #1 participant missing session 3

#identifies and removes all data for this participant
sessions<-processed_data %>%
  group_by(code) %>%
  summarise(session_count=n_distinct(sessionId))
complete<-sessions %>%
  filter(session_count==3) %>%
  pull(code)
processed_data<-processed_data %>%
  filter(code %in% complete) #356 remaining participants

rm(list=setdiff(ls(), c("processed_data","crb"))) #cleans environment

#final demographic data
mean((processed_data$demo.age.years)) #48.62
range((processed_data$demo.age.years)) #18-85
sd((processed_data$demo.age.years)) #18.13

processed_data%>% count(group) #/3 (per session) = 95 simple, 89 choice, 88 switch, 84 dual
processed_data%>% count(demo.age.group) #/3 = 146 middle-aged, 80 older, 130 younger
processed_data%>% count(demo.gender) #/3 = 155 male, 200 female, 1 prefer not to say
processed_data%>% count(site) #/3 = 115 Hamburg, 116 Montreal, 125 Sheffield




############################################################### MODEL ESTIMATION ##################################################################################

#################################################################### SIMPLE #######################################################################################

simp_outcome<-processed_data %>%
  select(code,site,group,sessionId,demo.age.years,grit,tis,gse,simp_draw_rt,simp_numb_rt,simp_shap_rt) %>%
  pivot_longer(cols=ends_with("_rt"),
               names_to="material",
               values_to="_rt") %>%
  pivot_wider(names_from=sessionId, values_from=`_rt`)

simp_outcome$group <- factor(simp_outcome$group, ordered=FALSE)
simp_outcome$group <- relevel(simp_outcome$group, ref="Simple")

lmer_simp_b<-lmer(pretest~group+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=simp_outcome)
lmer_simp_e<-lmer(posttest~group*pretest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=simp_outcome)
lmer_simp_m<-lmer(followup~group*posttest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=simp_outcome)

#checks

#non-normality of residual distribution
qqnorm(resid(lmer_simp_b))
qqnorm(resid(lmer_simp_e))
qqnorm(resid(lmer_simp_m))

#minor heteroscedasticity of variance
plot(lmer_simp_b)
plot(lmer_simp_e)
plot(lmer_simp_m)

#outlier removal and transformations to rectify distributional violations

#remove >=200ms RTs and 3MAD+median< RTs
pps_exclude1<-simp_outcome%>%
  group_by(code) %>%
  filter(any(pretest < 200)) %>%
  filter(any(posttest < 200)) %>%
  filter(any(followup < 200)) %>%
  pull(code) %>% unique() #none

medianRT1<-median(simp_outcome$pretest)
MAD_3_RT1<-3*mad(simp_outcome$pretest)
medianRT2<-median(simp_outcome$posttest)
MAD_3_RT2<-3*mad(simp_outcome$posttest)
medianRT3<-median(simp_outcome$followup)
MAD_3_RT3<-3*mad(simp_outcome$followup)

pps_exclude2<-simp_outcome%>%
  group_by(code) %>%
  filter(pretest > (medianRT1+MAD_3_RT1)) %>%
  filter(posttest > (medianRT2+MAD_3_RT2)) %>%
  filter(followup > (medianRT3+MAD_3_RT3)) %>%
  pull(code) %>% unique() #4 extreme Ids

extremes<-union(pps_exclude1,pps_exclude2)
simp_outcome<-simp_outcome %>%
  filter(!simp_outcome$code %in% extremes) #removes all data for extreme pps (-12 observations)

#transformation
simp_outcome$pretest<-log(simp_outcome$pretest)
simp_outcome$posttest<-log(simp_outcome$posttest)
simp_outcome$followup<-log(simp_outcome$followup)

#re-run lines 192-194

#singularity issue for maintenance model
print(summary(lmer_simp_m),cor=F) #site variance negligible - removed
lmer_simp_m<-lmer(followup~group*posttest+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=simp_outcome)

#cooks distance check
cooksD<-cooks.distance(lmer_simp_b)
influential1<-simp_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_simp_e)
influential2<-simp_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_simp_m)
influential3<-simp_outcome$code[which(cooksD>1)]
influential<-union(influential1,influential2)
influential<-union(influential,influential3) #12 influential outlying observations; 7 extreme pps
simp_outcome<-simp_outcome[!simp_outcome$code %in% influential, ] #removes all data for extreme pps (-21 observations) (1035 in total)

#z-standardise
simp_outcome$demo.age.years<-scale(simp_outcome$demo.age.years)
simp_outcome$grit<-scale(simp_outcome$grit)
simp_outcome$tis<-scale(simp_outcome$tis)
simp_outcome$gse<-scale(simp_outcome$gse)
simp_outcome$pretest<-scale(simp_outcome$pretest)
simp_outcome$posttest<-scale(simp_outcome$posttest)
simp_outcome$followup<-scale(simp_outcome$followup)

#re-run lines 192-193, 243

#lack of perfect (5<GVIF) multicolinearity
multicolinearity_test<-vif(lmer_simp_b, type="predictor") #grit (2.2), gse (2)
multicolinearity_test<-vif(lmer_simp_e, type="predictor") #pretest (2.1), grit (2.2), gse (2)
multicolinearity_test<-vif(lmer_simp_m, type="predictor") #posttest (2.4), grit (2.2), gse (2.1)

#final models
lmer_simp_b<-lmer(pretest~group+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=simp_outcome)
lmer_simp_e<-lmer(posttest~group*pretest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=simp_outcome)
lmer_simp_m<-lmer(followup~group*posttest+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=simp_outcome)

print(summary(lmer_simp_b),cor=F)
print(summary(lmer_simp_e),cor=F)
print(summary(lmer_simp_m),cor=F)

#visualisation of significant CRB effects

#Simple RT: significant effect of GSE in Dual Group at Post-Training
intercept<--0.33+0.65
slope<--0.03+0.27

simp_outcome1<-simp_outcome%>%filter(group == "Dual")
gse_posttest_simp_rt<-ggplot(simp_outcome1, aes(x=gse, y=posttest))+geom_point(aes(color="Data points"))+
  geom_abline(intercept=intercept, slope=slope, color="blue")+
  theme_classic()+theme(plot.title=element_text(face="italic",hjust=0.5))+
  scale_color_manual(name = "Training Group", values = ("Data points" = "blue"),labels = ("Data points" = "Dual Group"))+
  labs(title = paste("Effect of Self-Efficacy on Post-Training Simple RT"), x="Standardised Self-Efficacy", y="Standardised Simple RT")
ggsave(here("figs","gse_posttest_simp_rt.png"),plot=gse_posttest_simp_rt,width=8,height=6) #visualises and saves

#simple RT: significant effect of Grit in Simple & Switching Group at Follow-Up
intercept2=-0.11
slope2=0.22
intercept3=-0.11+0.16
slope3=0.22-0.35

simp_outcome2<-simp_outcome%>%filter(group == "Simple")
simp_outcome3<-simp_outcome%>%filter(group == "Switch")

grit_followup_simp_rt<-ggplot()+geom_point(data=simp_outcome2, aes(x=grit, y=followup, color="Simple Group"))+
  geom_abline(intercept=intercept2, slope=slope2, color="red")+
  geom_point(data=simp_outcome3, aes(x=grit, y=followup, color="Switching Group"))+
  geom_abline(intercept=intercept3, slope=slope3, color="green")+
  scale_color_manual(name="Training Group", values=c("Simple Group"="red", "Switching Group"="green"),
                     labels=c("Simple Group"="Simple Group", "Switching Group"="Switching Group"))+
  theme_classic()+theme(plot.title = element_text(face = "italic", hjust = 0.5))+
  labs(title="Effect of Grit on Follow-Up Simple RT", x="Standardised Grit", y="Standardised Simple RT")
ggsave(here("figs","grit_followup_simp_rt.png"),plot=grit_followup_simp_rt,width=8,height=6) #visualises and saves





#################################################################### CHOICE #######################################################################################

choi_outcome<-processed_data %>%
  select(code,site,group,sessionId,demo.age.years,grit,tis,gse,choi_draw_rt,choi_numb_rt,choi_shap_rt) %>%
  pivot_longer(cols=ends_with("_rt"),
               names_to="material",
               values_to="_rt") %>%
  pivot_wider(names_from=sessionId, values_from=`_rt`)

choi_outcome$group <- factor(choi_outcome$group, ordered=FALSE)
choi_outcome$group <- relevel(choi_outcome$group, ref="Simple")

lmer_choi_b<-lmer(pretest~group+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=choi_outcome)
lmer_choi_e<-lmer(posttest~group*pretest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=choi_outcome)
lmer_choi_m<-lmer(followup~group*posttest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=choi_outcome)

#singularity issue with baseline model
isSingular(lmer_choi_b) #true
print(summary(lmer_choi_b),cor=F) #site variance negligible - removed
lmer_choi_b<-lmer(pretest~group+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=choi_outcome)

#checks

#non-normality of residual distribution
qqnorm(resid(lmer_choi_b))
qqnorm(resid(lmer_choi_e))
qqnorm(resid(lmer_choi_m))

#minor heteroscedasticity of variance
plot(lmer_choi_b)
plot(lmer_choi_e)
plot(lmer_choi_m)

#outlier removal and transformations to rectify distributional violations

#remove >=200ms RTs and 3MAD+median< RTs
pps_exclude1<-choi_outcome%>%
  group_by(code) %>%
  filter(any(pretest < 200)) %>%
  filter(any(posttest < 200)) %>%
  filter(any(followup < 200)) %>%
  pull(code) %>% unique() #none

medianRT1<-median(choi_outcome$pretest)
MAD_3_RT1<-3*mad(choi_outcome$pretest)
medianRT2<-median(choi_outcome$posttest)
MAD_3_RT2<-3*mad(choi_outcome$posttest)
medianRT3<-median(choi_outcome$followup)
MAD_3_RT3<-3*mad(choi_outcome$followup)

pps_exclude2<-choi_outcome%>%
  group_by(code) %>%
  filter(pretest > (medianRT1+MAD_3_RT1)) %>%
  filter(posttest > (medianRT2+MAD_3_RT2)) %>%
  filter(followup > (medianRT3+MAD_3_RT3)) %>%
  pull(code) %>% unique() #5 extreme Ids

extremes<-union(pps_exclude1,pps_exclude2)
choi_outcome<-choi_outcome %>%
  filter(!choi_outcome$code %in% extremes) #removes all data for extreme pps (-15 observations)

#transformation
choi_outcome$pretest<-log(choi_outcome$pretest)
choi_outcome$posttest<-log(choi_outcome$posttest)
choi_outcome$followup<-log(choi_outcome$followup)

#re-run lines 333-334 and 339

#cooks distance method
cooksD<-cooks.distance(lmer_choi_b)
influential1<-choi_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_choi_e)
influential2<-choi_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_choi_m)
influential3<-choi_outcome$code[which(cooksD>1)]
influential<-union(influential1,influential2)
influential<-union(influential,influential3) #6 influential outlying observations; 3 pps
choi_outcome<-choi_outcome[!choi_outcome$code %in% influential, ] #removes all data for extreme pps (-9 observations) (1044 in total)

#z-standardise
choi_outcome$demo.age.years<-scale(choi_outcome$demo.age.years)
choi_outcome$grit<-scale(choi_outcome$grit)
choi_outcome$tis<-scale(choi_outcome$tis)
choi_outcome$gse<-scale(choi_outcome$gse)
choi_outcome$pretest<-scale(choi_outcome$pretest)
choi_outcome$posttest<-scale(choi_outcome$posttest)
choi_outcome$followup<-scale(choi_outcome$followup)

#re-run lines 333-334 and 339

#lack of perfect multicolinearity
multicolinearity_test<-vif(lmer_choi_b, type="predictor") #grit (2.3), gse (2.2)
multicolinearity_test<-vif(lmer_choi_e, type="predictor") #grit (2.3), gse (2.1)
multicolinearity_test<-vif(lmer_choi_m, type="predictor") #grit (2.3), gse (2.2)

#final models
lmer_choi_b<-lmer(pretest~group+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=choi_outcome)
lmer_choi_e<-lmer(posttest~group*pretest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=choi_outcome)
lmer_choi_m<-lmer(followup~group*posttest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=choi_outcome)

print(summary(lmer_choi_b),cor=F)
print(summary(lmer_choi_e),cor=F)
print(summary(lmer_choi_m),cor=F)




################################################################### SWITCHING #####################################################################################

swit_outcome<-processed_data %>%
  select(code,site,group,sessionId,demo.age.years,grit,tis,gse,swit_draw_rt,swit_numb_rt,swit_shap_rt) %>%
  pivot_longer(cols=ends_with("_rt"),
               names_to="material",
               values_to="_rt") %>%
  pivot_wider(names_from=sessionId, values_from=`_rt`)

swit_outcome$group <- factor(swit_outcome$group, ordered=FALSE)
swit_outcome$group <- relevel(swit_outcome$group, ref="Simple")

lmer_swit_b<-lmer(pretest~group+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=swit_outcome)
lmer_swit_e<-lmer(posttest~group*pretest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=swit_outcome)
lmer_swit_m<-lmer(followup~group*posttest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=swit_outcome) 

#singularity issue with baseline and enhancement model
isSingular(lmer_swit_b) #true
isSingular(lmer_swit_e) #true
print(summary(lmer_swit_b),cor=F) #site explains no variance, #respectification below
print(summary(lmer_swit_e),cor=F) #site explains no variance, #respectification below
lmer_swit_b<-lmer(pretest~group+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=swit_outcome)
lmer_swit_e<-lmer(posttest~group*pretest+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=swit_outcome)

#checks

#non-normality of residual distribution
qqnorm(resid(lmer_swit_b))
qqnorm(resid(lmer_swit_e))
qqnorm(resid(lmer_swit_m))

#minor heteroscedasticity of variance
plot(lmer_swit_b)
plot(lmer_swit_e)
plot(lmer_swit_m)

#outlier removal and transformations to rectify distributional violations

#remove >=200ms RTs and 3MAD+median< RTs
pps_exclude1<-swit_outcome%>%
  group_by(code) %>%
  filter(any(pretest < 200)) %>%
  filter(any(posttest < 200)) %>%
  filter(any(followup < 200)) %>%
  pull(code) %>% unique() #2 extreme Ids

medianRT1<-median(swit_outcome$pretest)
MAD_3_RT1<-3*mad(swit_outcome$pretest)
medianRT2<-median(swit_outcome$posttest)
MAD_3_RT2<-3*mad(swit_outcome$posttest)
medianRT3<-median(swit_outcome$followup)
MAD_3_RT3<-3*mad(swit_outcome$followup)

pps_exclude2<-swit_outcome%>%
  group_by(code) %>%
  filter(pretest > (medianRT1+MAD_3_RT1)) %>%
  filter(posttest > (medianRT2+MAD_3_RT2)) %>%
  filter(followup > (medianRT3+MAD_3_RT3)) %>%
  pull(code) %>% unique() #3 extreme Ids

extremes<-union(pps_exclude1,pps_exclude2) #5 extreme Ids
swit_outcome<-swit_outcome %>%
  filter(!swit_outcome$code %in% extremes) #removes all data for extreme pps (-15 observations)

#transformation
swit_outcome$pretest<-log(swit_outcome$pretest)
swit_outcome$posttest<-log(swit_outcome$posttest)
swit_outcome$followup<-log(swit_outcome$followup)

#re-run lines 441, 448-449

#cooks distance method
cooksD<-cooks.distance(lmer_swit_b)
influential1<-swit_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_swit_e)
influential2<-swit_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_swit_m)
influential3<-swit_outcome$code[which(cooksD>1)]
influential<-union(influential1,influential2)
influential<-union(influential,influential3) #11 influential outlying observations; 5 pps
swit_outcome<-swit_outcome[!swit_outcome$code %in% influential, ] #removes all data for extreme pps (-15 observations) (1038 in total)

#z-standardise
swit_outcome$demo.age.years<-scale(swit_outcome$demo.age.years)
swit_outcome$grit<-scale(swit_outcome$grit)
swit_outcome$tis<-scale(swit_outcome$tis)
swit_outcome$gse<-scale(swit_outcome$gse)
swit_outcome$pretest<-scale(swit_outcome$pretest)
swit_outcome$posttest<-scale(swit_outcome$posttest)
swit_outcome$followup<-scale(swit_outcome$followup)

#re-run lines 441, 448-449

#lack of perfect multicolinearity
multicolinearity_test<-vif(lmer_swit_b, type="predictor") #grit (2.3), gse (2.2)
multicolinearity_test<-vif(lmer_swit_e, type="predictor") #grit (2.3), gse (2.2)
multicolinearity_test<-vif(lmer_swit_m, type="predictor") #posttest (2.2), grit (2.3), gse (2.2)

#final models: (1|site) re-included as singularities resolved following outlier removal
lmer_swit_b<-lmer(pretest~group+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=swit_outcome)
lmer_swit_e<-lmer(posttest~group*pretest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=swit_outcome)
lmer_swit_m<-lmer(followup~group*posttest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=swit_outcome) 

print(summary(lmer_swit_b),cor=F)
print(summary(lmer_swit_e),cor=F)
print(summary(lmer_swit_m),cor=F)

#visualisation of significant CRB effects

#Switching RT: significant effect of Grit in Simple Group at Baseline
intercept<--0.01
slope<--0.22

swit_outcome1<-swit_outcome%>%filter(group == "Simple")
grit_baseline_swit_rt<-ggplot(simp_outcome1, aes(x=grit, y=pretest))+geom_point(aes(color="Data points"))+
  geom_abline(intercept=intercept, slope=slope, color="red")+
  theme_classic()+theme(plot.title=element_text(face="italic",hjust=0.5))+
  scale_color_manual(name = "Training Group", values = ("Data points" = "red"),labels = ("Data points" = "Simple Group"))+
  labs(title = paste("Effect of Grit on Baseline Switching RT"), x="Standardised Grit", y="Standardised Switching RT")
ggsave(here("figs","grit_baseline_swit_rt.png"),plot=grit_baseline_swit_rt,width=8,height=6) #visualises and saves

#Switching RT: significant effect of Grit in Choice Group at Follow-Up
intercept<--0.17+0.11
slope<--0.10+0.15

swit_outcome2<-swit_outcome%>%filter(group == "Choice")
grit_followup_swit_rt<-ggplot(swit_outcome2, aes(x=grit, y=followup))+geom_point(aes(color="Data points"))+
  geom_abline(intercept=intercept, slope=slope, color="orange")+
  theme_classic()+theme(plot.title=element_text(face="italic",hjust=0.5))+
  scale_color_manual(name = "Training Group", values = ("Data points" = "orange"),labels = ("Data points" = "Choice Group"))+
  labs(title = paste("Effect of Grit on Follow-Up Switching RT"), x="Standardised Grit", y="Standardised Switching RT")
ggsave(here("figs","grit_followup_swit_rt.png"),plot=grit_followup_swit_rt,width=8,height=6)





########################################################## DUAL #####################################################################

dual_outcome<-processed_data %>%
  select(code,site,group,sessionId,demo.age.years,grit,tis,gse,dual_draw_rt,dual_numb_rt,dual_shap_rt) %>%
  pivot_longer(cols=ends_with("_rt"),
               names_to="material",
               values_to="_rt") %>%
  pivot_wider(names_from=sessionId, values_from=`_rt`)

dual_outcome$group <- factor(dual_outcome$group, ordered=FALSE)
dual_outcome$group <- relevel(dual_outcome$group, ref="Simple")

lmer_dual_b<-lmer(pretest~group+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome)
lmer_dual_e<-lmer(posttest~group*pretest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome)
lmer_dual_m<-lmer(followup~group*posttest+(1|site)+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome) 

#singularity issue with maintenance model
isSingular(lmer_dual_m) #true
print(summary(lmer_dual_m),cor=F) #material explains no variance, #respectification below
lmer_dual_m<-lmer(followup~group*posttest+(1|site)+(1|code)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome)

#checks

#non-normality of residual distribution
qqnorm(resid(lmer_dual_b))
qqnorm(resid(lmer_dual_e))
qqnorm(resid(lmer_dual_m))

#minor heteroscedasticity of variance
plot(lmer_dual_b)
plot(lmer_dual_e)
plot(lmer_dual_m)

#outlier removal and transformations to rectify distributional violations

#remove >=200ms RTs and 3MAD+median< RTs
pps_exclude1<-dual_outcome%>%
  group_by(code) %>%
  filter(any(pretest < 200)) %>%
  filter(any(posttest < 200)) %>%
  filter(any(followup < 200)) %>%
  pull(code) %>% unique() #3 extreme Ids

medianRT1<-median(dual_outcome$pretest)
MAD_3_RT1<-3*mad(dual_outcome$pretest)
medianRT2<-median(dual_outcome$posttest)
MAD_3_RT2<-3*mad(dual_outcome$posttest)
medianRT3<-median(dual_outcome$followup)
MAD_3_RT3<-3*mad(dual_outcome$followup)

pps_exclude2<-dual_outcome%>%
  group_by(code) %>%
  filter(pretest > (medianRT1+MAD_3_RT1)) %>%
  filter(posttest > (medianRT2+MAD_3_RT2)) %>%
  filter(followup > (medianRT3+MAD_3_RT3)) %>%
  pull(code) %>% unique() #4 extreme Ids

extremes<-union(pps_exclude1,pps_exclude2) #7 extreme Ids
dual_outcome<-dual_outcome %>%
  filter(!dual_outcome$code %in% extremes) #removes all data for extreme pps (-21 observations)

#transformation
dual_outcome$pretest<-log(dual_outcome$pretest)
dual_outcome$posttest<-log(dual_outcome$posttest)
dual_outcome$followup<-log(dual_outcome$followup)

#re-run lines 576-577 and 583

#singularity issue for all models - remove site for all, + material for maintanenace
lmer_dual_b<-lmer(pretest~group+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome)
lmer_dual_e<-lmer(posttest~group*pretest+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome)
lmer_dual_m<-lmer(followup~group*posttest+(1|code)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome)

#cooks distance method
cooksD<-cooks.distance(lmer_dual_b)
influential1<-dual_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_dual_e)
influential2<-dual_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_dual_m)
influential3<-dual_outcome$code[which(cooksD>1)]
influential<-union(influential1,influential2)
influential<-union(influential,influential3) #16 influential outlying observations; 8 pps
dual_outcome<-dual_outcome[!dual_outcome$code %in% influential, ] #removes all data for extreme pps (-24 observations) (1023 in total)

#z-standardise
dual_outcome$demo.age.years<-scale(dual_outcome$demo.age.years)
dual_outcome$grit<-scale(dual_outcome$grit)
dual_outcome$tis<-scale(dual_outcome$tis)
dual_outcome$gse<-scale(dual_outcome$gse)
dual_outcome$pretest<-scale(dual_outcome$pretest)
dual_outcome$posttest<-scale(dual_outcome$posttest)
dual_outcome$followup<-scale(dual_outcome$followup)

#re-run lines 576-577 and 583

#lack of perfect multicolinearity
multicolinearity_test<-vif(lmer_dual_b, type="predictor") #grit (2.3), gse (2.2)
multicolinearity_test<-vif(lmer_dual_e, type="predictor") #pretest (2.3), grit (2.3), gse (2.2)
multicolinearity_test<-vif(lmer_dual_m, type="predictor") #posttest (2.3), grit (2.3), gse (2.2)

#final models: (1|material) now functions for maintenance model
lmer_dual_b<-lmer(pretest~group+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome)
lmer_dual_e<-lmer(posttest~group*pretest+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome)
lmer_dual_m<-lmer(followup~group*posttest+(1|code)+(1|material)+demo.age.years+grit*group+gse*group+tis*group,data=dual_outcome) 

print(summary(lmer_dual_b),cor=F)
print(summary(lmer_dual_e),cor=F)
print(summary(lmer_dual_m),cor=F)




###################################################### REASONING #####################################################################

reas_outcome<-processed_data %>%
  select(code,site,group,sessionId,demo.age.years,grit,tis,gse,assessment.reas.lettersets.score,assessment.reas.paperfolding.score,assessment.reas.rapm.score) %>%
  pivot_longer(cols=ends_with("score"),
               names_to="task",
               values_to="score") %>%
  pivot_wider(names_from=sessionId, values_from=`score`)

reas_outcome$group <- factor(reas_outcome$group, ordered=FALSE)
reas_outcome$group <- relevel(reas_outcome$group, ref="Simple")

lmer_reas_b<-lmer(pretest~group+(1|site)+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=reas_outcome)
lmer_reas_e<-lmer(posttest~group*pretest+(1|site)+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=reas_outcome)
lmer_reas_m<-lmer(followup~group*posttest+(1|site)+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=reas_outcome)

#failure to converge for enhancement model - removal of site random effect resolves
lmer_reas_e<-lmer(posttest~group*pretest+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=reas_outcome)

#singularity issue with maintenance model
isSingular(lmer_reas_m) #true
print(summary(lmer_reas_m),cor=F) #site explains no variance, #respectification below
lmer_reas_m<-lmer(followup~group*posttest+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=reas_outcome)

#checks

#normality of residual distribution
qqnorm(resid(lmer_reas_b))
qqnorm(resid(lmer_reas_e))
qqnorm(resid(lmer_reas_m))

#homoscedasticity of variance and linearity of model relationships
plot(lmer_reas_b)
plot(lmer_reas_e)
plot(lmer_reas_m)

#cooks distance method
cooksD<-cooks.distance(lmer_reas_b)
influential1<-reas_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_reas_e)
influential2<-reas_outcome$code[which(cooksD>1)]
cooksD<-cooks.distance(lmer_reas_m)
influential3<-reas_outcome$code[which(cooksD>1)]
influential<-union(influential1,influential2)
influential<-union(influential,influential3) #no infuential outliers

#z-standardise
reas_outcome$demo.age.years<-scale(reas_outcome$demo.age.years)
reas_outcome$grit<-scale(reas_outcome$grit)
reas_outcome$tis<-scale(reas_outcome$tis)
reas_outcome$gse<-scale(reas_outcome$gse)
reas_outcome$pretest<-scale(reas_outcome$pretest)
reas_outcome$posttest<-scale(reas_outcome$posttest)
reas_outcome$followup<-scale(reas_outcome$followup)

#re-run lines 688, 693, and 698

#lack of perfect multicolinearity
multicolinearity_test<-vif(lmer_reas_b, type="predictor") #grit (2.2), gse (2.1)
multicolinearity_test<-vif(lmer_reas_e, type="predictor") #pretest (2.1), grit (2.2), gse (2.1)
multicolinearity_test<-vif(lmer_reas_m, type="predictor") #posttest (2), grit (2.2), gse (2.1)

#final models
lmer_reas_b<-lmer(pretest~group+(1|site)+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=reas_outcome)
lmer_reas_e<-lmer(posttest~group*pretest+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=reas_outcome)
lmer_reas_m<-lmer(followup~group*posttest+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=reas_outcome)

print(summary(lmer_reas_b),cor=F)
print(summary(lmer_reas_e),cor=F)
print(summary(lmer_reas_m),cor=F)

#visualisation of significant CRB effects

#Reasoning: significant effect of Mindset in Switching Group at Baseline
intercept<--0.04+0.003
slope<--0.02-0.22

reas_outcome1<-reas_outcome%>%filter(group == "Switch")
tis_baseline_reas<-ggplot(reas_outcome1, aes(x=tis, y=pretest))+geom_point(aes(color="Data points"))+
  geom_abline(intercept=intercept, slope=slope, color="green")+
  theme_classic()+theme(plot.title=element_text(face="italic",hjust=0.5))+
  scale_color_manual(name = "Training Group", values = ("Data points" = "green"),labels = ("Data points" = "Switching Group"))+
  labs(title = paste("Effect of Mindset on Baseline Reasoning Skill"), x="Standardised Mindset", y="Standardised Reasoning Skill")
ggsave(here("figs","tis_baseline_reas.png"),plot=tis_baseline_reas,width=8,height=6) #visualises and saves

#Reasoning: significant effect of Grit in Switching Group at Followup
intercept<-0.01-0.08
slope<-0.04-0.16

grit_followup_reas<-ggplot(reas_outcome1, aes(x=grit, y=followup))+geom_point(aes(color="Data points"))+
  geom_abline(intercept=intercept, slope=slope, color="green")+
  theme_classic()+theme(plot.title=element_text(face="italic",hjust=0.5))+
  scale_color_manual(name = "Training Group", values = ("Data points" = "green"),labels = ("Data points" = "Switching Group"))+
  labs(title = paste("Effect of Grit on Follow-Up Reasoning Skill"), x="Standardised Grit", y="Standardised Reasoning Skill")
ggsave(here("figs","grit_followup_reas.png"),plot=grit_followup_reas,width=8,height=6) #visualises and saves




################################################## WORKING MEMORY ##################################################################

#z-standardised to allow aggregation of entirely differently scaled task measures
processed_data2<-processed_data
processed_data2$wm.binding.dprime<-scale(processed_data2$wm.binding.dprime)
processed_data2$wm.updating.accuracy<-scale(processed_data2$wm.updating.accuracy)

#wm.error: prior to scaling; absolute valuing and setting as negative so that scoring is based on magnitude, not direction of deviation
processed_data2$wm.reproduction.error<--abs(scale(processed_data2$wm.reproduction.error))

wrme_outcome<-processed_data2 %>%
  select(code,site,group,sessionId,demo.age.years,grit,tis,gse,wm.updating.accuracy,wm.binding.dprime,wm.reproduction.error) %>%
  pivot_longer(cols=starts_with("wm"),
               names_to="task",
               values_to="wm") %>%
  pivot_wider(names_from=sessionId, values_from=`wm`)

wrme_outcome$group <- factor(wrme_outcome$group, ordered=FALSE)
wrme_outcome$group <- relevel(wrme_outcome$group, ref="Simple")

lmer_wrme_b<-lmer(pretest~group+(1|site)+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=wrme_outcome)
lmer_wrme_e<-lmer(posttest~group*pretest+(1|site)+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=wrme_outcome)
lmer_wrme_m<-lmer(followup~group*posttest+(1|site)+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=wrme_outcome)

#singularity issue with enhancement & maintenance model
isSingular(lmer_wrme_e) #true
isSingular(lmer_wrme_m) #true
print(summary(lmer_wrme_e),cor=F) #site and code explains no variance, #respectification below
print(summary(lmer_wrme_m),cor=F) #site explains no variance, #respectification below
lmer_wrme_e<-lmer(posttest~group*pretest+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=wrme_outcome) #must retain code
lmer_wrme_m<-lmer(followup~group*posttest+(1|code)+(1|task)+demo.age.years+grit*group+gse*group+tis*group,data=wrme_outcome)

#checks

#non-normality of residual distribution
qqnorm(resid(lmer_wrme_b))
qqnorm(resid(lmer_wrme_e))
qqnorm(resid(lmer_wrme_m))

#minor heteroscedasticity of variance
plot(lmer_wrme_b)
plot(lmer_wrme_e)
plot(lmer_wrme_m)

#cannot LOG transform to aid normalisation (contains -values), can check influence of outliers

#cooks distance method
cooksD<-cooks.distance(lmer_wrme_b)
influential1<-which(cooksD>1)
cooksD<-cooks.distance(lmer_wrme_e)
influential2<-which(cooksD>1)
cooksD<-cooks.distance(lmer_wrme_m)
influential3<-which(cooksD>1) #no outliers deemed as influential

#potential perfect multicolinearity
multicolinearity_test<-vif(lmer_wrme_b, type="predictor") #*extreme multicolinearity in group w/ group*interaction effects
multicolinearity_test<-vif(lmer_wrme_e, type="predictor") #*
multicolinearity_test<-vif(lmer_wrme_m, type="predictor") #*

#re-specification: removal of cognitive belief interaction effects with group
lmer_wrme_b<-lmer(pretest~group+(1|site)+(1|code)+(1|task)+demo.age.years+grit+gse+tis,data=wrme_outcome)
lmer_wrme_e<-lmer(posttest~group*pretest+(1|code)+(1|task)+demo.age.years+grit+gse+tis,data=wrme_outcome)
lmer_wrme_m<-lmer(followup~group*posttest+(1|code)+(1|task)+demo.age.years+grit+gse+tis,data=wrme_outcome)

#re-checks

#lack of perfect multicolinearity
multicolinearity_test<-vif(lmer_wrme_b, type="predictor") #none
multicolinearity_test<-vif(lmer_wrme_e, type="predictor") #pretest (2)
multicolinearity_test<-vif(lmer_wrme_m, type="predictor") #none

#non-normality of residual distribution
qqnorm(resid(lmer_wrme_b))
qqnorm(resid(lmer_wrme_e))
qqnorm(resid(lmer_wrme_m))

#minor heteroscedasticity of variance
plot(lmer_wrme_b)
plot(lmer_wrme_e)
plot(lmer_wrme_m)

#cooks distance method
cooksD<-cooks.distance(lmer_wrme_b)
influential1<-which(cooksD>1)
cooksD<-cooks.distance(lmer_wrme_e)
influential2<-which(cooksD>1)
cooksD<-cooks.distance(lmer_wrme_m)
influential3<-which(cooksD>1)
wrme_outcome<-wrme_outcome[-influential2, ] #no outliers deemed as influential
#value 274 outlying is extremely outlying and low on the enhancement model; removal did not bias or impact estimates/statistical significance, so was retained

#z-standardisation - corrects distribution differences of means for each wrme measure to be aligned; assumption that all measure wrme equally
wrme_outcome$demo.age.years<-scale(wrme_outcome$demo.age.years)
wrme_outcome$grit<-scale(wrme_outcome$grit)
wrme_outcome$tis<-scale(wrme_outcome$tis)
wrme_outcome$gse<-scale(wrme_outcome$gse)
wrme_outcome$pretest<-scale(wrme_outcome$pretest)
wrme_outcome$posttest<-scale(wrme_outcome$posttest)
wrme_outcome$followup<-scale(wrme_outcome$followup)

#final models
lmer_wrme_b<-lmer(pretest~group+(1|site)+(1|code)+(1|task)+demo.age.years+grit+gse+tis,data=wrme_outcome)
lmer_wrme_e<-lmer(posttest~group*pretest+(1|code)+(1|task)+demo.age.years+grit+gse+tis,data=wrme_outcome)
lmer_wrme_m<-lmer(followup~group*posttest+(1|code)+(1|task)+demo.age.years+grit+gse+tis,data=wrme_outcome)

print(summary(lmer_wrme_b),cor=F)
print(summary(lmer_wrme_e),cor=F)
print(summary(lmer_wrme_m),cor=F)





########################################################### MODEL EVALUATIONS ###########################################################################################

model_list<-list(lmer_simp_b,lmer_simp_e,lmer_simp_m,
                 lmer_choi_b,lmer_choi_e,lmer_choi_m,
                 lmer_swit_b,lmer_swit_e,lmer_swit_m,
                 lmer_dual_b,lmer_dual_e,lmer_dual_m,
                 lmer_reas_b,lmer_reas_e,lmer_reas_m,
                 lmer_wrme_b,lmer_wrme_e,lmer_wrme_m)
model_names<-c("lmer_simp_b","lmer_simp_e","lmer_simp_m",
               "lmer_choi_b","lmer_choi_e","lmer_choi_m",
               "lmer_swit_b","lmer_swit_e","lmer_swit_m",
               "lmer_dual_b","lmer_dual_e","lmer_dual_m",
               "lmer_reas_b","lmer_reas_e","lmer_reas_m",
               "lmer_wrme_b","lmer_wrme_e","lmer_wrme_m")

icc_results<-list()
ranova_results<-list()
r_squared_results<-list()

for(i in seq_along(model_list)) {
  model<-model_list[[i]]
  model_name<-model_names[[i]]
  icc_results[[model_name]]<-performance::icc(model)
  ranova_results[[model_name]]<-ranova(model)
  r_squared_results[[model_name]]<-r.squaredGLMM(model,null)
}
print(icc_results) #intra-class correlation: variance explained by random effects
print(ranova_results) #repeated measures anova: significance of random effects
print(r_squared_results) #marginal & conditional r-squared: varianced explained by fixed/+random effects





################################### POST-HOC ANOVAs FOR CHECKNG CRBs BETWEEN TRAINING GROUPS ######################################################################

crb<-crb%>% semi_join(processed_data,crb,by="code") #refines to only include final sample

#grit
group_grit<-aov(grit~group, data=crb)
summary(group_grit) #no sig. differences
means_grit<-crb %>%
  group_by(group) %>%
  summarise(mean_grit=mean(grit))
print(means_grit) #Simple=3.55, Choice=3.55, Switching=3.56, Dual=3.49

#assumptions
qqnorm(group_grit$residuals) #minor violation, robust as large sample
boxplot(grit~group, xlab='training intervention', ylab='grit', data=crb) #minor violation, robust as similar group sizes

#tis 
group_tis<-aov(tis~group, data=crb)
summary(group_tis) #no sig. differences
means_tis<-crb %>%
  group_by(group) %>%
  summarise(mean_tis=mean(tis))
print(means_tis) #Simple=4.05, Choice=4.12, Switching=4.10, Dual=4.10

#assumptions
qqnorm(group_tis$residuals) #minor violation, robust as large sample
boxplot(tis~group, xlab='training intervention', ylab='tis', data=crb) #minor violation, robust as similar group sizes

#gse 
group_gse<-aov(gse~group, data=crb)
summary(group_gse) #no sig. difference
means_gse<-crb %>%
  group_by(group) %>%
  summarise(mean_gse=mean(gse))
print(means_gse) #Switch=3.13, Simple=3.24, Choice=3.16, Dual=3.2

#assumptions
qqnorm(group_gse$residuals) #minor violation, robust as large sample
boxplot(gse~group, xlab='training intervention', ylab='gse', data=crb) #minor violation, robust as similar group sizes

#LOG-transformation to resolve distributional violations
crb$grit<-log(crb$grit)
crb$tis<-log(crb$tis)
crb$gse<-log(crb$gse)

#final models
group_grit<-aov(grit~group, data=crb)
group_tis<-aov(tis~group, data=crb)
group_gse<-aov(gse~group, data=crb)

summary(group_grit)
summary(group_tis)
summary(group_gse)
