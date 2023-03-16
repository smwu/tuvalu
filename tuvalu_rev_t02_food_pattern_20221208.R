# install the package needed, 'install.packages("")' for installation
# use library to call them 
library(tidyverse)
library(readxl)
library(lubridate)
library(tableone)
#Output file to excel
library(openxlsx)
#Make plot
library(corrplot)
library(poLCA)
library(insight)
library(forcats)

#==================== Helper functions ================================
#Create functions to extract estimate, CI and p-values
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
linear_coef<-function(b){
  cbind(names(b$coefficients[,1]),paste(specify_decimal(b$coefficients[,1],2)," (",specify_decimal(b$coefficients[,1]+qnorm(0.025)*b$coefficients[,2],2),"-",specify_decimal(b$coefficients[,1]+qnorm(c(.975))*b$coefficients[,2],2),", p=",specify_decimal(b$coefficients[,4],3),")",sep=""))
}
logistic_coef<-function(b){
  cbind(names(b$coefficients[,1]),paste(specify_decimal(exp(b$coefficients[,1]),2)," (",specify_decimal(exp(b$coefficients[,1]+qnorm(0.025)*b$coefficients[,2]),2),"-",specify_decimal(exp(b$coefficients[,1]+qnorm(c(.975))*b$coefficients[,2]),2),", p=",specify_decimal(b$coefficients[,4],3),")",sep=""))
}

#==================== Import and clean data ===================================
#Dataset manipulation: R is case-sensitive, so we use lower case for all variables

#import the file, we read the second sheet, which was transposed successfully by the team in Tuvalu (,2) for second sheet
#Note: I use google drive here, so please feel free to set another folder according to where you store the data
#tuvalu<-read_excel("G:/My Drive/tuvalu/Dataset/2022-Nutrition-Survey_20220530.xlsx")
#Set up working directory, save the output into T02 folders
#setwd("G:/My Drive/tuvalu/Results/work/T02/20221208")
setwd("/Users/Stephanie/Documents/GitHub/tuvalu")
tuvalu <- read_excel("2022-Nutrition-Survey_20220530.xlsx")

#use pipeline in package 'dplyr' to clean data; mutate the names of variables

#Data cleaning 2022 data
#Indicator for those with same responses in knowledge questions
tuvalu$kledg_exclude<-ifelse(apply(tuvalu[, 19:28], 1, function(i) length(unique(i)) > 1)==TRUE,0,1)
tuvalu1<-tuvalu%>%mutate(
  #Create year identifier
  year="2022",
  #Create combine indicator "yearid"= year+study ID
  yearid=paste(year,`Participant No.`),
  #studyid as categorical one, each subject should have unique ID
  studyid=factor(`Participant No.`),
  #Study region and date of interview
  region=factor(`Interview Region`),
  #Main island as 0 (both students and high schoolers), 1 as other islands
  region_c=factor(ifelse(region%in%c(1,10),0,1)),
  #Grouping of outlying islands, (0) as funafuti, (1) as North three islands (including Nanumea, Nanumaga, and Niutao), (2) as Middle three islands (including Vaitupu, Nukufetau, and Nui), and (3) as South two islands (including Nukulaelae and Niulakita)
  region_4group_cat=ifelse(region%in% c(1,10),0,ifelse(region%in% c(2,3,4),1,ifelse(region%in% c(5,6,7,11),2,ifelse(region  %in% c(8,9),3,NA)))),
  #Population density of each island
  popln_density= ifelse(region %in% c(1,10) , 6320/2.4,
    ifelse(region == 10 , 6320/2.4,
    ifelse(region == 2 , 512/3.87,
    ifelse(region == 3 , 491/3,
    ifelse(region == 4 , 582/2.53,
    ifelse(region == 6 , 610/2.83,
    ifelse(region == 7 , 597/2.99,
    ifelse(region == 8 , 300/1.82,
    ifelse(region == 9 , 34/0.4,
    ifelse(region %in% c(5,11) , 1061/5.6, NA
  )))))))))),
  # High school student: 0 as Funafuti (interview region 10), 1 as Motufoua High School (interview region 11), 2 as adults 
  highschool= case_when(
    region == 10 ~ 0,
    region == 11 ~ 1,
    TRUE ~ 2
  ),
  #Adult (1) versus adolescent (2)
  adult_c=ifelse(highschool==2,1,0),
  #Date of interview
  interview_date= ymd(`Interview Date`),
  #body measurements
  #Height (cm): Exclude extreme values (>250 or <80 cm)
  ht=as.numeric(`Height (cm)`),
  #Weight: Exclude extreme values (<0 kg)
  wt=as.numeric(`Weight (kg)`),
  bmi=10000*wt/(ht)^2,
  bmi=ifelse(bmi>100,NA,bmi),
  #Categorize for overweight and obesity with conventional cutpoint (<25 as 1, 25-30 as 2, and >30 as 3)
  bmi_c= cut(bmi, c(0, 25, 30 ,35 ,40 , Inf),
    include.lowest = TRUE),
  obesity_1=ifelse(bmi>=30,1,0),
  obesity_2=ifelse(bmi>=35,1,0),
  obesity_3=ifelse(bmi>=40,1,0),
  #Waist circumference: Exclude extreme values (<0 cm) 
  wc=as.numeric(`Waist Circumference (cm)`),
  #demographics
  #Gender of subjects: Male=1, Female=2
  gender=factor(Q1),
  #Use gender and wc to define higher waist circ. (Men: >90, female: >80)
  wc_h= case_when(
    gender==1 & wc>90 ~ 1,
    gender==2 & wc>80 ~ 1,
    TRUE ~ 0),
  #age in years
  age=ifelse((2022-as.numeric(Q2))>150,NA,2022-as.numeric(Q2)),
  age_c=cut(age, c(0,30,40,50,60,70,Inf)),
  #Marital status
  marital=factor(Q3),
  #Highest education level and construct education level, dichotomized into lower=0 (level 1,2,3; to secondary school) higher=1 (tertiary)
  education=factor(Q4),
  education_c=factor(ifelse(Q4==4,1,0)),
  #Tuvalu citizenship (if==2 then end interview) and residence before 18 y/o
  citizen=factor(Q5),
  region_before18=factor(Q6),
  #How long do we live in current region
  region_now_c=factor(Q7),
  #1. Regular or fixed work;2. Temporary work; 3. No or students
  employ=factor(Q8),
  #construct income, dichotomized into lower=0 (level 1,2) and higher=1 (others); considering annual income at 1000 USD, make high versus low at 1000 AUD
  income=as.numeric(Q9),
  income_c=ifelse(income>1000,1,0),
  #Cut by median because only 17 subjects>1000 AUD
  income_h=factor(ifelse(income>median(income, na.rm=TRUE),1,0)),
  #Self-reported medical history
  #hypertension
  htn=factor(Q10),
  #dyslipidemia
  dl=factor(Q11),
  #Diabetes
  dm=factor(Q12),
  # NCD defined as HTN or DM or DL
  ncd=factor(ifelse(Q10==1|Q11==1|Q12==1,1,0)),
  #Health-related knowledge, change the response into categorical (rename as "kledg1_n")
  #For 2022
  #The correct answers are 1122211221, so we change the values "kledg_" to correct (1) or not (0, included the unknowns)
  kledg1_1=ifelse(Q13==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg1_2=ifelse(Q14==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg1_3=ifelse(Q15==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg1_4=ifelse(Q16==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg1_5=ifelse(Q17==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg1_6=ifelse(Q18==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg1_7=ifelse(Q19==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg1_8=ifelse(Q20==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg1_9=ifelse(Q21==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg1_10=ifelse(Q22==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_sum=ifelse(kledg_exclude==1,NA,kledg1_1+kledg1_2+kledg1_3+kledg1_4+kledg1_5+kledg1_6+kledg1_7+kledg1_8+kledg1_9+kledg1_10),
  kledg_h=ifelse(kledg_sum>median(kledg_sum,na.rm = TRUE),1,ifelse(kledg_exclude==1,NA,0)),
  #Health-related attitude, change the response into categorical variable (rename as "att_n"),
  #Higher awareness in attitute as 1 (Strongly or agree), otherwise=0.
  att_1=as.integer(Q23),
  att_2=as.integer(Q24),
  att_3=as.integer(Q25),
  att_4=as.integer(Q26),
  att_5=as.integer(Q27),
  att_6=as.integer(Q28),
  att_7=as.integer(Q29),
  att_8=as.integer(Q30),
  att_9=as.integer(Q31),   
  #Food frequency items
  #Keep names as they are (original dataset 38:62, 25 items) except for several one with special chars
  Rice=ifelse(Rice<3,1,ifelse(Rice==3,2,ifelse(Rice>3,3,NA))),
  `Swamp taro or taro`=ifelse(`Swamp taro or taro`<3,1,ifelse(`Swamp taro or taro`==3,2,ifelse(`Swamp taro or taro`>3,3,NA))),
  Breadfruit=ifelse(Breadfruit<3,1,ifelse(Breadfruit==3,2,ifelse(Breadfruit>3,3,NA))),
  Fish=ifelse(Fish<3,1,ifelse(Fish==3,2,ifelse(Fish>3,3,NA))),
  Pork=ifelse(Pork<3,1,ifelse(Pork==3,2,ifelse(Pork>3,3,NA))),
  Cabbage=ifelse(Cabbage<3,1,ifelse(Cabbage==3,2,ifelse(Cabbage>3,3,NA))),
  bird_nest_fern=ifelse(bird_nest_fern<3,1,ifelse(bird_nest_fern==3,2,ifelse(bird_nest_fern>3,3,NA))),
  Banana=ifelse(Banana<3,1,ifelse(Banana==3,2,ifelse(Banana>3,3,NA))),
  Coconut=ifelse(Coconut<3,1,ifelse(Coconut==3,2,ifelse(Coconut>3,3,NA))),
  `Imported fruits (Ex. apples, oranges, pears)`=ifelse(`Imported fruits (Ex. apples, oranges, pears)`<3,1,ifelse(`Imported fruits (Ex. apples, oranges, pears)`==3,2,ifelse(`Imported fruits (Ex. apples, oranges, pears)`>3,3,NA))),
  Eggs=ifelse(Eggs<3,1,ifelse(Eggs==3,2,ifelse(Eggs>3,3,NA))),
  `Sweetened beverages (Ex. coke, juice)`=ifelse(`Sweetened beverages (Ex. coke, juice)`<3,1,ifelse(`Sweetened beverages (Ex. coke, juice)`==3,2,ifelse(`Sweetened beverages (Ex. coke, juice)`>3,3,NA))),
  `Ice cream`=ifelse(`Ice cream`<3,1,ifelse(`Ice cream`==3,2,ifelse(`Ice cream`>3,3,NA))),
  Potatoes=ifelse(Potatoes<3,1,ifelse(Potatoes==3,2,ifelse(Potatoes>3,3,NA))),
  Cassava=ifelse(Cassava<3,1,ifelse(Cassava==3,2,ifelse(Cassava>3,3,NA))),
  `Instant noodles`=ifelse(`Instant noodles`<3,1,ifelse(`Instant noodles`==3,2,ifelse(`Instant noodles`>3,3,NA))),
  Chicken=ifelse(Chicken<3,1,ifelse(Chicken==3,2,ifelse(Chicken>3,3,NA))),
  `Lamb or beef`=ifelse(`Lamb or beef`<3,1,ifelse(`Lamb or beef`==3,2,ifelse(`Lamb or beef`>3,3,NA))),
  Cucumber=ifelse(Cucumber<3,1,ifelse(Cucumber==3,2,ifelse(Cucumber>3,3,NA))),
  `Imported vegetables`=ifelse(`Imported vegetables`<3,1,ifelse(`Imported vegetables`==3,2,ifelse(`Imported vegetables`>3,3,NA))),
  Papaya=ifelse(Papaya<3,1,ifelse(Papaya==3,2,ifelse(Papaya>3,3,NA))),
  Pandanus=ifelse(Pandanus<3,1,ifelse(Pandanus==3,2,ifelse(Pandanus>3,3,NA))),
  Milk=ifelse(Milk<3,1,ifelse(Milk==3,2,ifelse(Milk>3,3,NA))),
  `Chips or biscuits`=ifelse(`Chips or biscuits`<3,1,ifelse(`Chips or biscuits`==3,2,ifelse(`Chips or biscuits`>3,3,NA))),
  Cake=ifelse(Cake<3,1,ifelse(Cake==3,2,ifelse(Cake>3,3,NA))),
  #import versus domestic: whether interviewees eat more imported staple food, _staple (rice, instant noodle     
  # versus swamp taro, breadfruit), meat, _meat (pork, lamb/beef, chicken  versus fish); reference as domestic 
  #Define as imported dominant if they use the import more (2), balanced (1), domestic dominant (0)
  # More is defined as using one (or more) of the imported/domestic food item everyday
  # Define a binary variable for imported ==1, not==0
  food_imp_dom_staple=ifelse(`Instant noodles`==1|Rice==1,2,ifelse(`Swamp taro or taro`==1|Breadfruit==1,0,1)),
  food_imported_staple_c=factor(ifelse(food_imp_dom_staple==2,1,0)),
  food_imp_dom_meat=ifelse(Pork==1|Chicken==1|`Lamb or beef`==1,2,ifelse(Fish==1,0,1)),
  food_imported_meat_c=factor(ifelse(food_imp_dom_meat==2,1,0)),
  food_imported_veg_c=factor(ifelse(`Imported vegetables`<=2,1,0)),
  food_imported_fruit_c=factor(ifelse(`Imported fruits (Ex. apples, oranges, pears)`<=2,1,0)),
  food_imported_sweet_beverage_c=factor(ifelse(`Sweetened beverages (Ex. coke, juice)`<=2,1,0)),
  food_imported_ice_cream_c=factor(ifelse(`Ice cream`<=2,1,0)),
  food_imported_chip_biscuit_c=factor(ifelse(`Chips or biscuits`<=2,1,0)),
  #Diet change (https://stackoverflow.com/questions/46339538/dplyrcount-multiple-columns)
  #Increased intake since 2020
  increased_1=factor(`Q33-1`),
  increased_2=factor(`Q33-2`),
  increased_3=factor(`Q33-3`),
  decreased_1=factor(`Q34-1`),
  decreased_2=factor(`Q34-2`),
  decreased_3=factor(`Q34-3`),                
  #Behaviors
  #Smoking status, and yes/no
  smoking=factor(Q35),
  smoking_c=factor(ifelse(Q35==3,0,1)),#variable 'smoking' as dichotomous categorical (yes=1/no=0)
  shs_c=ifelse(Q36==2,0,1), #SHS exposure (yes=1/no=0)
  #variable 'alcohol' as 3-levels, and create dichotomous categorical (yes=1/no=0)
  alcohol=factor(Q37),
  alcohol_c=factor(ifelse(Q37==3,0,1)),
  #variable 'exercise' for categorical exercise habit (1 as highest)
  exercise=factor(Q38), 
  #Family garden (yes=1/no=0) and gov garden (1 as highest, defined as '1' in gov_garden_c)
  gov_garden=factor(Q39),
  gov_garden_c=factor(ifelse(Q39==1,1,0)),
  fam_garden=factor(ifelse(Q40==2,0,1)),
  #COVID questionaire
  covid_1=as.integer(Q41),
  covid_2=as.integer(Q42),
  covid_3=as.integer(Q43),
  covid_4=as.integer(Q44),
  covid_5=as.integer(Q45),
  covid_6=as.integer(Q46),
  covid_7=as.integer(Q47)
)

write_csv(tuvalu1, "clean_data_2022.csv")

#Get food item names
food_names <- names(tuvalu1[c(38:62)])

#Select adult only, and this is the dataset we are going to use in the following analysis (2022)
tuvalu2<-tuvalu1%>%filter(highschool==2)


# ######################Get descriptive data######################################

#Table 0. Demographics and health outcomes
#define continuous vars as allvars
contvars1<-c("age","ht","wt","wc","bmi")
#define categorical vars as catvars #The variables specified here must also be specified in the vars argument.
catvars1<-c("age_c","gender","region_c","exercise","smoking","education","employ","income_c","alcohol","marital","bmi_c","wc_h","fam_garden","gov_garden")
allvars1<-c(contvars1,catvars1)
table0<-CreateTableOne(vars=allvars1, data=tuvalu2, factorVars =catvars1, includeNA = TRUE)
tab0<-  print(table0, noSpaces=TRUE)
tab0<-  as.data.frame(cbind(rowname=rownames(tab0),tab0))

#Table 1: Diet response (Stratified)
table1<-CreateTableOne(vars=food_names, data=tuvalu2, factorVars = food_names, includeNA = TRUE)
tab1<-  print(table1, noSpaces=TRUE)
tab1<-  as.data.frame(cbind(rowname=rownames(tab1),tab1))

#Table 2:Diet response (Main versus outlying)
table2<-CreateTableOne(vars=food_names, data=tuvalu2,strata="region_c", factorVars = food_names, includeNA = TRUE)
tab2<-  print(table2, noSpaces=TRUE)
tab2<-  as.data.frame(cbind(rowname=rownames(tab2),tab2))

#export the tables to excel at working folder
write.xlsx(list("Table0. Demographics"=tab0,"Table 1. Food intake" = tab1, "Table 2. Food intake (islands)" = tab2), file = "Food_intake_Descriptive_analysis_T02.xlsx") 

#Correlation plot
#Complete case
tuvalu3<-tuvalu2[complete.cases(tuvalu2[,c(names(tuvalu2[c(38:62)]))]),]
#Correlation and plot
cor(tuvalu3[c(names(tuvalu2[c(38:62)]))])->a
corrplot(a, tl.pos='l')

# Version records##
# 20220915: Extract the code from original code set
# 20220924: Create variable list and code platform for Jerry
# 20221207: Make correlation plot 


#================= Dietary pattern analysis ===================================
tuv_diet <- tuvalu2 %>% dplyr::select(c(`Participant No.`, all_of(food_names)))
names(tuv_diet) <- c("Participant", "Rice", "Taro", "Breadfruit", "Fish", "Port", "Cabbage", 
                     "Bird_nest_fern", "Banana", "Coconut", "Imp_fruits", "Eggs",
                     "Sweetened_bevs", "Ice_cream", "Potatoes", "Cassava", 
                     "Instant_noodles", "Chicken", "Lamb_beef", "Cucumber", 
                     "Imp_vegs", "Papaya", "Pandanus", "Milk", 
                     "Chips_biscuits", "Cake")
tuv_diet_compl <- tuv_diet %>% drop_na() # complete cases only
diet_na_inds <- setdiff(tuv_diet$Participant, tuv_diet_compl$Participant) # dropped id's due to NAs
tuv_diet_compl <- tuv_diet_compl %>% dplyr::select(!(Participant))

# formula for basic LCA
f <- as.formula(paste0("cbind(", paste0(names(tuv_diet_compl), collapse=", "), ")~1") )

# Fit models with different numbers of patterns
# nclass = number of latent classes
# maxiter = max iterations to run for convergence
# nrep = number of times to estimate the model using diff start values, keeping model with max log-lik
set.seed(20221220)
lc1<-poLCA(f, data=tuv_diet_compl, nclass=1, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE) 
lc2<-poLCA(f, data=tuv_diet_compl, nclass=2, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE)
lc3<-poLCA(f, data=tuv_diet_compl, nclass=3, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE)
lc4<-poLCA(f, data=tuv_diet_compl, nclass=4, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE)
lc5<-poLCA(f, data=tuv_diet_compl, nclass=5, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE)
lc6<-poLCA(f, data=tuv_diet_compl, nclass=6, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE)
lc7<-poLCA(f, data=tuv_diet_compl, nclass=7, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE)
lc8<-poLCA(f, data=tuv_diet_compl, nclass=8, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE)
lc9<-poLCA(f, data=tuv_diet_compl, nclass=9, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE)
lc10<-poLCA(f, data=tuv_diet_compl, nclass=10, na.rm = FALSE, nrep=30, maxiter=3000, verbose=FALSE)


### Model diagnostics table
# function to obtain ABIC for a model
abic <- function(llik, N, npar) {
  return( (-2*llik) + ((log((N + 2) / 24)) * npar) )
}
# Obtain CAIC for a model
caic <- function(llik, N, npar) {
  return( (-2*llik) + npar * (1 + log(N)) )
}
# Calculate entropy. Closer to 1 indicates better class separation
entropy <- function(p) {
  plogp <- - p * log(p)
  plogp[is.na(plogp)] <- 0  # the limit of plogp is 0 by L'Hopital's
  return(sum(plogp))
}
entropy_R2 <- function(model) {
  error_prior <- entropy(model$P)
  error_post <- mean(apply(model$posterior, 1, entropy))
  return((error_prior - error_post) / error_prior)
}
# Calculate average max class membership probability across individuals
class_prob <- function(model) {
  return( mean(apply(model$posterior, 1, max)) )
}

# combining results to a dataframe
Model <- 1:10
BIC <- c(lc1$bic, lc2$bic, lc3$bic, lc4$bic, lc5$bic, lc6$bic, lc7$bic, lc8$bic, 
         lc9$bic, lc10$bic)
ABIC <- c(abic(lc1$llik, lc1$N, lc1$npar), abic(lc2$llik, lc2$N, lc2$npar),
          abic(lc3$llik, lc3$N, lc3$npar), abic(lc4$llik, lc4$N, lc4$npar),
          abic(lc5$llik, lc5$N, lc5$npar), abic(lc6$llik, lc6$N, lc6$npar),
          abic(lc7$llik, lc7$N, lc7$npar), abic(lc8$llik, lc8$N, lc8$npar),
          abic(lc9$llik, lc9$N, lc9$npar), abic(lc10$llik, lc10$N, lc10$npar))
CAIC <- c(caic(lc1$llik, lc1$N, lc1$npar), caic(lc2$llik, lc2$N, lc2$npar),
          caic(lc3$llik, lc3$N, lc3$npar), caic(lc4$llik, lc4$N, lc4$npar),
          caic(lc5$llik, lc5$N, lc5$npar), caic(lc6$llik, lc6$N, lc6$npar),
          caic(lc7$llik, lc7$N, lc7$npar), caic(lc8$llik, lc8$N, lc8$npar),
          caic(lc9$llik, lc9$N, lc9$npar), caic(lc10$llik, lc10$N, lc10$npar))
log_likelihood <- c(lc1$llik, lc2$llik, lc3$llik, lc4$llik, lc5$llik, lc6$llik,
                    lc7$llik, lc8$llik, lc9$llik, lc10$llik)
entropy <- c(entropy_R2(lc1), entropy_R2(lc2), entropy_R2(lc3), entropy_R2(lc4),
             entropy_R2(lc5), entropy_R2(lc6), entropy_R2(lc7), entropy_R2(lc8),
             entropy_R2(lc9), entropy_R2(lc10))
class_prob <- c(class_prob(lc1), class_prob(lc2), class_prob(lc3), class_prob(lc4),
                class_prob(lc5), class_prob(lc6), class_prob(lc7), class_prob(lc8),
                class_prob(lc9), class_prob(lc10))
results <- data.frame(Model, BIC, ABIC, CAIC, log_likelihood, entropy, class_prob)
which.min(BIC)  # K=8 corresponds to min BIC
export_table(results, format="html")


### Model diagnostics plot
# Order categories of results$model in order of appearance
results$Model <- as.factor(results$Model) 
results2 <- results %>% dplyr::select(-one_of(c("entropy", "class_prob")))
results2 <- tidyr::gather(results2, key="Metric", value="Value", -Model)
results2 %>% ggplot(aes(x=Model, y=Value)) +
  geom_point() + theme_bw() + 
  facet_grid(Metric ~ ., scales = "free")

### population shares of classes
sizes_lc4 <- round(prop.table(table(lc4$predclass)), 4)
sizes_lc5 <- round(prop.table(table(lc5$predclass)), 4)
sizes_lc6 <- round(prop.table(table(lc6$predclass)), 4)  # <10%
sizes_lc7 <- round(prop.table(table(lc7$predclass)), 4)  # <10%
sizes_lc8 <- round(prop.table(table(lc8$predclass)), 4)  # <5%
all_sizes <- list(sizes_lc4, sizes_lc5, sizes_lc6, sizes_lc7, sizes_lc8)
lapply(all_sizes, print)

# LMR Test
library(tidyLPA)
lrt <- function(null, alt) {
  calc_lrt(null$Nobs, null$llik, null$npar, length(null$P), 
           alt$llik, alt$npar, length(alt$P))
}
n <- nrow(tuv_diet_compl)
lrt(lc1, lc2)
lrt(lc2, lc3)
lrt(lc3, lc4)
lrt(lc4, lc5)
lrt(lc5, lc6)
lrt(lc6, lc7)
lrt(lc7, lc8)
lrt(lc8, lc9)
lrt(lc9, lc10)

# Standard output of conditional item response probablities
lc4

plot_pattern_probs <- function(model) {
  lcmodel <- reshape2::melt(model$probs, level=2)
  lcmodel %>%
    ggplot(aes(x = L2, y = value, fill = Var2)) + 
    geom_bar(stat = "identity", position = "stack") + 
    facet_grid(Var1 ~ .) + 
    scale_fill_brewer(type="seq", palette="Greys") + 
    theme_bw() + 
    labs(x = "Food items",y="Item consumption probabilities", 
         fill ="Item \nconsumption \nprobabilities") + 
    theme( axis.text.y=element_text(size=7),
           #axis.ticks.y=element_blank(),   
           panel.grid.major.y=element_blank(),
           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}
plot_pattern_probs(lc4)
plot_pattern_probs(lc5)
plot_pattern_probs(lc6)
plot_pattern_probs(lc7)


plot_pattern_modes <- function(model) {
  est_item_probs <- model$probs
  mode_item_probs <- lapply(est_item_probs, function(x) apply(x, 1, which.max))
  mode_item_probs <- as.data.frame(do.call("rbind", mode_item_probs))
  mode_item_probs$Item <- fct_rev(factor(rownames(mode_item_probs), 
                                         levels=rownames(mode_item_probs)))
  mode_plot <- mode_item_probs %>% gather("Class", "Level", -Item) 
  mode_plot %>% ggplot(aes(x=Class, y=Item, fill=Level)) + 
    geom_tile(color="black") + 
    geom_text(aes(label = Level), col="white", cex=2) +
    scale_fill_gradient(trans="reverse") + 
    theme(legend.position="none")
}

plot_pattern_modes(lc4)
plot_pattern_modes(lc5)
plot_pattern_modes(lc6)
plot_pattern_modes(lc7)


#============================ proceeding with 4 patterns =======================
set.seed(3)
lc4<-poLCA(f, data=tuv_diet_compl, nclass=4, na.rm = FALSE, nrep=30, 
           maxiter=3000, verbose=FALSE)
plot_pattern_modes(lc4)
### population shares of classes
round(prop.table(table(lc4$predclass)), 4)
entropy_R2(lc4)
class_prob(lc4)

indiv_class <- lc4$predclass
post_probs <- lc4$posterior
post_probs <- round(post_probs, 4)
modal_probs <- apply(post_probs, 1, max)
summary(modal_probs)  ## low misclassification error
sort(modal_probs)[1:50]  ## lowest confidence predictions

#================= Examine variables ===========================================
tuvalu4 <- tuvalu2[!(tuvalu2$`Participant No.` %in% diet_na_inds), ]
tuvalu4$latent_class <- factor(indiv_class)
tuvalu4$obesity_1 <- factor(tuvalu4$obesity_1, levels=c(0,1))
tuvalu4$obesity_3 <- factor(tuvalu4$obesity_3, levels=c(0,1))
hist(tuvalu4$age)
hist(tuvalu4$income, breaks=30)
summary(tuvalu4$income)
table(tuvalu4$income)


#================= Demographic cross tabulations ===============================
library(sjPlot)
library(flextable)
library(tableone)
library(officer)
library(kableExtra)
table(tuvalu4$obesity_1)
table(tuvalu4$obesity_3)
hist(tuvalu4$wc, breaks= 20)

## Alternative single-variable cross-tabulation table and plot
# tab_xtab(var.row = tuvalu4$region_c, var.col = tuvalu4$latent_class, 
#          title = "Demographic Cross-Tabulation", show.row.prc = TRUE)
# plot_xtab(tuvalu4$region_c, tuvalu4$latent_class, 
#           margin = "row", bar.pos = "stack", coord.flip = TRUE)


# 'create_demog_table' creates a table of demographic characteristics using 
# only the data in the specified dataset(s)
# Input: dataset, column_names, row_names
# Output: word document including formatted demographic table 
create_demog_table <- function(dataset, column_names, row_names) {
  # Create table of demographic comparisons stratified by malaria status
  lc_demog <- CreateTableOne(vars = c("gender", "age", "education_c", "region_c",
                                      "ncd", "smoking_c", "income", "exercise"),
                                  factorVars = c("gender", "education_c", "region_c",
                                                 "ncd", "smoking_c", "exercise"),
                                  strata = "latent_class", addOverall = T,
                                  data = dataset)
  lc_demog_tab <- print(lc_demog, noSpaces = TRUE, nonnormal = c("age", "income"))
  #lc_demog_tab <- print(lc_demog, noSpaces = TRUE)
  
  lc_demog_tab <- as.data.frame(lc_demog_tab)[, c(2:5, 1, 6)]
  colnames(lc_demog_tab) <- column_names
  rownames(lc_demog_tab) <- row_names
        # lc_demog_tab <- print(lc_demog, noSpaces = TRUE, showAllLevels = TRUE)
        # lc_demog_tab <- lc_demog_tab[,1:7] 
        # print(lc_demog_tab) %>% kbl %>% kable_paper("hover")
  
  # Convert to table
  table <- flextable(lc_demog_tab %>% rownames_to_column("Feature"))
  table <- align(table, align = "left", part="all")
  table <- width(table,width=0.95)
  # Export to word
  doc <- read_docx()
  doc <- body_add_flextable(doc, value = table)
  #table_name <- paste0('Table_LC_Demog.docx')
  table_name <- paste0('Table_LC_Demographics.docx')
  docx <- print(doc, target = table_name)
}

create_demog_table(tuvalu4, 
                   column_names = c("Plant-Based", "Mixed", "Limited", 
                                    "Imported", "Overall", "P-value"),
                   row_names = c("Sample size", "Sex: Female (%)", 
                                 "Age (median [IQR])", "Education: > HS (%)",
                                 "Region: outlying (%)", "NCD: Reported (%)", 
                                 "Smoking: Yes (%)", "Income (median [IQR])", 
                                 "Exercise (%)", "   High", "   Med", "   Low"))


create_outcomes_table <- function(dataset, strat_var, column_names, row_names,
                                  table_name) {
  # Create table of demographic comparisons stratified by malaria status
  lc_demog <- CreateTableOne(vars = c("gender", "age", "education_c", "region_c",
                                      "ncd", "smoking_c", "income", "exercise",
                                      "latent_class"),
                             factorVars = c("gender", "education_c", "region_c",
                                            "ncd", "smoking_c", "exercise",
                                            "latent_class"),
                             strata = strat_var, addOverall = T,
                             data = dataset)
  lc_demog_tab <- print(lc_demog, noSpaces = TRUE, nonnormal = c("age", "income"))
  lc_demog_tab <- as.data.frame(lc_demog_tab)[, c(2:3, 1, 4)]
  colnames(lc_demog_tab) <- column_names
  rownames(lc_demog_tab) <- row_names

  # Convert to table
  table <- flextable(lc_demog_tab %>% rownames_to_column("Feature"))
  table <- align(table, align = "left", part="all")
  table <- width(table,width=1.5)
  # Export to word
  doc <- read_docx()
  doc <- body_add_flextable(doc, value = table)
  docx <- print(doc, target = table_name)
}

outcome_row_names <- c("Sample size", "Sex: Female (%)", 
                       "Age (median [IQR])", "Education: > HS (%)",
                       "Region: outlying (%)", "NCD: Reported (%)", 
                       "Smoking: Yes (%)", "Income (median [IQR])", 
                       "Exercise (%)", "   High", "   Med", "   Low",
                       "Latent Class", "   Plant-Based", "   Mixed", 
                       "   Limited", "   Imported")
create_outcomes_table(tuvalu4, strat_var = "obesity_1",
                   column_names = c("Not Obese", "Obese (BMI >= 30)", 
                                    "Overall", "P-value"),
                   row_names = outcome_row_names,
                   table_name = "Table_LC_Outcomes.docx")
create_outcomes_table(tuvalu4, strat_var = "obesity_3",
                      column_names = c("Not Morbidly Obese", 
                                       "Morbidly Obese (BMI >= 40)", 
                                       "Overall", "P-value"),
                      row_names = outcome_row_names,
                      table_name = "Table_LC_Outcomes_Morbid.docx")


#================= Regression model relating diet to obesity ===================
library(lme4)
tuvalu4$age_center <- tuvalu4$age - mean(tuvalu4$age, na.rm = TRUE)
tuvalu4$latent_class <- factor(indiv_class, levels = c(2,1,3,4))
#tuvalu4$latent_class <- factor(indiv_class, levels = c(1,2,3,4))

# don't include gender since no association
# don't include income
## OBESITY
fit_ob1 <- glmer(obesity_1 ~ latent_class + age_center + education_c + 
                  smoking_c + exercise + ncd + (1|region_c), data = tuvalu4, 
                 family = binomial, nAGQ = 10)  # higher nAGQ -> higher accuracy
summary(fit_ob1)
res_ob1 <- summary(fit_ob1)
OR_ob1 <- data.frame(exp(res_ob1$coefficients[,1]))
colnames(OR_ob1) <- c("Cond'l OR")
OR_ob1

## MORBID OBESITY
fit_ob3 <- glmer(obesity_3 ~ latent_class + age_center + education_c + 
                   smoking_c + exercise + ncd + (1|region_c), data = tuvalu4, 
                 family = binomial, nAGQ = 10)  # higher nAGQ -> higher accuracy
summary(fit_ob3)
res_ob3 <- summary(fit_ob3)
OR_ob3 <- data.frame(exp(res_ob3$coefficients[,3]))
colnames(OR_ob3) <- c("Cond'l OR")
OR_ob3

## WAIST CIRCUMFERENCE
fit_wc <- lmer(wc ~ latent_class + age_center + education_c + 
                   smoking_c + exercise + ncd + (1|region_c), data = tuvalu4)  
summary(fit_wc)

### CHECK MORBID OBESITY CODING
# ============ Fit using Bayesian hierarchical modeling ========================
library(rstanarm)
options(mc.cores = 4)
fit_ob1 <- stan_glmer(obesity_1 ~ latent_class + gender + age_center + education_c + 
                        smoking_c + exercise + ncd + (1|region_c), data = tuvalu4, 
                      family = binomial, adapt_delta = 0.99) 
summary(fit_ob1)
OR_ob1 <- data.frame(exp(fit_ob1$coefficients))
colnames(OR_ob1) <- c("Cond'l OR")
OR_ob1
posterior_interval(fit_ob1, prob = 0.95)

fit_ob3 <- stan_glmer(obesity_3 ~ latent_class + gender + age_center + education_c + 
                      smoking_c + exercise + ncd + (1|region_c), data = tuvalu4, 
                      family = binomial, adapt_delta = 0.999) 
summary(fit_ob3)
posterior_interval(fit_ob3, prob = 0.95)
OR_ob3 <- data.frame(exp(fit_ob3$coefficients))
colnames(OR_ob3) <- c("Cond'l OR")
OR_ob3

fit_wc <- stan_glmer(wc ~ latent_class + gender + age_center + education_c + 
                 smoking_c + exercise + ncd + (1|region_c), data = tuvalu4,
                 adapt_delta = 0.999)  
summary(fit_wc)
posterior_interval(fit_wc, prob = 0.95)


#================= Miscellaneous code ============================================
## Alternative code for changing levels of the factors
#Get food item column names
food_names <- names(tuvalu[c(38:62)])

# Function to recode factors to have 3 categories
refactor <- function(column) {
  recode_factor(column, `1` = 1, `2` = 1, `3` = 2, `4` = 3, `5` = 3)
}

# Apply recoding to all food item columns
tuvalu_refactor <- mutate_at(tuvalu, food_names, refactor)



tuv_adults <- tuvalu%>% 
  mutate(
    #Create year identifier
    year="2022",
    #Create combine indicator "yearid"= year+study ID
    yearid=paste(year,`Participant No.`),
    #studyid as categorical one, each subject should have unique ID
    studyid=factor(`Participant No.`),
    #Study region and date of interview
    region=factor(`Interview Region`),                                                                                 
    # High school student: 0 as Funafuti (interview region 10), 1 as Motufoua High School (interview region 11), 2 as adults 
    highschool= case_when(
      region == 10 ~ 0,
      region == 11 ~ 1,
      TRUE ~ 2
    )
  ) %>% 
  filter(highschool==2)
table4 <- CreateTableOne(vars=food_names, data=tuv_adults, factorVars = food_names, includeNA = TRUE)
tab4 <- print(table4, noSpaces=TRUE)
tab4 <- as.data.frame(cbind(rowname=rownames(tab4), tab4))