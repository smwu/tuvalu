
# install the package needed, 'install.packages("")' for installation
# use library to call them 
library(dplyr)
library(readxl)
library(lubridate)
library(tableone)
#Output file to excel
library(openxlsx)
#Graph-related
library(ggplot2)
library(ggpubr)
library(corrplot)
library(igraph)
library(reshape2)
#Function for regression models
library(finalfit)
#Factor analysis: Which is somehow theory-based
library(psych)
library(gt)
library(lavaan)
library(MASS)
#PCA: Linear combination of variables

#Put the functions here
#Create functions to extract estimate, CI and p-values
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
linear_coef<-function(b){
  cbind(names(b$coefficients[,1]),paste(specify_decimal(b$coefficients[,1],2)," (",specify_decimal(b$coefficients[,1]+qnorm(0.025)*b$coefficients[,2],2),"-",specify_decimal(b$coefficients[,1]+qnorm(c(.975))*b$coefficients[,2],2),", p=",specify_decimal(b$coefficients[,4],3),")",sep=""))
}
logistic_coef<-function(b){
  cbind(names(b$coefficients[,1]),paste(specify_decimal(exp(b$coefficients[,1]),2)," (",specify_decimal(exp(b$coefficients[,1]+qnorm(0.025)*b$coefficients[,2]),2),"-",specify_decimal(exp(b$coefficients[,1]+qnorm(c(.975))*b$coefficients[,2]),2),", p=",specify_decimal(b$coefficients[,4],3),")",sep=""))
}

#Set up working directory
#setwd("G:/My Drive/tuvalu/Results/work/T00/20221017")

#Dataset manipulation: R is case-sensitive, so we use lower case for all variables
#import the file, we read the second sheet, which was transposed successfully by the team in Tuvalu (,2) for second sheet
tuvalu20<-read_excel("Questionnaire-data-20200624_550-cases.xlsx")
tuvalu<-read_excel("2022-Nutrition-Survey_20220530.xlsx")
#use pipeline in package 'dplyr' to clean data; mutate the names of variables
#For 2020 data
#Indicator for those with same responses in knowledge questions
tuvalu20$kledg_exclude<-ifelse(apply(tuvalu20[, 45:59], 1, function(i) length(unique(i)) > 1)==TRUE,0,1)
#use pipeline in package 'dplyr' to clean data; mutate the names of variables
tuvalu20_1<-tuvalu20%>%mutate(
  #Create year identifier
  year="2020",
  #Create combine indicator "yearid"
  yearid=paste(year,`4`),
  #studyid as categorical one, each subject should have unique ID
  studyid=factor(`4`),
  #Study region and date of interview
  region=ifelse(`1`==3,10,ifelse(`1`==4,11,1)),
  #Main island as 0 (both students and high schoolers), 1 as other islands
  region_c=factor(ifelse(region%in%c(1,10),0,1)),
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
  interview_date= ymd(`2`),
  #body measurements
  #Height (cm): Exclude extreme values (>250 or <80 cm)
  ht=as.numeric(`17a`),
  #Weight: Exclude extreme values (<0 kg)
  wt=as.numeric(`17b`),
  bmi=10000*wt/(ht)^2,
  bmi=ifelse(bmi>100,NA,bmi),
  #Categorize for overweight and obesity with conventional cutpoint (<25 as 1, 25-30 as 2, and >30 as 3)
  bmi_c= cut(bmi, c(0, 25, 30 ,35 ,40 , Inf),
    include.lowest = TRUE),
  obesity_1=ifelse(bmi>=30,1,0),
  obesity_2=ifelse(bmi>=35,1,0),
  obesity_3=ifelse(bmi>=40,1,0),
  #Waist circumference: Exclude extreme values (<0 cm) 
  wc=as.numeric(`17d`),
  #demographics
  #Gender of subjects: Male=1, Female=2
  gender=factor(`6`),
  #Use gender and wc to define higher waist circ. (Men: >90, female: >80)
  wc_h= case_when(
    gender==1 & wc>90 ~ 1,
    gender==2 & wc>80 ~ 1,
    TRUE ~ 0),
  #age in years
  age=ifelse((2020-as.numeric(`7`))>150,NA,2020-as.numeric(`7`)),
  age_c=cut(age, c(0,30,40,50,60,70,Inf)),
  #Marital status
  marital=factor(`8`),
  #Highest education level and construct education level, dichotomized into lower=0 (level 1,2,3; to secondary school) higher=1 (tertiary)
  education=factor(`10`),
  education_c=ifelse(`10`==4,1,0),
  #Tuvalu citizenship (if==2 then end interview) and residence before 18 y/o
  citizen=factor(`5`),
  #There's some difference between Vaitupu as 5 	
  region_before18=case_when(`13a`==2~5,
	`13a`==3~2,
	`13a`==4~3,
	`13a`==5~4,
	TRUE~`13a`
	),
  #How long do we live in current region (not asked in 2020)
  #In 2020, only asked Do you have a job?, so put 1. Regular or fixed work;; 3. No or students
  employ=ifelse(`14a`==2,3,`14a`),
  #This one is also slightly different
  #construct income, dichotomized into lower=0 (level 1,2,3) and higher=1 (others); considering annual income at 1000 USD, make high versus low at 1000 AUD
  income_c=ifelse(`16`>3,1,0),
  #Self-reported medical history
  #hypertension
  htn=factor(`18a`),
  #dyslipidemia
  dl=factor(`18b`),
  #Diabetes
  dm=factor(`18c`),
  # NCD defined as HTN or DM or DL
  ncd=ifelse(htn==1|dl==1|dm==1,1,0),
  #Knowledge questions
  #The correct answers are: 1211122 12121212, remove those with same response levels
  #We change the values "kledg_" to correct (1) or not (0, included the unknowns)
  kledg_1=ifelse(`19`==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_2=ifelse(`20`==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_3=ifelse(`21`==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_4=ifelse(`22`==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_5=ifelse(`23`==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_6=ifelse(`24`==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_7=ifelse(`25`==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_8=ifelse(`26`==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_9=ifelse(`27`==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_10=ifelse(`28`==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_11=ifelse(`29`==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_12=ifelse(`30`==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_13=ifelse(`31`==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_14=ifelse(`32`==1,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_15=ifelse(`33`==2,1,ifelse(kledg_exclude==1,NA,0)),
  kledg_sum=ifelse(kledg_exclude==1,NA,kledg_1+kledg_2+kledg_3+kledg_4+kledg_5+kledg_6+kledg_7+kledg_8+kledg_9+kledg_10+kledg_11+kledg_12+kledg_13+kledg_14+kledg_15),
  kledg_h=ifelse(kledg_sum>median(kledg_sum,na.rm = TRUE),1,ifelse(kledg_exclude==1,NA,0)),
  #Food frequency items
  #Keep names as they are (original dataset 38:62, 25 items)
  #Diet change (https://stackoverflow.com/questions/46339538/dplyrcount-multiple-columns)
  #Diet change not asked, and questions for K&A are different, so not put in           
  #Behaviors (Dietary): 
	#Frequency (2020): 1. Everyday/Aso katoa; 2. Weekly but not every day/Fakatasi ite vaiaso kae se ko aso katoa; 3. Monthly but not every week/Fakatasi ite masina kae se ko vaiaso katoa; 4. Less than once per month/Muata kae fakatasi aka loa ite masina 
	#Frequency (2022): 1. Almost EVERY DAY; 2. Several time a WEEK; 3. Several times a MONTH; 4. LESS than once a MONTH; 5. NEVER
 `Rice`=`46f-Rice`,
 #Only asked in 2020: `46a-Chocolate or candies` ,`46b-Cucumber`,`46b-Carrot`,`46f-Noodles (exclude Instant noodles)`,`46e-Soymilk or tofu`,`46f-Toast`,`46f-Cereal`,`46g-Raw dishes (i.e vegetables or meat)`,`46g-Roasted dishes`,`46g-Fried dishes` ,`46g-Boiled dishes`,`46g-Steamed dishes` 
 #Only asked in 2022: `Swamp taro or taro`,`Breadfruit`,  `Bird's-nest fern (Lauru)`, `Coconut`, `Ice cream`, `Potatoes`,`Cassava`, `Papaya`, `Cake`                                  
 `Fish`=`46d-Seafood (Including fish)`,
 `Pork`=`46d-Pork`,                                        
 `Cabbage`=`46b-Cabbage`,
 `Banana`= `46c-Bananas`,                                       
 #Need to discuss: 
 #In 2020, asked three items `46c-Apples`   `46c-Oranges`  `46c-Pears`
 #In 2022, asked `Imported fruits (Ex. apples, oranges, pears)`=,
 #I suggest using the highest level of the three in 2020 to represent Imported fruits
 `Eggs`=`46e-Eggs`,                                        
 `Sweetened beverages (Ex. coke, juice)`=`46a-Carbonated drinks`,                                           
 `Instant noodles`=`46a-Instant noodles`,
 `Chicken`=`46d-Chicken`,                                     
 #2022 asked `Lamb or beef`, 2020 only asked `46d-Beef`
 `Lamb or beef`= `46d-Beef`,
 `Cucumber`= `46b-Cucumber`,                                    
 `Imported vegetables`=`46b-Frozen and canned vegetables`,
 `Pandanus`=`46c-Pandanus`,
 `Milk`=`46e-Milk`,                                        
 `Chips or biscuits`=`46a-Chips and biscuits`,
  #Smoking status as yes/no
  smoking_c=ifelse(`49`==1,0,1),#variable 'smoking' as dichotomous categorical (yes=1/no=0)
  #shs_c=ifelse(Q36==2,0,1), #SHS exposure (yes=1/no=0) not asked in 2020
  #variable 'alcohol' as 3-levels, and create dichotomous categorical (yes=1/no=0)
  alcohol_c=ifelse(`50`==1,0,1),
  #variable 'exercise' for categorical exercise habit (1 as highest)
  # Need to discuss: There are differences in Q38 (2022) AND 51 in 2020
	#exercise=factor(Q38)	
)
#For 2022 data
#Indicator for those with same responses in knowledge questions
tuvalu$kledg_exclude<-ifelse(apply(tuvalu[, 19:28], 1, function(i) length(unique(i)) > 1)==TRUE,0,1)
tuvalu1<-tuvalu%>%mutate(
  #Create year identifier
  year="2022",
  #Create combine indicator "yearid"
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
  #Keep names as they are (original dataset 38:62, 25 items) exccept for several one with special chars
  "birdnest"=as.numeric(bird_nest_fern),
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
#Create new names for duplicated variables
dichoto_diet<-NA
for(x in 1:25){dichoto_diet[x]=paste(names(tuvalu1[(x+37)]),"_c",sep="")}
#Create duplicate diet data, and rename
tuvalu1[c(38:62)]->tuvalu1_temp
#Convert dietary frequency into frequent (more than weekly) versus non-frequent, Make them factors
for (x in 1:25) {tuvalu1_temp[x]<-ifelse(tuvalu1_temp[x]<=2,1,0)}
tuvalu1_temp<-tuvalu1_temp%>%mutate_all(.,factor)
names(tuvalu1_temp)<-dichoto_diet
#Combine the dataset with the duplicated (food items)
tuvalu1<-cbind(tuvalu1,tuvalu1_temp)
#Create daily consumption for rice and fishes
tuvalu1<-tuvalu1%>%mutate(Rice_daily=ifelse(Rice==1,1,0),
                          Fish_daily=ifelse(Fish==1,1,0))
#Merge the two datasets
#get a list of shared names 
shared_names<-names(tuvalu1)[names(tuvalu1)%in%names(tuvalu20_1)]
#Categorical variables and continuous variables (exclude studyid)
cat_names<-shared_names[-c(18,19,25,26,27,28,33,36)]
cont_names<-shared_names[c(26,27,28,33,36)]
#List of food intake
food_2022<-cat_names[1:15]
#Exclude ID, population density and interview date from categorical variables 
catvars<-cat_names[-c(20)]
allvars<-c(catvars,cont_names)
#Subset to shared names, change to same types for shared names
tuvalu1_sub<-tuvalu1[,shared_names]%>%mutate_at(cat_names, as.factor)%>%mutate_at(cont_names, as.numeric)
tuvalu20_1_sub<-tuvalu20_1[,shared_names]%>%mutate_at(cat_names, as.factor)%>%mutate_at(cont_names, as.numeric)
#Create merged dataset
tuvalu202022<-full_join(tuvalu1_sub,tuvalu20_1_sub)
#Select adult only
tuvalu202022_adult<-tuvalu202022%>%filter(highschool==2)
tuvalu202022_adolescent<-tuvalu202022%>%filter(highschool!=2)
#Select adult only (2022)
tuvalu2<-tuvalu1%>%filter(highschool==2)
# ######################Get descriptive data######################################
# ######################Get descriptive data######################################
# ################################################
# 2020 AND 2022 combined dataset (Adult/Teenagers)
# ################################################
#Compare demographics in 2020 and 2022
###### Descriptive data by survey year/wave
#Table 1: Adult demographics
table1<-CreateTableOne(vars=allvars, data=tuvalu202022_adult,strata="year", factorVars = catvars, includeNA = FALSE)
tab1<-  print(table1, noSpaces=TRUE)
tab1<-  as.data.frame(cbind(rowname=rownames(tab1),tab1))

#Table 2:Adolescent demographics
table2<-CreateTableOne(vars=allvars, data=tuvalu202022_adolescent,strata="year", factorVars = catvars, includeNA = FALSE)
tab2<-  print(table2, noSpaces=TRUE)
tab2<-  as.data.frame(cbind(rowname=rownames(tab2),tab2))

#export the tables to excel
write.xlsx(list("Table 1. Adult" = tab1, "Table 2. Teenagers" = tab2), file = "Descriptive_data_2020_2022_by_survey_year.xlsx") #NOT SHOWING THE ROW NAMES!

# Regression models (Table 2)

#This will be the analysis for age group and obesity prevalence.
#unadjusted model
glm_food_import_un_obesity1<-glm(obesity_1~age_c, data=tuvalu202022_adult, family=binomial(link = "logit"))
glm_food_import_un_obesity3<-glm(obesity_3~age_c, data=tuvalu202022_adult, family=binomial(link = "logit"))
#Adjusted models
glm_food_import_obesity1<-glm(obesity_1~age_c+smoking_c+gender+education_c+year, data=tuvalu202022_adult, family=binomial(link = "logit"))
glm_food_import_obesity3<-glm(obesity_3~age_c+smoking_c+gender+education_c+year, data=tuvalu202022_adult, family=binomial(link = "logit"))
#use the functions to extract OR and CIs, save to working directory
write.xlsx(list("Obesity"=rbind(logistic_coef(summary(glm_food_import_un_obesity1)),logistic_coef(summary(glm_food_import_obesity1))),"Severe obesity"=rbind(logistic_coef(summary(glm_food_import_un_obesity3)),logistic_coef(summary(glm_food_import_obesity3)))),"Regresion_obesity_age.xlsx")

##### TESTING CODE
tuv_na <- na.omit(tuvalu202022_adult[,c('age', 'obesity_1', 'age_c', 'bmi',
                                        'gender', 'region', 'region_c', 'year', 
                                        'alcohol_c', 'smoking_c', 'obesity_2',
                                        'marital', 'education', 'education_c', 
                                        'employ', 'income_c')])
table(tuv_na$year)
tuv_na$region <- factor(tuv_na$region, levels=1:9)
tuv_na$year <- relevel(tuv_na$year, "2020")
tuv_na$age_cent <- tuv_na$age - mean(tuv_na$age)
tuv_na$age2 <- tuv_na$age_cent^2
tuv_na$age3 <- tuv_na$age_cent^3

tuv_na2020 <- tuv_na %>% filter(year=="2020")
tuv_na2022 <- tuv_na %>% filter(year=="2022")

##### Univariate analysis to find variables associated with obesity ############
assoc_df <- data.frame(x=c("Gender", "Year", "Alcohol", "Smoking", "Income", 
                           "Marital", "Education", "Employment", 
                           "Age", "Age Categ"), 
                       y=rep("Obesity", times=10),
                       p_chi = rep(NA, times=10),
                       p_fish = rep(NA, times=10),
                       p_t = rep(NA, times=10))

## Binary variables
print("Gender ~ Obesity: significant")
chisq.test(tuv_na$gender, tuv_na$obesity_1)
fisher.test(tuv_na$gender, tuv_na$obesity_1)
assoc_df[assoc_df$x=="Gender",'p_chi'] <- chisq.test(tuv_na$gender, tuv_na$obesity_1)$p.value
assoc_df[assoc_df$x=="Gender",'p_fish'] <- fisher.test(tuv_na$gender, tuv_na$obesity_1)$p.value

print("Year ~ Obesity: not significant")
chisq.test(tuv_na$year, tuv_na$obesity_1)
fisher.test(tuv_na$year, tuv_na$obesity_1)
assoc_df[assoc_df$x=="Year",'p_chi'] <- chisq.test(tuv_na$year, tuv_na$obesity_1)$p.value
assoc_df[assoc_df$x=="Year",'p_fish'] <- fisher.test(tuv_na$year, tuv_na$obesity_1)$p.value

print("Alcohol ~ Obesity: highly significant")
chisq.test(tuv_na$alcohol_c, tuv_na$obesity_1)
fisher.test(tuv_na$alcohol_c, tuv_na$obesity_1)
assoc_df[assoc_df$x=="Alcohol",'p_chi'] <- chisq.test(tuv_na$alcohol_c, tuv_na$obesity_1)$p.value
assoc_df[assoc_df$x=="Alcohol",'p_fish'] <- fisher.test(tuv_na$alcohol_c, tuv_na$obesity_1)$p.value

print("Smoking ~ Obesity: significant")
chisq.test(tuv_na$smoking_c, tuv_na$obesity_1)
fisher.test(tuv_na$smoking_c, tuv_na$obesity_1)
assoc_df[assoc_df$x=="Smoking",'p_chi'] <- chisq.test(tuv_na$smoking_c, tuv_na$obesity_1)$p.value
assoc_df[assoc_df$x=="Smoking",'p_fish'] <- fisher.test(tuv_na$smoking_c, tuv_na$obesity_1)$p.value

print("Income ~ Obesity: significant")
chisq.test(tuv_na$income_c, tuv_na$obesity_1)
fisher.test(tuv_na$income_c, tuv_na$obesity_1)
assoc_df[assoc_df$x=="Income",'p_chi'] <- chisq.test(tuv_na$income_c, tuv_na$obesity_1)$p.value
assoc_df[assoc_df$x=="Income",'p_fish'] <- fisher.test(tuv_na$income_c, tuv_na$obesity_1)$p.value

## Categorical variables
print("Marital ~ Obesity: significant")
chisq.test(tuv_na$marital, tuv_na$obesity_1)
table(tuv_na$marital, tuv_na$obesity_1)  # okay for chi-sq
assoc_df[assoc_df$x=="Marital",'p_chi'] <- chisq.test(tuv_na$marital, tuv_na$obesity_1)$p.value

print("Education ~ Obesity: significant")
chisq.test(tuv_na$education, tuv_na$obesity_1)
chisq.test(tuv_na$education_c, tuv_na$obesity_1)  # not significant
assoc_df[assoc_df$x=="Education",'p_chi'] <- chisq.test(tuv_na$education, tuv_na$obesity_1)$p.value

print("Employment ~ Obesity: significant")
chisq.test(tuv_na$employ, tuv_na$obesity_1)
assoc_df[assoc_df$x=="Employment",'p_chi'] <- chisq.test(tuv_na$employ, tuv_na$obesity_1)$p.value

## Continuous variables
print("Age ~ Obesity: highly significant")
t.test(tuv_na$age~tuv_na$obesity_1)
chisq.test(tuv_na$age_c,tuv_na$obesity_1)
assoc_df[assoc_df$x=="Age Categ",'p_chi'] <- chisq.test(tuv_na$age_c, tuv_na$obesity_1)$p.value
assoc_df[assoc_df$x=="Age",'p_t'] <- t.test(tuv_na$age~tuv_na$obesity_1)$p.value

library(knitr)
library(kableExtra)
kable(assoc_df, format = "rst", digits=3)

# Plots
library(ggpubr)

p1 <- tuv_na %>% 
  ggplot(aes(x=gender, y=bmi, fill=year)) +
  geom_boxplot()
p2 <- tuv_na %>% 
  ggplot(aes(x=year, y=bmi, fill=year)) +
  geom_boxplot()
p3 <- tuv_na %>% 
  ggplot(aes(x=alcohol_c, y=bmi, fill=year)) +
  geom_boxplot()
p4 <- tuv_na %>% 
  ggplot(aes(x=smoking_c, y=bmi, fill=year)) +
  geom_boxplot()
p5 <- tuv_na %>% 
  ggplot(aes(x=income_c, y=bmi, fill=year)) +
  geom_boxplot()
p6 <- tuv_na %>% 
  ggplot(aes(x=marital, y=bmi, fill=year)) +
  geom_boxplot()
p7 <- tuv_na %>% 
  ggplot(aes(x=education, y=bmi, fill=year)) +
  geom_boxplot()
p8 <- tuv_na %>% 
  ggplot(aes(x=employ, y=bmi, fill=year)) +
  geom_boxplot()
p9 <- tuv_na %>% 
  ggplot(aes(x=age_c, y=bmi, fill=year)) +
  geom_boxplot()
p10 <- tuv_na %>% ggplot(aes(x=age, y=bmi, col=year)) +
  geom_point() + geom_smooth(method="lm", formula=y~poly(x,2))
ggarrange(p1, p2, p3, p4, p5, p6, common.legend = TRUE, nrow=2, ncol=3)
ggarrange(p7, p8, p9, p10, common.legend = TRUE, nrow=2, ncol=2)


##### Regression analyses ####################################################

## Linear regression w/ bmi
# alcohol, all age terms, gender, income, marital-divorced
summary(lm(bmi ~ alcohol_c * age_cent+ age2+age3+ gender + smoking_c + income_c + marital +
             education + employ, data=tuv_na))
logistic_coef(summary(lm(bmi ~ alcohol_c * age_cent+ age2+age3+ gender + smoking_c + income_c + marital +
                           education + employ, data=tuv_na)))

## Contrasts comparing each age category to the previous (e.g., 40-50 vs. 30-40)
my.backward.diff <- matrix(c(-5/6, -4/6, -3/6, -2/6, -1/6,
                             1/6, -4/6, -3/6, -2/6, -1/6,
                             1/6, 2/6, -3/6, -2/6, -1/6,
                             1/6, 2/6, 3/6, -2/6, -1/6,
                             1/6, 2/6, 3/6, 4/6, -1/6,
                             1/6, 2/6, 3/6, 4/6, 5/6), ncol=5, byrow=TRUE)
contrasts(tuv_na$age_c) <- my.backward.diff
summary(lm(bmi ~ alcohol_c + age_c, data=tuv_na))
logistic_coef(summary(lm(bmi ~ alcohol_c + age_c, data=tuv_na)))

## Logistic regression using obesity
## Minimally adjusted model: age and alcohol both significant
summary(glm(obesity_1 ~ alcohol_c + age_c, data=tuv_na2020, family=binomial())) # AIC 364
summary(glm(obesity_1 ~ alcohol_c + age_c, data=tuv_na2022, family=binomial())) # AIC 1128
# OR scale
min_coef2020 <- logistic_coef(summary(glm(obesity_1 ~ alcohol_c + age_c, data=tuv_na2020, family=binomial())))
min_coef2022 <- logistic_coef(summary(glm(obesity_1 ~ alcohol_c + age_c, data=tuv_na2022, family=binomial())))
min_glm <- as.data.frame(cbind(min_coef2020, min_coef2022[,-1]))
colnames(min_glm) <- c("Coefficient", "2020", "2022")
kable(min_glm, format="rst", digits=3)

## Fully adjusted model
# age, marital-married, education-high school, employment-none
full_coef2020 <- logistic_coef(summary(glm(obesity_1 ~ alcohol_c + age_c + gender + smoking_c + income_c + marital +
      education + employ, data=tuv_na2020, family=binomial())))  # AIC 359
full_coef2022 <- logistic_coef(summary(glm(obesity_1 ~ alcohol_c + age_c + gender + smoking_c + income_c + marital +
                            education + employ, data=tuv_na2022, family=binomial())))  # AIC 1118
full_glm <- as.data.frame(right_join(as.data.frame(full_coef2020), 
                                     as.data.frame(full_coef2022), by="V1"))
colnames(full_glm) <- c("Coefficient", "2020", "2022")
kable(full_glm, format="rst", digits=3)


##### Additional misc analyses #################################################

# library(ggcorrplot) 
# model.matrix(~0+., data=tuv_na[,c('age', 'obesity_1', 'bmi',
#                                   'gender', 'year', 'alcohol_c', 'smoking_c', 
#                                   'marital', 'education', 'employ', 
#                                   'income_c')]) %>%
#   cor(use="pairwise.complete.obs") %>%
#   ggcorrplot(show.diag=F, type="lower", lab=TRUE, lab_size=2)

t.test(obesity_1 ~ gender, data=tuv_na)

fit1 <- glm(obesity_1 ~ age, data = tuv_na, family = binomial(link = "logit"))
pred_fit1 <- predict(fit1, type = "response")
plot(obesity_1 ~ age, data=tuv_na)
lines(pred_fit1 ~ tuv_na$age, type="l", col="red")

fit2 <- glm(obesity_1 ~ age + age2 + age3, data = tuv_na, family = binomial(link = "logit"))
summary(fit2)
pred_fit2 <- predict(fit2, type = "response")
plot(obesity_1 ~ age_c, data=tuv_na) 

fit3 <- lm(bmi ~ age + age2 + age3, data = tuv_na)
summary(fit3)
tuv_na %>% ggplot(aes(x=age, y=bmi)) + 
  geom_jitter(width=0,height=0.05) + 
  geom_smooth(method="lm", formula=y~poly(x, 2)) +
  ggtitle("Cubic Polynomial Regression")

tuvalu202022_adult$region <- factor(tuvalu202022_adult$region, levels=1:9)
par(mfrow = c(2,2))
boxplot(tuvalu202022_adult$bmi ~ factor(tuvalu202022_adult$region, levels=1:9),
        xlab = "Region (1 is main island)", ylab = "BMI")
boxplot(tuvalu202022_adult$bmi ~ tuvalu202022_adult$alcohol_c,
        xlab = "Alcohol (1 indicates consumption)", ylab = "BMI")
boxplot(tuvalu202022_adult$bmi ~ tuvalu202022_adult$smoking_c,
        xlab = "Smoking (1 indicates smoker)", ylab = "BMI")
boxplot(tuvalu202022_adult$bmi ~ tuvalu202022_adult$gender,
        xlab = "Gender (1 indicates men)", ylab = "BMI")


fit6 <- glm(obesity_1 ~ year + age + alcohol_c + smoking_c + gender, 
                 data=tuv_na,
                 family=binomial())
summary(fit6)

fit7 <- glm(obesity_1 ~ year, data=tuv_na, family=binomial())
summary(fit7)
fit8 <- lm(bmi ~ year, data=tuv_na)
summary(fit8)
fit9 <- glm(obesity_1 ~ year + age + gender + alcohol_c + smoking_c + income_c, 
            data=tuvalu202022_adult, family=binomial())
summary(fit9)

plot(tuvalu202022_adult$age, tuvalu202022_adult$obesity_1)
plot(plot_data$age, temp1$fitted.values)
lines(tuvalu202022_adult$age, temp1$fit)

library(Hmisc)
fit4 <- lm(tuv_na$bmi ~ rcspline.eval(tuv_na$age, nk=3, inclx=T))
par(mfrow=c(1,1))
plot(tuv_na$age, tuv_na$bmi, xlab="Age", ylab="BMI", main="Restricted Cubic Spline with 3 Knots")
points(tuv_na$age, fitted(fit4), col="red", pch=16)

fit5_2020 <- glm(obesity_1 ~ age + alcohol_c + smoking_c + gender + region_c, 
            data=tuvalu202022_adult[tuvalu202022_adult$year == "2020",],
            family=binomial())
summary(fit5_2020)
logistic_coef(summary(fit5_2020))

#Sensitivity analysis by stratification
#Year 2020
#unadjusted model
glm_food_import_un_obesity1<-glm(obesity_1~age_c, data=tuvalu202022_adult[tuvalu202022_adult$year=="2020",], family=binomial(link = "logit"))
glm_food_import_un_obesity3<-glm(obesity_3~age_c, data=tuvalu202022_adult[tuvalu202022_adult$year=="2020",], family=binomial(link = "logit"))
#Adjusted models
glm_food_import_obesity1<-glm(obesity_1~age_c+smoking_c+gender+education_c, data=tuvalu202022_adult[tuvalu202022_adult$year=="2020",], family=binomial(link = "logit"))
glm_food_import_obesity3<-glm(obesity_3~age_c+smoking_c+gender+education_c, data=tuvalu202022_adult[tuvalu202022_adult$year=="2020",], family=binomial(link = "logit"))
#use the functions to extract OR and CIs, save to working directory
write.xlsx(list("Obesity"=rbind(logistic_coef(summary(glm_food_import_un_obesity1)),logistic_coef(summary(glm_food_import_obesity1))),"Severe obesity"=rbind(logistic_coef(summary(glm_food_import_un_obesity3)),logistic_coef(summary(glm_food_import_obesity3)))),"Regresion_obesity_age_2020.xlsx")
#Year 2022
#unadjusted model
glm_food_import_un_obesity1<-glm(obesity_1~age_c, data=tuvalu202022_adult[tuvalu202022_adult$year=="2022",], family=binomial(link = "logit"))
glm_food_import_un_obesity3<-glm(obesity_3~age_c, data=tuvalu202022_adult[tuvalu202022_adult$year=="2022",], family=binomial(link = "logit"))
#Adjusted models
glm_food_import_obesity1<-glm(obesity_1~age_c+smoking_c+gender+education_c, data=tuvalu202022_adult[tuvalu202022_adult$year=="2022",], family=binomial(link = "logit"))
glm_food_import_obesity3<-glm(obesity_3~age_c+smoking_c+gender+education_c, data=tuvalu202022_adult[tuvalu202022_adult$year=="2022",], family=binomial(link = "logit"))
#use the functions to extract OR and CIs, save to working directory
write.xlsx(list("Obesity"=rbind(logistic_coef(summary(glm_food_import_un_obesity1)),logistic_coef(summary(glm_food_import_obesity1))),"Severe obesity"=rbind(logistic_coef(summary(glm_food_import_un_obesity3)),logistic_coef(summary(glm_food_import_obesity3)))),"Regresion_obesity_age_2022.xlsx")

#Add cross product term
#unadjusted model
glm_food_import_un_obesity1<-glm(obesity_1~age_c*year, data=tuvalu202022_adult, family=binomial(link = "logit"))
glm_food_import_un_obesity3<-glm(obesity_3~age_c*year, data=tuvalu202022_adult, family=binomial(link = "logit"))
#Adjusted models
glm_food_import_obesity1<-glm(obesity_1~age_c*year+smoking_c+gender+education_c, data=tuvalu202022_adult, family=binomial(link = "logit"))
glm_food_import_obesity3<-glm(obesity_3~age_c*year+smoking_c+gender+education_c, data=tuvalu202022_adult, family=binomial(link = "logit"))
#use the functions to extract OR and CIs, save to working directory
write.xlsx(list("Obesity"=rbind(logistic_coef(summary(glm_food_import_un_obesity1)),logistic_coef(summary(glm_food_import_obesity1))),"Severe obesity"=rbind(logistic_coef(summary(glm_food_import_un_obesity3)),logistic_coef(summary(glm_food_import_obesity3)))),"Regresion_obesity_age_crossprod.xlsx")