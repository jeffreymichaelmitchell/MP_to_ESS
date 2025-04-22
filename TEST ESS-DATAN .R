
library(tidyverse)
library(rio)
ess <- import("ESS 1-11 TEST.csv")
table(ess$cntry)
table(ess$cntry,ess$essround)

packages <- c("tidyverse", "dplyr", "knitr", "Hmisc",
              "lme4", "lmerTest", "sjstats", "sjPlot")
lapply(packages, library, character.only = TRUE)

install.packages("eurostat")
library(remotes)
remotes::install_github("ropengov/eurostat")

#df<-ESS %>% 
  #dplyr::select(cntry, essround, yrbrn, brncntr, eisced, gndr, imsmetn, 
               # imdfetn, impcntr, imbgeco,imueclt, imwbcnt, , hincfel, lrscale, dweight)

df<-ess %>% 
  dplyr::select(,essround,cntry,region,dweight,pspwght,pweight,anweight,freehms,hmsacld ,hmsfmlsh ,lrscale,gincdif ,ipeqopt,ipeqopta,brncntr,blgetmg,feethngr,facntn,facntr ,mocntn,mocntr,
                ,dscrage,dscrdk ,dscrdsb ,dscretn ,dscrgnd ,dscrgrp ,dscrlng ,dscrnap ,dscrntn ,dscroth ,dscrrce ,dscrrlg ,dscrrlg ,dscrsex ,gndr ,age ,agea ,domicil , yrbrn, brncntr, eisced, gndr, imsmetn, 
                ,eduyrs ,eisced ,hincfel ,hincsrc ,hincsrca ,hinctnt ,hinctnta ,grspay ,grspaya ,isco08 ,iscoco ,empl,uemp12m ,uemp5yr  ,imdfetn, impcntr, imbgeco,imueclt, imwbcnt,
                , hincfel, lrscale, brncntr, cntbrth, cntbrtha, cntbrthb, cntbrthc, cntbrthd )

#add contry of birth ( jeff)



library(tidyverse)
library(rio)

packages <- c("tidyverse", "dplyr", "knitr", "naniar", "Hmisc",
              "lme4", "lmerTest", "sjstats", "sjPlot", 
              "essurvey", "WDI", "zoo", "manifestoR")
ess <- import("ESS 1-11 TEST.csv")




df<-ess %>% 
  dplyr::select(,essround,cntry,region,dweight,pspwght,pweight,anweight,, brncntr, facntr, mocntr, livecntr, livecnta, freehms,hmsacld ,hmsfmlsh ,lrscale,gincdif ,ipeqopt,ipeqopta,brncntr,blgetmg,feethngr,facntn,facntr ,mocntn,mocntr,
                ,dscrage,dscrdk ,dscrdsb ,dscretn ,dscrgnd ,dscrgrp ,dscrlng ,dscrnap ,dscrntn ,dscroth ,dscrrce ,dscrrlg ,dscrrlg ,dscrsex ,gndr ,age ,agea ,domicil , yrbrn, brncntr, eisced, gndr, imsmetn, 
                ,eduyrs ,eisced ,hincfel ,hincsrc ,hincsrca ,hinctnt ,hinctnta ,grspay ,grspaya ,isco08 ,iscoco ,empl,uemp12m ,uemp5yr  ,imdfetn, impcntr, imbgeco,imueclt, imwbcnt,
                , hincfel, lrscale )


df<-df %>% filter(cntry != "AL", cntry !="XK")

df$cntry<- recode(df$cntry, 
                  "AT"="Austria",
                  "BE"="Belgium",
                  "BG"="Bulgaria",
                  "CH"="Switzerland",
                  "CY"="Cyprus",
                  "CZ"="Czech Republic",
                  "DE"="Germany",
                  "DK"="Denmark",
                  "EE"="Estonia",
                  "ES"="Spain",
                  "FI"="Finland",
                  "FR"="France",
                  "GB"="United Kingdom",
                  "GR"="Greece",
                  "HR"="Croatia",
                  "HU"="Hungary",
                  "IE"="Ireland",
                  "IL"="Israel",
                  "IS"="Iceland",
                  "IT"="Italy",
                  "PL"="Poland",
                  "PT"="Portugal",
                  "LT"="Lithuania",
                  "LU"="Luxembourg",
                  "NL"="Netherlands",
                  "NO"="Norway",
                  "RU"="Russian Federation",
                  "SE"="Sweden",
                  "SI"="Slovenia",
                  "SK"="Slovak Republic",
                  "TR"="Turkey",
                  "UA"="Ukraine",
                  "RO"="Romania",
                  "LV"="Latvia"
)

table(df$cntry)

df$year<-recode(df$essround,
                "1"="2002",
                "2"="2004",
                "3"="2006",
                "4"="2008",
                "5"="2010",
                "6"="2012",
                "7"="2014",
                "8"="2016",
                "9"="2018",
                "10"="2020",
                "11"="2023")

table(df$year)
df$year<-as.numeric(df$year)

df <- df %>% filter(!is.na(imdfetn) & !imdfetn %in% c('0','7','8','9'))
df <- df %>% filter(!is.na(imsmetn) & !imsmetn %in% c('0','7','8','9'))
df <- df %>% filter(!is.na(impcntr) & !impcntr %in% c('0','7','8','9'))
df <- df %>% filter(!is.na(imbgeco) & !imdfetn %in% c('0','7','8','9'))
df <- df %>% filter(!is.na(imsmetn) & !imueclt %in% c('0','7','8','9'))
df <- df %>% filter(!is.na(impcntr) & !imwbcnt %in% c('0','7','8','9'))



df$imdfetn<-as.numeric(df$imdfetn)
df$imsmetn<-as.numeric(df$imsmetn)
df$impcntr<-as.numeric(df$impcntr)
df$imdfetn<-recode(df$imdfetn, "1"="4", "2"="3", "3"="2","4"="1")
df$imsmetn<-recode(df$imsmetn, "1"="4", "2"="3", "3"="2","4"="1")
df$impcntr<-recode(df$impcntr, "1"="4", "2"="3", "3"="2","4"="1")
df$imdfetn<-as.numeric(df$imdfetn)
df$imsmetn<-as.numeric(df$imsmetn)
df$impcntr<-as.numeric(df$impcntr)

df<-df %>% 
  mutate(immsent= (imsmetn+imdfetn+impcntr)/3, na.rm=TRUE) %>% 
  mutate(immatt= (imbgeco+imueclt+imwbcnt)/3, na.rm=TRUE) 

correlation_matrix <- cor(df[, c("immsent", "imdfetn", "impcntr", "imbgeco", "imueclt", "imwbcnt")])
print(correlation_matrix)


  
#insert loop/mutate merged discrimination variable condition etno/racial/rel discrim <- if etno and/or rasce and/or/ nationality and/or religion and/or language
#ediscr = ethninc discrmination, generaldiscr=all discrm
  
df$etndisc_binary<- ifelse(df$dscretn==1, 1, 
                    ifelse(df$dscrrce==1, 1, 
                           ifelse(df$dscrntn ==1, 1,
                                  ifelse(df$dscrlng ==1, 1,
                                         ifelse(df$dscrrlg ==1, 1, 0
                                                )))))

#table (df$dscrgrp, df$etndisc_binary)




#correlation_matrix <- cor(df[, c("dscretn", "dscrrlg", "dscrntn", "dscrrce", "dscrage","dscrdsb", "dscrgnd", "dscroth", "dscrsex")], use = "complete.obs")

df$cntryyr <- as.numeric(as.factor(df$cntry))*10000 + df$essround


#removing cntryyear with etndisc lower than 30 N

df <- df %>%
 mutate(cntryyear_etndscr_above_30 = ifelse(df$cntryyr %in% c('10006', "60003", "60004", '60005', '60009', '70001', '70008', '70009', '90001', '90002', '90003', "120002",  '120003', '160005', '200002', '200006', '200008', '200009', '200010', '210001', '210006', '220005', '220009', '220010', '220011', '280003', '290001', '290002', '290003', '290004', '290005', '290006', '290007', '290008', '290009', '300001', '300002', '300003', '300007', '300008', '350002', '350003', '350004', '350005', '350006', '350007', '350008', '350009', '350010', '360009', '380005'), NA, df$cntryyr))
table(df$cntryyear_etndscr_above_30, df$etndisc_binary)

df<-df %>% 
      group_by(cntryyr) %>% 
       mutate(country_imsent=mean(immsent, na.rm=TRUE)) %>%
      ungroup()

table(df$country_imsent)


df<-df %>% 
  group_by(cntryyr) %>% 
  mutate(country_immatt=mean(immatt, na.rm=TRUE)) %>%
  ungroup()

table(df$country_immatt)


#df$etndisc_zero<- ifelse(df$etndisc_accumlated==00000, 'Zero',"Discr")
#table (df$etndisc_zero)

#Accumulated discrimination, one, two or many

df$etndisc_one <- ifelse(df$etndisc_accumlated=='1.0.0.0.0', 'One',
                        ifelse(df$etndisc_accumlated=='0.1.0.0.0', 'One',
                                  ifelse(df$etndisc_accumlated=='0.0.1.0.0','One',
                                         ifelse(df$etndisc_accumlated=='0.0.0.1.0',"One",
                                         ifelse(df$etndisc_accumlated=='0.0.0.0.1','One',"No")))))


table(df$etndisc_one)
                                         

  
df$etndsc_two<- ifelse(df$etndisc_accumlated=="1.1.0.0.0", 'Two',
                           ifelse(df$etndisc_accumlated=="1.0.1.0.0", "Two",
                                  ifelse(df$etndisc_accumlated=="1.0.0.1.0",'Two',
                                         ifelse(df$etndisc_accumlated=="1.0.0.1.0","Two",
                                         ifelse(df$etndisc_accumlated=="1.0.0.0.1","Two",
                                          ifelse(df$etndisc_accumlated=="0.1.1.0.0", "Two",
                                           ifelse(df$etndisc_accumlated=="0.1.0.1.0", "Two",
                                           ifelse(df$etndisc_accumlated=="0.1.0.0.1", "Two",
                                            ifelse(df$etndisc_accumlated=="0.0.1.1.0", "Two",
                                            ifelse(df$etndisc_accumlated=="0.0.1.0.1", "Two",
                                             ifelse(df$etndisc_accumlated=="0.0.0.1.1", "Two","No"
                                        )))))))))))  
table(df$dscrgrp,df$etndsc_two)

                                       

#creating two variables for groups with or withoit immigrant background 
#(i.e either having migrated or having a partent that have migratef to the country/ being born in the country by native parents)
                                         
                                         
df$immigrant_or_immigrant_background<- ifelse(df$brncntr== 2, 'Yes',
                                              ifelse(df$facntr==2, 'Yes', 
                                                     ifelse(df$mocntr==2, "Yes", 'No')))

df$native_with_native_parents <- ifelse(
                      df$brncntr == 1 & df$facntr == 1 & df$mocntr == 1, "Yes", "No")
                      
#kontextual indicators - ???? HUR LADDAR MAN NER DEM?

library(WDI)
wdi <- WDI(country = "all", indicator = c("SM.POP.TOTL.ZS", "NY.GDP.PCAP.PP.KD", "SL.UEM.TOTL.NE.ZS", 
                                           "SL.TLF.TOTL.FE.ZS", "SP.POP.65UP.TO.ZS", "SM.POP.TOTL.ZS",
                                           "CC.EST", "SI.POV.GINI", "NY.GDP.PCAP.KD.ZG","SI.DST.03RD.20"), start = 2002, end = 2023)

#select only the rows that correspond to the ESS  
wdi <- wdi[wdi$country %in% df$cntry,] 


                      
#table(df$native_with_native_parents)                                                 
#table(df$immigrant_or_immigrant_background, df$etndisc_binary)


df<-df %>%
  mutate(age=year-yrbrn)

df$age<-ifelse(df$age<16, NA, df$age)
df$age<-ifelse(df$age>100, NA, df$age)

df <-df %>% 
  mutate(university= as.numeric(eisced >5))

df<-df %>% 
  mutate(young= as.numeric(age< 26)) %>% 
  mutate(old= as.numeric(age>64))

df<-df %>% 
  group_by(df$cntryyr) %>% 
  mutate(country_imsent=mean(df$immsent, na.rm=TRUE), country_immatt= mean(df$immatt, na.rm=TRUE)) %>%
  ungroup()

library(lme4)
library(lmerTest)

df$etndisc_binary_num<- as.numeric(df$etndisc_binary)
ols<- lm(etndisc_binary ~ country_immatt , data = df)
summary(ols)

m0<- lmer(etndisc_binary ~ country_immatt + (1|cntry), data = df)
summary(m0)

plot_model(m0, type = 'pred')

m1<-lmer(etndisc_binary ~ country_immatt + (1|cntry)+ (1|year), data = df)
summary(m1)

plot_model(m1, type = 'pred')


#select only the rows that correspond to the ESS  
wdi <- wdi[wdi$country %in% df$cntry,] 
#Interpolate % stock FB here then merge 2000-2005-2010-2015 
#(then just rename 2015 to 2016 for the last ess wave)

#borde vi ha Eurostat-data istället? De har årliga mätningar, för att mäta förändring duger det ju inte att ha 9 år gammal data 

install.packages("zoo")
library(zoo)


#need to change years for forgein born


df<-left_join(df, wdi, by=c("cntry"= "country", "year"= "year"))


df$GDP<-df$NY.GDP.PCAP.PP.KD
df$UNEMP<-df$SL.UEM.TOTL.NE.ZS
df$AGED<-df$SP.POP.65UP.TO.ZS
df$CORRUPT<- df$CC.EST
df$FEMLAB<- df$SL.TLF.TOTL.FE.ZS

df$GINI<-df$SI.POV.GINI

#Between country variables for prejudice,, GDP,Gini
df<-df %>% 
  group_by(cntry) %>% 
  mutate(country_immatt_mean= mean(immatt, na.rm=TRUE), country_etndisc_binary_mean= mean(etndisc_binary, na.rm=TRUE),
         GINI_mean= mean(GINI, na.rm=TRUE), UNEMP_mean=mean(UNEMP, na.rm=TRUE), GDP_Mean= mean(GDP, na.rm=TRUE)) %>%
  ungroup()

#Center individual level immat (trust in jeffs data) against the other people in their country year

df<-df %>% 
  group_by(cntryyr) %>% 
  mutate(ppl_immatt_mean=mean(immatt, na.rm=TRUE)) %>%
  ungroup()

#This might be thought of as the level to which individuals differ from their country in their level of prejudice


df$ppl_immatt_mean_2<-df$immatt-df$ppl_immatt_mean

#Make the within country variables for generalized trust and controls (VARFÖR PctFB D? + kan jag göra som jag gjorde med GINI?)
df$country_immatt_3<- df$country_immatt - df$country_immatt_mean
df$log_GDP<- log(df$GDP)
df$log_GDP_mean<- log(df$GDP_Mean)
df$log_GDP_2<- df$log_GDP-df$log_GDP_mean
df$GINI_2<- df$GINI-df$GINI_mean
df$UNEMP2<- df$UNEMP-df$UNEMP_mean

#nytt dataset med bara macrovariabler för FRA 2016

#ta bara med country-year 2015?-2016

new_df <- df %>%
       dplyr::select(GDP, GDP_Mean, log_GDP, log_GDP_mean, log_GDP_2, UNEMP, UNEMP2, UNEMP_mean,ppl_immatt_mean, country_imsent, country_immatt, country_immatt_mean, country_etndisc_binary_mean, country_immatt_3, AGED, GINI, GINI_2, GINI_mean, cntry, year) %>% 
       filter(year == '2016')

new_df <-new_df %>% distinct()


#fastnat på att ladda manifestdata
#kolla upp paper - christian chimara alexander schmitt (pol elit doscourses ) anti immigrant attitudes, -> kolla upp)


install.packages("manifestoR")
library(manifestoR)



#ladda ner UN data on migration stock

# The sample R script below sends a request to the Migration Data Portal API to retrieve data for ‘stock_abs_’ and 'refug_host' indicators in 2015 and 2020. It will raise an exception if the request is unsuccessful. Otherwise, it parses the JSON response from the API.
# Loading necessary libraries
library(httr)
library(jsonlite)
# Setting up the API URL
base_url <- 'https://www.migrationdataportal.org/api/international-values?'
# Setting up the request header with API key
headers_ <- add_headers(`api-key` = '3827056fe2ed0c23fc3a643336e30000')
# Defining the parameters for the GET request
params_ <- list(
  indicator = 'stock_abs_+refug_host',
  year = '2000+2005+2010+2015+2020'
)
# Handling exceptions and errors using tryCatch
response_json <- tryCatch({
  # Making a GET request with the specified header and parameters
  api_response <- GET(base_url, headers_, query = params_)
  # Check if the request was successful (status code 200)
  if (status_code(api_response) != 200) {
    stop("Failed to fetch data: ", status_code(api_response))
  } 
  # Parsing the JSON response
  content(api_response, as = "parsed", type = "application/json") 
}, error = function(e) {
  # Catch and print any errors
  message("An error occurred: ", e)
  NULL
})
# Print the JSON response if successful
if (!is.null(response_json)) {
  print(response_json)
}


#ny df med landsvariabler för FRA-analys -> gör ett nytt skript , spara dataframen -> anv ny script med denna df -> använd för att lyfta fram till fra analys
# hitta varför tjeckien inte var i 2016 data


new_df <- df %>%
  +     dplyr::select(GDP, GDP_Mean, log_GDP, log_GDP_mean, log_GDP_2, UNEMP, UNEMP2, UNEMP_mean,ppl_immatt_mean, country_imsent, country_immatt, country_immatt_mean, country_etndisc_binary_mean, country_immatt_3, AGED, GINI, GINI_2, GINI_mean, cntry, year) %>% 
  +     filter(year == '2016')
> new_df %>% distinct()






#isco klass aggregerad isco08 -> 0-9
df$isco08_aggregerad <- cut(
     df$isco08, 
       breaks = c(-1, 999, 1999, 2999, 3999, 4999, 5999, 6999, 7999, 8999, 9999),
       labels = 0:9,
      right = TRUE)
 
   df$isco08_aggregerad <- as.numeric(as.character(df$isco08_aggregerad))
 
   # Gör alla värden som är större än 9999 till NA
   df$isco08_aggregerad[df$isco08 > 9999] <- NA
 
   table(df$isco08_aggregerad)
   
table(df$isco08_aggregerad, df$etndisc_binary)



#ej lyckat försök att göra qvintiles av ink eftersom det är olika tid i mätningar för hinctnt och hinctntna
#df <- df %>%
#  mutate (household_income <- hinctnt+ hinctnta)

#df<-df %>% 
#  mutate(incq1= as.numeric(hinctnt< 26)) %>% 
#  mutate(incq2= as.numeric(hinctnt>64)) %>%
#  mutate(incq3= as.numeric(hinctnt< 26)) %>% 
#  mutate(inq4= as.numeric(hinctnt< 26))

#create a variable for respondents who have parents that both are born in the country

 df <- df %>%
mutate(both_parents_born_in_cnrty = ifelse(facntr == 1 & mocntr == 1, "Yes", "No" ))


#ha med både immatt och immsent?






#variabler att ha med
essround - ESS round
cntry - Country
region - Region
dweight - Design weight
pspwght - Post-stratification weight including design weight ?
pweight - Population size weight (must be combined with dweight or pspwght) ?
anweight - Analysis weight?
  
#Anti-invanding / etnsika minoriteter
imsmetn - Allow many/few immigrants of same race/ethnic group as majority
imdfetn - Allow many/few immigrants of different race/ethnic group from majority
impcntr - Allow many/few immigrants from poorer countries outside Europe
imbgeco - Immigration bad or good for country's economy
imueclt - Country's cultural life undermined or enriched by immigrants
imwbcnt - Immigrants make country worse or better place to live
algyplv - Allow many or few Gypsies to come and live in country
aljewlv - Allow many or few Jewish people to come and live in country
allbpe - Allow unskilled labourers from [poor European country providing largest number of migrants]
allbpne - Allow unskilled labourers from [poor non-European country providing largest number of migrants]
almuslv - Allow many or few Muslims to come and live in country
alpfpe - Allow professionals from [poor European country providing largest number of migrants]
alpfpne - Allow professionals from [poor non-European country providing largest number of migrants]
alwspsc - Immigrant communities should be allowed separate schools
blncmig - Number of people leaving country compared to coming in
cpimpop - Country's number of immigrants compared to European countries same size
ctbfsmv - All countries benefit if people can move where their skills needed
dfegcf - Different race or ethnic group: have any close friends
dfegcon - Different race or ethnic group: contact, how often
dfeghbg - Different race or ethnic group: contact, how bad or good
eimgrpc - Immigrants from Europe: most from rich/poor countries
eimpcnt - Allow many/few immigrants from poorer countries in Europe
eimrcnt - Allow many/few immigrants from richer countries in Europe
idetalv - People of minority race/ ethnic group in ideal living area
imacrlv - If immigrants commit any crime they should be made to leave
imbghct - Immigration to country bad or good for home countries in the long run
imbleco - Taxes and services: immigrants take out more than they put in or less
imfljob - Immigrants help to fill jobs where there are shortage of workers
imgclg - Any immigrant colleagues
imgetn - Most immigrants to country of same race/ethnic group as majority
imgfrnd - Any immigrant friends
imgrpc - Immigrants from outside Europe: from rich/poor countries
imhecop - Immigrants harm economic prospects of the poor more than the rich
imrcntr - Allow many/few immigrants from richer countries outside Europe
imrsprc - Richer countries responsible to accept people from poorer countries
imscrlv - If immigrants commit serious crime they should be made to leave
imsmrgt - Immigrants should be given same rights as everyone else
imtcjob - Immigrants take jobs away in country or create new jobs
imunplv - If immigrants are long term unemployed they should be made to leave
imwbcrm - Immigrants make country's crime problems worse or better
imwgdwn - Average wages/salaries generally brought down by immigrants
lwdscwp - Law against ethnic discrimination in workplace good/bad for a country
lwpeth - Law against promoting racial or ethnic hatred good/bad for a country
noimbro - Of every 100 people in country how many born outside country
pplstrd - Better for a country if almost everyone share customs and traditions
qfimchr - Qualification for immigration: christian background
qfimcmt - Qualification for immigration: committed to way of life in country
qfimedu - Qualification for immigration: good educational qualifications
qfimfml - Qualification for immigration: close family living here
qfimlng - Qualification for immigration: speak country's official language
qfimwht - Qualification for immigration: be white
qfimwlt - Qualification for immigration: be wealthy
qfimwsk - Qualification for immigration: work skills needed in country
rlgueim - Religious beliefs and practices undermined or enriched by immigrants
shrrfg - Country has more than its fair share of people applying refugee status
smctmbe - Some cultures: much better or all equal
smegbhw - Some races or ethnic groups: born harder working
smegbli - Some races or ethnic groups: born less intelligent
imrccon - Immigrants receive more or less than they contribute
imsclbn - When should immigrants obtain rights to social benefits/services


#homofobi
freehms - Gays and lesbians free to live life as they wish
hmsacld - Gay and lesbian couples right to adopt children
hmsfmlsh - Ashamed if close family member gay or lesbian

#sexism-feminism och traditionella familjeideal
mnrsphm - Men should take as much responsibility as women for home and children
mnrgtjb - Men should have more right to job than women when jobs are scarce

mascfel - How masculine respondent feels
femifel - How feminine respondent feels
trplcnt - How fair the police in [country] treat women/men
eqwrkbg - Bad or good for family life in [country] if equal numbers of women and men are in paid work
eqpolbg - Bad or good for politics in [country] if equal numbers of women and men are in positions of political leadership
eqmgmbg - Bad or good for businesses in [country] if equal numbers of women and men are in higher management positions
eqpaybg - Bad or good for economy in [country] if women and men receive equal pay for doing the same work
eqparep - Dividing the number of seats in parliament equally between women and men
eqparlv - Require both parents to take equal periods of paid leave to care for their child
freinsw - Firing employees who make insulting comments directed at women in the workplace
fineqpy - Making businesses pay a fine when they pay men more than women for doing the same work
wsekpwr - How often women seek to gain power by getting control over men
weasoff - How often women get easily offended?
wlespdm - How often women paid less than men for same work in [country]
wexashr - How often women exaggerate claims of sexual harassment in the workplace
wprtbym - Women should be protected by men
wbrgwrm - Women tend to have better sense of what is right and wrong compared with men
prntghr - Children in home, parents should stay together even if don't get along
rrfmly - A person's family should be main priority in life
pthcncr - Part-time work rather than full-time negative consequences for career
ptmhmcc - Total time part-time work rather than full-time caring for children
gvcldcr - Child care services for working parents, governments' responsibility
impbemw - How important being a man/woman is to the way respondent think about him/herself
prrfmly - A person's family should be main priority in life ?
  advcyc - Approve if person gets divorced while children aged under 12. SPLIT BALLOT
aftjbyc - Approve if person has full-time job while children aged under 3. SPLIT BALLOT
alvgptn - Approve if person lives with partner not married to. SPLIT BALLOT
anvcld - Approve if person chooses never to have children. SPLIT BALLOT
tygsexi - Have sexual intercourse, age too young. SPLIT BALLOT

#auktoritärianism
lrnobed - Obedience and respect for authority most important virtues children should learn
loylead - Country needs most loyalty towards its leaders
lawobey - The law should always be obeyed
ilglpst - Participated illegal protest activities last 12 months
ipfrule - Important to do what is told and follow rules
ipfrulea - Important to do what is told and follow rules
impoblw - To be a good citizen: how important to always obey laws/regulations

#Nationalism-cosmopolism
atchctr - How emotionally attached to [country]
atcherp - How emotionally attached to Europe
fclcntr - Feel close to country
comnlng - Better for a country if almost everyone speak one common language
ctbfsmv - All countries benefit if people can move where their skills needed
gvrfgap - Government should be generous judging applications for refugee status
gvtrimg - Compared to yourself government treats new immigrants better or worse
pplstrd - Better for a country if almost everyone share customs and traditions
vrtrlg - Better for a country if a variety of different religions

#höger-vänster eko
gincdif - Government should reduce differences in income levels
needtru - Employees need strong trade unions to protect work conditions/wages
dfincac - Large differences in income acceptable to reward talents and efforts
ditxssp - Government decrease/increase taxes and social spending
smdfslv - For fair society, differences in standard of living should be small
sofrdst - Society fair when income and wealth is equally distributed

trummb - Trade union, last 12 months: member
truna - Trade union, last 12 months: no answer
trunn - Trade union, last 12 months: none apply
truptp - Trade union, last 12 months: participated
truref - Trade union, last 12 months: refusal

#höger-vänster ideolgi
lrscale - Placement on left right scale

#social justice
ipeqopt - Important that people are treated equally and have equal opportunities
ipeqopta - Important that people are treated equally and have equal opportunities

#minoritet
brncntr - Born in country
blgetmg - Belong to minority ethnic group in country
feethngr - Feel part of same race or ethnic group as most people in country
cntbrth - Country of birth
cntbrtha - Country of birth
cntbrthb - Country of birth
cntbrthc - Country of birth
cntbrthd - Country of birth
ctzcntr - Citizen of country
ctzship - Citizenship
ctzshipa - Citizenship
ctzshipb - Citizenship
ctzshipc - Citizenship
ctzshipd - Citizenship
facntn - Continent of birth, father
facntr - Father born in country
fbrncnt - Country of birth, father
fbrncnta - Country of birth, father
fbrncntb - Country of birth, father
fbrncntc - Country of birth, father
livecntr - How long ago first came to live in country
livecnta - What year you first came to live in country
lnghoma - Language most often spoken at home: first mentioned
lnghom1 - Language most often spoken at home: first mentioned
lnghomb - Language most often spoken at home: second mentioned
lnghom2 - Language most often spoken at home: second mentioned
mbrncnt - Country of birth, mother
mbrncnta - Country of birth, mother
mbrncntb - Country of birth, mother
mbrncntc - Country of birth, mother
mmbrn - Month of birth
mocntn - Continent of birth, mother
mocntr - Mother born in country
anctry1 - First ancestry, European Standard Classification of Cultural and Ethnic Groups
anctrya1 - First ancestry, European Standard Classification of Cultural and Ethnic Groups
anctry2 - Second ancestry, European Standard Classification of Cultural and Ethnic Groups
anctrya2 - Second ancestry, European Standard Classification of Cultural and Ethnic Groups
acetalv - People of minority race/ ethnic group in current living area

pray - How often pray apart from at religious services
rlgatnd - How often attend religious services apart from special occasions
rlgblg - Belonging to particular religion or denomination
rlgblge - Ever belonging to particular religion or denomination
rlgdgr - How religious are you

#diskriminering
dscrage - Discrimination of respondent's group: age
dscrdk - Discrimination of respondent's group: don't know
dscrdsb - Discrimination of respondent's group: disability
dscretn - Discrimination of respondent's group: ethnic group
dscrgnd - Discrimination of respondent's group: gender
dscrgrp - Member of a group discriminated against in this country
dscrlng - Discrimination of respondent's group: language
dscrna - Discrimination of respondent's group: no answer
dscrnap - Discrimination of respondent's group: not applicable
dscrntn - Discrimination of respondent's group: nationality
dscroth - Discrimination of respondent's group: other grounds
dscrrce - Discrimination of respondent's group: colour or race
dscrref - Discrimination of respondent's group: refusal
dscrrlg - Discrimination of respondent's group: religion
dscrsex - Discrimination of respondent's group: sexuality
trtrsp - Feel people treat you with respect
trtunf - Feel people treat you unfairly
imprlg - Important in life: religion
imprwct - Fairly or unfairly treated in attempt to improve things at work
rghmgpr - The rights of minority groups are protected
rghmgprc - In country the rights of minority groups are protected
pplvdmc - In country people with extreme political views are prevented from expressing them
pplvdmi - Important for democracy: prevent people from expressing extreme political views
evfrjob - Everyone in country fair chance get job they seek
ifredu - Compared other people in country, fair chance achieve level of education I seek
ifrjob - Compared other people in country, fair chance get job I seek
netifr - Your net [pay/pensions/social benefits] is unfairly low, fair, or unfairly high
mascfel - How masculine respondent feels
femifel - How feminine respondent feels
trmedmw - Unfairly treated when visiting a doctor or seeking medical treatment because being a man/woman
trwrkmw - Unfairly treated in hiring pay or promotion at work because being a man/woman
trplcmw - Unfairly treated by the police because being a man/woman
trmdcnt - Women/men: treated equally fairly when seeking medical treatment in [country]
trwkcnt - Women/men: treated equally fairly in hiring, pay or promotions at work in [country]
trplcnt - How fair the police in [country] treat women/men

plcrspc - How often do police treat people in country with respect
plcstf - How satisfied with treatment from police when contacted
plcvcrc - How police treat victims of crime: Different races/ethnic groups
plcvcrp - How police treat victims of crime: Rich/poor


#Välmående/hälsa
stflife - How satisfied with life as a whole
happy - How happy are you
health - Subjective general health
hlthhmp - Hampered in daily activities by illness/disability/infirmity/mental problem
flrms - At times feel as if I am a failure
fltanx - Felt anxious, how often past week
fltbrd - Felt bored, how often past week
fltpcfl - Felt calm and peaceful, how often past week
fltrstm - Felt rested when woke up in morning, how often past week
flttrd - Felt tired, how often past week
lfcllk - On the whole life is close to how I would like it to be
pstvms - In general feel very positive about myself
stflfsf - Satisfied with how life turned out so far
ctrlife - How much control over life in general nowadays
alcbnge - Frequency of binge drinking for men and women, last 12 months
alcfreq - How often drink alcohol
alcwkdy - Grams alcohol, last time drinking on a weekday, Monday to Thursday
alcwknd - Grams alcohol, last time drinking on a weekend day, Friday to Sunday
anypacc - Any problems with accommodation listed on showcard
cgtsday - How many cigarettes smoke on typical day
cgtsmke - Cigarettes smoking behaviour
cgtsmok - Cigarette smoking behaviour
cnfpplh - Serious conflict between people in household when growing up, how often
dosprt - Do sports or other physical activity, how many of last 7 days
dshltdk - Discussed health, last 12 months: don't know
dshltgp - Discussed health, last 12 months: general practitioner
dshltms - Discussed health, last 12 months: medical specialist
dshltna - Discussed health, last 12 months: no answer
dshltnt - Discussed health, last 12 months: none of these
dshltref - Discussed health, last 12 months: refusal
eatveg - How often eat vegetables or salad, excluding potatoes
etfruit - How often eat fruit, excluding drinking juice
fnsdfml - Severe financial difficulties in family when growing up, how often
height - Height of respondent (cm)
hlpfmhr - Hours a week looking after or helping family members, friends, neighbours or others
hlpfmly - Looking after or helping family members, friends, neighbours or others
hltphal - Health problems, hampered, last 12 month: allergies
hltphbn - Health problems, hampered, last 12 month: back or neck pain
hltphbp - Health problems, hampered, last 12 month: breathing problems
hltphdi - Health problems, hampered, last 12 month: diabetes
hltphdk - Health problems, hampered, last 12 month: don't know
hltphhb - Health problems, hampered, last 12 month: high blood pressure
hltphhc - Health problems, hampered, last 12 month: heart or circulation problem
hltphna - Health problems, hampered, last 12 month: no answer
hltphnap - Health problems, hampered, last 12 month: not applicable
hltphnt - Health problems, hampered, last 12 month: none of these
hltphpa - Health problems, hampered, last 12 month: muscular or joint pain in hand or arm
hltphpf - Health problems, hampered, last 12 month: muscular or joint pain in foot or leg
hltphref - Health problems, hampered, last 12 month: refusal
hltphsc - Health problems, hampered, last 12 month: skin condition related
hltphsd - Health problems, hampered, last 12 month: stomach or digestion related
hltphsh - Health problems, hampered, last 12 month: severe headaches
hltpral - Health problems, last 12 months: allergies
hltprbn - Health problems, last 12 months: back or neck pain
hltprbp - Health problems, last 12 months: breathing problems
hltprca - Have or had any health problems listed on showcard (cancer)
hltprdi - Health problems, last 12 months: diabetes
hltprdk - Health problems, last 12 months: don't know
hltprhb - Health problems, last 12 months: high blood pressure
hltprhc - Health problems, last 12 months: heart or circulation problem
hltprna - Health problems, last 12 months: no answer
hltprnt - Health problems, last 12 months: none of these
hltprpa - Health problems, last 12 months: muscular or joint pain in hand or arm
hltprpf - Health problems, last 12 months: muscular or joint pain in foot or leg
hltprref - Health problems, last 12 months: refusal
hltprsc - Health problems, last 12 months: skin condition related
hltprsd - Health problems, last 12 months: stomach or digestion related
hltprsh - Health problems, last 12 months: severe headaches
medtrdk - No medical consultation or treatment, reason: don't know
medtrna - No medical consultation or treatment, reason: no answer
medtrnap - No medical consultation or treatment, reason: not applicable
medtrnl - No medical consultation or treatment, reason: not available where you live
medtrnp - No medical consultation or treatment, reason: could not pay
medtrnt - No medical consultation or treatment, reason: could not take time off work
medtrnu - Never unable to get medical consultation or treatment, reason, last 12 months
medtrnaa - No medical consultation or treatment, reason: no appointments available
medtroc - No medical consultation or treatment, reason: other commitments
medtroth - No medical consultation or treatment, reason: other
medtrref - No medical consultation or treatment, reason: refusal
medtrun - Unable to get medical consultation or treatment, last 12 months
medtrwl - No medical consultation or treatment, reason: waiting list too long
fltdpr - Felt depressed, how often past week
fltsd - Felt sad, how often past week

#Individkontroller
gndr - Gender
yrbrn - Year of birth
age - Age of respondent, calculated
agea - Age of respondent, calculated
brwmny - Borrow money to make ends meet, difficult or easy
crpdwk - Control paid work last 7 days
domicil - Domicile, respondent's description
dsbld - Doing last 7 days: permanently sick or disabled
edufld - Field or subject, highest qualification
edulvla - Highest level of education
edulvlb - Highest level of education
edulvlfa - Father's highest level of education
edulvlfb - Father's highest level of education
edulvlma - Mother's highest level of education
edulvlmb - Mother's highest level of education
edulvlpa - Partner's highest level of education
edulvlpb - Partner's highest level of education
eduyrs - Years of full-time education completed
eisced - Highest level of education, ES - ISCED
eiscedf - Father's highest level of education, ES - ISCED
eiscedm - Mother's highest level of education, ES - ISCED
eiscedp - Partner's highest level of education, ES - ISCED
emprf14 - Father's employment status when respondent 14
emprm14 - Mother's employment status when respondent 14
occf14 - Father's occupation when respondent 14
occf14a - Father's occupation when respondent 14
occf14b - Father's occupation when respondent 14
occm14 - Mother's occupation when respondent 14
occm14a - Mother's occupation when respondent 14
occm14b - Mother's occupation when respondent 14
emplrel - Employment relation
hincfel - Feeling about household's income nowadays
hincsrc - Main source of household income
hincsrca - Main source of household income
hinctnt - Household's total net income, all sources
hinctnta - Household's total net income, all sources
grspay - Usual gross pay in euro, before deductions for tax and insurance
grspaya - Usual gross pay in euro, before deductions for tax and insurance
icwhct - Have a set 'basic' or contracted number of hours
isco08 - Occupation, ISCO08
isco08p - Occupation partner, ISCO08
iscoco - Occupation, ISCO88 (com)
iscocop - Occupation partner, ISCO88 (com)
lvghw - Currently living with husband/wife
lvghwa - Currently living with husband/wife/civil partner
lvgoptn - Currently living with another partner than husband/wife
partner - Lives with husband/wife/partner at household grid
lvgptn - Currently living with partner
lvgptna - Currently living with partner
lvgptne - Ever lived with a partner without being married
lvgptnea - Ever lived with a partner, without being married
mainact - Main activity last 7 days
empl - Employment status
pdjobev - Ever had a paid job
pdjobyr - Year last in paid job
uemp12m - Any period of unemployment and work seeking lasted 12 months or more
uemp3m - Ever unemployed and seeking work for a period more than three months
uemp5yr - Any period of unemployment and work seeking within last 5 years
dsdclve - To what extent had to draw on savings/debt to cover ordinary living expenses last 3 years
marital - Legal marital status
maritala - Legal marital status
maritalb - Legal marital status, post coded
marsts - Legal marital status
nacer1 - Industry, NACE rev.1
nacer11 - Industry, NACE rev.1.1
nacer2 - Industry, NACE rev.2
dclvlf - Free to decide how to live my life
plinsoc - Your place in society
yrlvdae - How long lived in this area
iincsrc - Respondent's main source of income
mascfel - How masculine respondent feels
femifel - How feminine respondent feels
