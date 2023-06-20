install.packages(c('tidymodels','cowplot','plotly','data.table','ggmap','sf','caret'))
install.packages('readxl')
library('dplyr')
library('tidyr')
library('tidymodels')
library("cowplot")
library('plotly')
library('ggplot2')
library('data.table')
library('ggmap')
library('sf')
library('reshape2')
library('caret')
library('yardstick')
library("readxl")

penda <- readxl::read_xlsx("C:/Users/denis/Desktop/Analysis/Analyst_Assessment_Data_2023.xlsx")
diagnosis <- readxl::read_xlsx("C:/Users/denis/Desktop/Analysis/diagnosis.xlsx")
invoice <-  readxl::read_xlsx("C:/Users/denis/Desktop/Analysis/invoice.xlsx")

df = merge(x = penda, y = diagnosis, by = "VisitCode", all = TRUE)
df = merge(x = df, y = invoice, by = "VisitCode", all = TRUE)

df$Amount <- with(df, ifelse(Payor %in% c("Insurance Company B", "Insurance Company A") & Amount < 100, 100, Amount))

# Splitting into date and time
df$date <- as.Date(df$VisitDateTime)
str(penda)

#visits pipeine and Kimathi street
DATE1 <- as.Date("2022-04-30")
DATE2 <- as.Date("2022-10-01")
pendkmpipe <- df[df$MedicalCenter == 'Kimathi Street'| df$MedicalCenter == 'Pipeline', ]
pendkmpipe <- pendkmpipe[pendkmpipe$date > DATE1,]
pendkmpipe <- pendkmpipe[pendkmpipe$date < DATE2,]


#diagnosis taasia embakasi
pendembTass <- df[df$MedicalCenter == 'Tassia'|df$MedicalCenter == 'Embakasi', ]
pendembTass <- pendembTass[pendembTass$date > 2021-12-31,]

pendembTass <- pendembTass %>% group_by(Diagnosis) %>% summarise(Count = n())%>%
  arrange(desc(Count))


#most profitable payor
df$profit <- df$Amount * 0.3
payor_profit <- aggregate(profit ~ Payor, data = df, sum)
payor_profit <- payor_profit %>% arrange(desc(profit))

#least profitable medical center 
Center_profit <- aggregate(profit ~ MedicalCenter, data = df, sum)
Center_profit <- Center_profit %>% arrange(desc(profit))

#spend for acute gastritis
gastritis <- df %>% filter(Diagnosis == "acute gastritis")
gastritis <- gastritis %>% group_by(Diagnosis) %>%summarise(average = mean(Amount))

#how many unique patients experienced a blended healthcare approach in their healthcare journey
blended_patients <- unique(df$PatientCode[df$VisitCategory %in% c("In-person Visit", "Telemedicine Visit")])
num_blended_patients <- length(blended_patients)


#Calculate the Net Promoter Score (NPS) in Q3 2022.
#{Please note that valid NPS scores range from 0 to 10}

penda_data_q3_2022 <- subset(df, format(as.Date(VisitDateTime), "%Y-%m-%d") >= "2022-07-01" & format(as.Date(VisitDateTime), "%Y-%m-%d") <= "2022-09-30")
summary(df$Amount)
penda_data_q3_2022$NPS_Score <- ifelse(penda_data_q3_2022$Amount <= 375, 0:2,
                                       ifelse(penda_data_q3_2022$Amount <= 1340, 3:5,
                                              ifelse(penda_data_q3_2022$Amount <= 1716, 6:7,
                                                     ifelse(penda_data_q3_2022$Amount <= 2500, 7:9, 10))))

promoters <- sum(penda_data_q3_2022$NPS_Score >= 9)
detractors <- sum(penda_data_q3_2022$NPS_Score <= 6)
passives <- sum(penda_data_q3_2022$NPS_Score > 6 & penda_data_q3_2022$NPS_Score < 9)

total_responses <- promoters + detractors + passives

nps <- (promoters / total_responses) * 100 - (detractors / total_responses)

#second visits
total_patients <- length(unique(df$PatientCode))
second_visit_patients <- sum(table(df$PatientCode) == 2)
proportion_second_visits <- second_visit_patients / total_patients


#april
# Filter the dataset for April 2022
penda_april_2022 <- df %>%
  filter(format(as.Date(VisitDateTime), "%Y-%m") == "2022-04")

# Sort the dataset by patient and visit date
penda_april_2022 <- penda_april_2022 %>%
  arrange(PatientCode, VisitDateTime)

total_visits <- nrow(penda_april_2022)
within_30_days <- sum(difftime(penda_april_2022$VisitDateTime, lag(penda_april_2022$VisitDateTime)) <= 30, na.rm = TRUE)
# Calculate the percentage
percentage_within_30_days <- (within_30_days / total_visits) * 100
