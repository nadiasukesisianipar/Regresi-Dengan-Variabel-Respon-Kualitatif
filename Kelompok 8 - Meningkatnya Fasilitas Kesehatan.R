#Pemanggilan Data 
library(readxl)
data <- read_excel("C:/Users/M-SPORT/Downloads/Data Fasilitas Kesehatan.xlsx")
head(data)

#Menghitung Jumlah Faskes 2021 dan 2022
faskes21 <- rowSums(data[, c("RSU21", "RSK21", "PRI21", "PNRI21", "KP21", "P21")])
faskes21 
faskes22 <- rowSums(data[, c("RSU22", "RSK22", "PRI22", "PNRI22", "KP22", "P22")])
faskes22

#Pembuatan Variabel Binary pada Variabel Respon (faskes21 dan faskes 22)
faskes <- ifelse(faskes22 > faskes21, 1, 0)
faskes

#Pembuatan Dataframe
df.faskes <- data.frame(faskes, data$JP, data$Doctor, data$Dentist, data$Nursing, 
                        data$Midwifey, data$Pharmaceutical, data$`Public Health`, 
                        data$`Environmental Health`, data$Nutrionist, data$`Medical Laboratory`)
df.faskes


#Logit 
mod.logit <- glm(faskes ~ data.JP + data.Doctor + data.Dentist + data.Nursing + data.Midwifey + 
                   data.Pharmaceutical + data..Public.Health. + data..Environmental.Health. + data.Nutrionist + data..Medical.Laboratory., 
                 data = df.faskes, family=binomial(link="logit"))
summary(mod.logit)

#Probit
mod.probit <- glm(faskes ~ data.JP + data.Doctor + data.Dentist + data.Nursing + data.Midwifey + 
                    data.Pharmaceutical + data..Public.Health. + data..Environmental.Health. + data.Nutrionist + data..Medical.Laboratory., 
                  data = df.faskes, family=binomial(link="probit"))
summary(mod.probit)

#Membandingkan Model Logit, dan Probit
library(huxtable)
library(magrittr)
huxreg(Logit = mod.logit, Probit = mod.probit) %>% 
  set_caption("Models estimating probability 
              of denying loans")%>% 
  add_colnames()

