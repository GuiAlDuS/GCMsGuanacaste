library(tidyverse)
library(lubridate)
setwd("~/OneDrive/Guillermo/GCMsClimaLiberia")

liberiaGCMs <- read_csv("data.csv")

#ver variables
eje1 <- liberiaGCMs %>% group_by(Variable) %>% summarise(nmros = n())


tasmax_anual <- liberiaGCMs %>% 
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario) %>% 
  filter(Variable == "tasmax") %>%
  summarise(tasmax_aNo = mean (Value))

tasmax_anual$aNo <- as.integer(tasmax_anual$aNo)
tasmax_anual$fecha <- ymd(sprintf("%d-01-01", tasmax_anual$aNo))

tasmax_anual$tasmax_aNo <- tasmax_anual$tasmax_aNo - 273.15

ggplot(tasmax_anual, aes(x=aNo, y=tasmax_aNo, color=Scenario, group=Scenario)) + 
  geom_line() + 
  stat_smooth(method="loess", level=.8)



tasmin_anual <- liberiaGCMs %>% 
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario) %>% 
  filter(Variable == "tasmin") %>%
  summarise(tasmin_aNo = mean (Value))

tasmin_anual$aNo <- as.integer(tasmin_anual$aNo)
tasmin_anual$fecha <- ymd(sprintf("%d-01-01", tasmin_anual$aNo))

tasmin_anual$tasmin_aNo <- tasmin_anual$tasmin_aNo - 273.15

ggplot(tasmin_anual, aes(x=aNo, y=tasmin_aNo, color=Scenario, group=Scenario)) + 
  geom_line() + 
  stat_smooth(method="loess", level=.8)

##lluvia
options(scipen = 999)

pr_diario <- liberiaGCMs %>% 
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y"))

#unidades: kg m-2 s-1 
#1kg de agua = 1 L
# 1 día = 86400 segundos
pr_diario$lluviadia <- pr_diario$Value * 86400

pr_anual_GCM <- pr_diario%>% 
  group_by(aNo, Scenario, Model) %>% 
  filter(Variable == "pr") %>%
  summarise(pr_GCM = sum (lluviadia))
  
pr_anual <- pr_anual_GCM %>%
  group_by(aNo, Scenario) %>%
  summarise (pr_aNo = mean (pr_GCM))

pr_anual$aNo <- as.integer(pr_anual$aNo)
pr_anual$fecha <- ymd(sprintf("%d-01-01", pr_anual$aNo))

ggplot(pr_anual, aes(x=aNo, y=pr_aNo, color=Scenario, group=Scenario)) + 
  geom_line() + 
  stat_smooth(method="loess", level=.8)

#cambio por mes
tasmax_mensual <- liberiaGCMs %>% 
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(mes, Scenario) %>% 
  filter(Variable == "tasmax") %>%
  summarise(tasmax_aNo = mean (Value))
 
ggplot(tasmax_mensual, aes(x=mes, y=tasmax_aNo, color=Scenario, group=Scenario)) + 
  geom_line() + 
  stat_smooth(method="loess", level=.8)



###Convertir archivos
datos1 <- read_tsv("DatosPocosol_10agosto2017.txt", skip=2, col_names = FALSE, col_types = cols(.default = col_character()))

header1 <- scan("DatosPocosol_10agosto2017.txt", nlines = 1, what = character(), sep = "\t") 
header2 <- scan("DatosPocosol_10agosto2017.txt", skip = 1, nlines = 1, what = character(), sep = "\t") 
names(datos1) <- paste0(header1, header2)

write_csv(datos1, "Pocosol.csv")
