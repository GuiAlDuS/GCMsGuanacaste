library(tidyverse)
library(lubridate)

liberiaGCMs <- read_csv("dataLiberia.csv")

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


tasmax_anual_GCM <- liberiaGCMs %>% 
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>% 
  filter(Variable == "tasmax") %>%
  summarise(tasmax_aNo = mean (Value - 273.15))

tasmin_anual_GCM <- liberiaGCMs %>% 
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>% 
  filter(Variable == "tasmin") %>%
  summarise(tasmin_aNo = mean (Value - 273.15))

anual_GCMs <- pr_anual_GCM %>% inner_join(tasmax_anual_GCM, by = c("aNo", "Scenario", "Model")) %>% inner_join(tasmin_anual_GCM, by = c("aNo", "Scenario", "Model"))

anual_GCMs <- anual_GCMs %>% rename(pr = pr_GCM, tasmax = tasmax_aNo, tasmin = tasmin_aNo)

saveRDS(anual_GCMs, "anual_GCMs.rds")
