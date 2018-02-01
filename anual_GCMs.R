library(tidyverse)

liberiaGCMs <- read_csv("dataLiberia.csv")

options(scipen = 999)


#transformación para lluvia diaria
#unidades: kg m-2 s-1 
#1kg de agua = 1 L
# 1 día = 86400 segundos

pr_anual_GCM <- liberiaGCMs %>% 
  filter(Variable == "pr") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>% 
  summarise(pr_GCM = sum (Value * 86400))

tasmax_anual_GCM <- liberiaGCMs %>%
  filter(Variable == "tasmax") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>% 
  summarise(tasmax_aNo = mean (Value - 273.15))

tasmin_anual_GCM <- liberiaGCMs %>%
  filter(Variable == "tasmin") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>% 
  summarise(tasmin_aNo = mean (Value - 273.15))

anual_GCMs <- pr_anual_GCM %>% inner_join(tasmax_anual_GCM, by = c("aNo", "Scenario", "Model")) %>% inner_join(tasmin_anual_GCM, by = c("aNo", "Scenario", "Model"))

anual_GCMs <- anual_GCMs %>% rename(Modelo = Model, pr = pr_GCM, tasmax = tasmax_aNo, tasmin = tasmin_aNo)

saveRDS(anual_GCMs, "anual_GCMs.rds")


##Todos los GCMs
liberiaTodosGCMs <- read_csv("dataLiberia_todosGSMs.csv")

options(scipen = 999)


#unidades: kg m-2 s-1 
#1kg de agua = 1 L
# 1 día = 86400 segundos

pr_anual_TodosGCM <- liberiaTodosGCMs %>% 
  filter(Variable == "pr") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>%
  summarise(pr_GCM = sum (Value * 86400, na.rm=TRUE))

tasmax_anual_TodosGCM <- liberiaTodosGCMs %>% 
  filter(Variable == "tasmax") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>% 
  summarise(tasmax_aNo = mean (Value - 273.15, na.rm=TRUE))

tasmin_anual_TodosGCM <- liberiaTodosGCMs %>% 
  filter(Variable == "tasmin") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>% 
  summarise(tasmin_aNo = mean (Value - 273.15, na.rm=TRUE))

anual_TodosGCMs <- pr_anual_TodosGCM %>% full_join(tasmax_anual_TodosGCM, by = c("aNo", "Scenario", "Model")) %>% full_join(tasmin_anual_TodosGCM, by = c("aNo", "Scenario", "Model")) #ojo que hay diferente cantidad de observaciones en cada tabla 

anual_TodosGCMs <- anual_TodosGCMs %>% rename(Modelo = Model, pr = pr_GCM, tasmax = tasmax_aNo, tasmin = tasmin_aNo)

saveRDS(anual_TodosGCMs, "anual_TodosGCMs.rds")


## graficos
ggplot(anual_TodosGCMs, aes(x=as.integer(aNo), y=tasmin, color=Scenario, group=Scenario)) + 
  geom_line() + 
  stat_smooth(method="loess", level=.8)

ggplot(anual_TodosGCMs, aes(x=as.integer(aNo), y=tasmax, color=Scenario, group=Scenario)) + 
  geom_line() + 
  stat_smooth(method="loess", level=.8)

ggplot(anual_TodosGCMs, aes(x=as.integer(aNo), y=pr, color=Scenario, group=Scenario)) + 
  geom_line() + 
  stat_smooth(method="loess", level=.8)

aNo_datos <- anual_TodosGCMs %>% group_by(aNo) %>% summarise(n())

datos_2012 <- anual_TodosGCMs %>% filter(aNo == 2012)
