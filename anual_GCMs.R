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
  summarize(pr_GCM = sum (Value * 86400), prmax_dia = max (Value * 86400))

tasmax_anual_GCM <- liberiaGCMs %>%
  filter(Variable == "tasmax") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>% 
  summarize(tasmax_aNo = mean (Value - 273.15), tasmax_dia = max (Value - 273.15))

tasmin_anual_GCM <- liberiaGCMs %>%
  filter(Variable == "tasmin") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(aNo, Scenario, Model) %>% 
  summarize(tasmin_aNo = mean (Value - 273.15), tasmin_max_dia = max (Value - 273.15))

anual_GCMs <- pr_anual_GCM %>% inner_join(tasmax_anual_GCM, by = c("aNo", "Scenario", "Model")) %>% inner_join(tasmin_anual_GCM, by = c("aNo", "Scenario", "Model"))

anual_GCMs <- anual_GCMs %>% rename(Modelo = Model, pr = pr_GCM, tasmax = tasmax_aNo, tasmin = tasmin_aNo)

saveRDS(anual_GCMs, "anual_GCMs.rds")

per_5_95 <- anual_GCMs %>% 
  filter(Scenario == "historical") %>% group_by(Scenario) %>% 
  summarise(`tasmax_5%`=quantile(tasmax, probs=0.05),
            `tasmax_95%`=quantile(tasmax, probs=0.95),
            `tasmin_5%`=quantile(tasmin, probs=0.05),
            `tasmin_95%`=quantile(tasmin, probs=0.95),
            `pr_5%`=quantile(pr, probs=0.05), 
            `pr_95%`=quantile(pr, probs=0.95))

anual_GCMs %>% filter(Scenario == "historical") %>% group_by(Scenario) %>% summarise(min(aNo), max(aNo))

saveRDS(per_5_95, "perc_anual_GCMs.rds")

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

datos_2039 <- anual_TodosGCMs %>% filter(aNo == 2039)


## número de días

p90tmax <- liberiaGCMs %>% 
  filter(Variable == "tasmax") %>% 
  mutate(aNo = format(Date, "%Y")) %>%
  group_by(Model, aNo) %>% 
  summarize(tmax_aNo = max(Value))
  mutate(p90max = )
  

#estacionalidad
#hacer por meses

tasmax_mensual <- liberiaGCMs %>%
  filter(Variable == "tasmax") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(mes, aNo, Scenario, Model) %>% 
  summarize(tasmax_mes = mean (Value - 273.15))

tasmin_mensual <- liberiaGCMs %>%
  filter(Variable == "tasmin") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(mes, aNo, Scenario, Model) %>% 
  summarize(tasmin_mes = mean (Value - 273.15))

pr_mensual <- liberiaGCMs %>% 
  filter(Variable == "pr") %>%
  mutate(mes = format(Date, "%m"), aNo = format(Date, "%Y")) %>% 
  group_by(mes, aNo, Scenario, Model) %>% 
  summarize(pr_mes = sum (Value * 86400))

mensual_GCMs <- pr_mensual %>% inner_join(tasmax_mensual, by = c("aNo", "mes", "Scenario", "Model")) %>% inner_join(tasmin_mensual, by = c("aNo", "mes", "Scenario", "Model"))

mensual_GCMs <- mensual_GCMs %>% rename(Modelo = Model)

saveRDS(mensual_GCMs, "mensual_GCMs.rds")

#media de datos históricos
media_historico <- mensual_GCMs %>% 
  filter(Scenario == "historical") %>% group_by(mes) %>% 
  summarise(tasmax_media=median(tasmax_mes),
            tasmin_media=median(tasmin_mes),
            pr_media=median(pr_mes))

saveRDS(media_historico, "media_historico_mensual.rds")

ggplot(pr_mensual, aes(x=mes, y=pr_mes)) + geom_violin() + scale_y_log10() + geom_point(data = media_historico, aes(x=mes, y=pr_media), shape = 18, size = 3)

ggplot(tasmax_mensual, aes(x=mes,y=tasmax_mes)) + geom_violin() + stat_summary(fun.y=median, geom="point", size=2, color="red")

