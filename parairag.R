library(tidyverse)

#bravo <- read.csv("irag.csv", encoding = "UTF-8") %>%
 # select(-X.U.FEFF.) %>%
  #mutate(fecha = as.Date(fecha,"%d/%m/%Y"))
  

#comienza cada día, cambiar###
fecha<-"2021-08-04"

hospgral <- read.csv("Sistema de Información de la Red IRAG.csv", 
                     encoding = "UTF-8") %>%
  rename(hosp_gral = "X..Ocupación")
hospvent <- read.csv("Sistema de Información de la Red IRAG (1).csv", 
                     encoding = "UTF-8") %>%
  select(-Estado)%>%
  rename(cama_venti = "X..Ocupación")
hosventuci <- read.csv("Sistema de Información de la Red IRAG (2).csv", 
                     encoding = "UTF-8")%>%
  select(-Estado)%>%
  rename(venti_uci = "X..Ocupación")

fasr <- cbind(fecha,hospgral,hospvent,hosventuci)
fasr = fasr [ , (c(names(bravo)))]

#df principal
bravo <- read.csv("irag.csv", encoding = "UTF-8") %>%
  select(-X.U.FEFF.) %>%
  mutate(fecha = as.Date(fecha,"%Y-%m-%d"))

#df final
irag <- rbind(bravo,fasr) %>%
 mutate(fecha = as.Date(fecha,"%d-%m"))

write.csv(irag,"irag.csv")


remove(fasr,hospgral,hospvent,hosventuci)

#mapa de calor para ocupación red IRAG##

g_hosp <- ggplot(irag, aes(fecha,Estado,fill=hosp_gral))
g_vent <- ggplot(irag, aes(fecha,Estado,fill=cama_venti))

hm_hosp<- g_hosp + 
  geom_tile(color="white",size=0.1)+
  scale_fill_viridis_c(name="Hospitalización general")+
  theme_minimal(base_size = 11)+
  labs(title = "Ocupación de camas de hospitalización general")+
  theme(legend.position = "bottom")
hm_vent <- g_vent + 
  geom_tile(color="white",size=0.1)+
  scale_fill_viridis_c(name="Hospitalización general")+
  labs(title = "Ocupación de camas con ventilador") +
  theme_minimal(base_size = 11)+
  theme(legend.position = "bottom")

#SERIE TEMPORAL PORCETUAL
#subdf medias → por fecha
hgral_nac <- aggregate(bravo[3],list(bravo$fecha),mean)%>%
  rename(fecha = "Group.1")
vent_nac <- aggregate(bravo[4],list(bravo$fecha),mean)%>%
  rename(fecha = "Group.1")
nac <- cbind(hgral_nac,vent_nac)%>%   #subset
  select(-3)
nac<-pivot_longer(nac,2:3)%>% #ocupación como dicotómica
  rename(tipo_ocupación = "name", ocupación_p = "value")

##gráfica##  
tsirag <- ggplot(nac, aes(fecha,ocupación_p,group=tipo_ocupación))+ 
  geom_line()+
  geom_point()

hm_vent #heatmap irag ventilador
hm_hosp #heatmap irag generales
tsirag #line irag

h <- ggplot(hgral_nac, aes(fecha,hosp_gral,gro))
v <- ggplot(vent_nac, aes(fecha,cama_venti))


h + geom_line() + geom_point() 
v + geom_line() + geom_point() 



