library(tidyverse)

bravo <- read.csv("bravanza1.csv", encoding = "UTF-8") %>%
  rename(Estado = "X.U.FEFF.estado") %>%
  mutate(fecha = as.Date(fecha,"%d/%m/%Y"))
  

fecha<-"03-ago"

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

irag <- rbind(bravo,fasr) %>%
 mutate(fecha = as.Date(fecha,"%d/%m/%Y"))

remove(fasr,irag,jul26)

#mapa de calor para ocupación red IRAG##

g_hosp <- ggplot(bravo, aes(fecha,Estado,fill=hosp_gral))
g_vent <- ggplot(bravo, aes(fecha,Estado,fill=cama_venti))

g_hosp + 
  geom_tile(color="white",size=0.1)+
  scale_fill_viridis_c(name="Hospitalización general")+
  theme_minimal(base_size = 11)+
  labs(title = "Ocupación de camas de hospitalización general")+
  theme(legend.position = "bottom")
g_vent + 
  geom_tile(color="white",size=0.1)+
  scale_fill_viridis_c(name="Hospitalización general")+
  labs(title = "Ocupación de camas con ventilador") +
  theme_minimal(base_size = 11)+
  theme(legend.position = "bottom")

#serie de tiempo
hgral_nac <- aggregate(bravo[3],list(bravo$fecha),mean)%>%
  rename(fecha = "Group.1")
vent_nac <- aggregate(bravo[4],list(bravo$fecha),mean)%>%
  rename(fecha = "Group.1")

ggplot(hgral_nac, aes(hosp_gral))+
  geom_density()
  

nac <- cbind(hgral_nac,vent_nac)%>%
  select(-3)
nac<-pivot_longer(nac,2:3)

ggplot(nac, aes(fecha,value,group=name))+ 
  geom_line()+
  geom_point()

plot()

h <- ggplot(hgral_nac, aes(fecha,hosp_gral,gro))
v <- ggplot(vent_nac, aes(fecha,cama_venti))



h + geom_line() + geom_point() 
v + geom_line() + geom_point() 

scale_y_discrete(trans="reverse")
view(bravo)
