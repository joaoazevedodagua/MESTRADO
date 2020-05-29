## Satscan HIV
library(tidyverse)
library(sf)
library(rsatscan)

load('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/resultado_TODOS_satscan.RData')
load('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/casos_todos.RData')

rcol_todos 

tempo_todos <- as_tibble(result_hiv$gis)  %>% 
  left_join(rcol_todos ,by='CLUSTER') %>% 
  mutate(LOC_ID= as.character(LOC_ID.x))



### GrÃ¡ficos casos x tempo clusters

date <- 1985:2016
micro <- unique(fx13mais$MICROCOD)
dia <- tibble(MICROCOD=rep(micro,each=32),anodiag=rep(date,length(micro)))


casos <- dia %>% left_join(fx13mais,by=c('MICROCOD','anodiag'))


#rcol_todos2 <-  rcol_todos %>%
#  select(CLUSTER,START_DATE,END_DATE) 

cluster_anos <-  casos %>% 
  left_join(tempo_todos, by=c('MICROCOD'='LOC_ID')) %>% 
  mutate (START_DATE = as.numeric(as.character(START_DATE)),
          END_DATE = as.numeric(as.character(END_DATE)) ) %>% 
  filter (P_VALUE.x <= 0.05) %>% 
  mutate (status = ifelse(anodiag >= START_DATE  & anodiag <= END_DATE,1,0)) %>%
  group_by(CLUSTER,anodiag,status) %>% 
  summarise(casos = sum(n,na.rm=T)) 


cluster_aux <- cluster_anos %>% 
  mutate(   x =  ifelse(status==1, anodiag,1985),
            y = ifelse(status==1, casos,NA_real_))



# casos %>% 
# left_join(tempo_todos, by=c('MICROCOD'='LOC_ID')) %>% 
# mutate (START_DATE = as.numeric(as.character(START_DATE)),
#         END_DATE = as.numeric(as.character(END_DATE)), 
#        status = ifelse(anodiag >= START_DATE  & anodiag <= END_DATE,1,0),
#         x =  ifelse(status==1, anodiag,1985),
#         y = ifelse(status==1, n,NA_real_))



cluster_tempo_todos <-  ggplot(cluster_anos,aes(anodiag,casos)) +
  geom_area( color="darkblue", fill="lightblue")   +
  geom_area( data= cluster_aux,aes(x,y,fill = 'red')) +
  facet_wrap(~CLUSTER,ncol = 1,strip.position='left') +
  scale_x_continuous(breaks = seq(1985,2016,by=5)) +
  theme_light()

cluster_tempo_todos
ggsave(cluster_tempo_todos,filename = 'C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/grafico_tempo_todos.eps')

# 
# novo_tema <- theme_light()
# novo_tema$panel.grid.major$colour <- 'white'
# novo_tema$panel.grid.minor$colour <- 'white'

