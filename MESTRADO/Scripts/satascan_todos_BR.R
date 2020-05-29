## Satscan HIV
library(tidyverse)
library(sf)
library(rsatscan)


## carregado dados 
## hiv.micro (casos ind) 1.4 Gb
## proj.all (projeções pop por anox e fx)
## cent.geom (centroides restriros ao interior do poligono)
load('C:/Users/Joao Azevedo/ownCloud/Joao/Rdata/Arq_satscan_19-Ago.RData')

mapa <- read_sf('C:/Users/Joao Azevedo/ownCloud/Joao/shape/microregiao_wgs84.shp')
## removendo menores de 13 anos e contando casos por Microregião e Ano diag
## e sexo M
hiv.cnt <- hiv.micro %>% 
  filter(fetar != 'FX0a13'  ) %>% 
  group_by(MICROCOD,anodiag) %>% count()


## pop mais de 13 anos todoses
pop13mais <- proj.all %>% 
  filter(sexo == 'Total') %>%  
  mutate(FXmais13 = FX14a24 + FX25a34 + FX35a54 +FX55mais) %>% 
  select( micro ,ano, FXmais13)

## junatado casos e pop por microreggiao 
## remove a micro 52998
fx13mais <- hiv.cnt %>% 
  left_join(pop13mais,by=c('MICROCOD' = 'micro','anodiag' = 'ano')) %>% 
  filter(!is.na(MICROCOD) & MICROCOD != '52998' )

save(fx13mais,file='C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/casos_todos.RData')
### parametros para o satscan

### Fazendo o SatScan

ARQ <- 'hiv_fx13mais_total.cas' ## nome do arquivo de saida


ss.local <- "C:/Program Files/SaTScan"  ## onde esta instalado

## hack para rodar pois a lib rsatscan esta desatualizada 
ssenv$.ss.params.OLD <-  ssenv$.ss.params
ssenv$.ss.params <- readLines('C:/Users/Joao Azevedo/ownCloud/Joao/Scripts/template_satscan.txt')


## arq casos MICROREG,Casos, Tempo  
## diretorio para saida dos dados

local <- 'C:/Users/JOAOAZ~1/ownCloud/Joao/satscan_BR_todos/'

casos <- paste0(local,'hivtot.cas')
pop <- paste0(local,'hivtot.pop')
geo <- paste0(local,'hivtot.geo')

### local, casos, data 
write.table(fx13mais[,c(1,3,2)],file=casos , 
            row.names = FALSE,col.names = FALSE,qmethod = "double",fileEncoding = "latin1")

## Arq populacao
write.table(fx13mais[,c(1,2,4)],file=pop , 
            row.names = FALSE,col.names = FALSE,qmethod = "double",fileEncoding = "latin1")


## Arq centroides 
#st_geometry(cent.geom) <- NULL
write.table(cent.geom[,c(2,4,3)],file=geo , 
            row.names = FALSE,col.names = FALSE,qmethod = "double",fileEncoding = "latin1")


#

## opcoes

ss.options(list(CaseFile=casos,
                StartDate=1985,
                EndDate=2016, 
                PrecisionCaseTimes=4 ,      
                PopulationFile=pop,
                CoordinatesFile=geo, 
                CoordinatesType=1,          
                AnalysisType=3,
                ModelType=0,
                TimeAggregationUnits=4,
                TimeAggregationLength=1,
                ScanAreas=1,
                TimeAggregationLength=1,
                MaxSpatialSizeInPopulationAtRisk=50,
                MinimumTemporalClusterSize=1,
                MaxTemporalSize=25,
                UseDistanceFromCenterOption='n',
                MinimumCasesInHighRateClusters=50,
                MaxSpatialSizeInDistanceFromCenter_Reported=250, ## distancia 
                MaxSpatialSizeInPopulationAtRisk_Reported=5  #está como padrão 5, para mudar mexer aqui
))
ss.options(c("NonCompactnessPenalty=0", "ReportGiniClusters=n
             ", "LogRunToHistoryFile=n"))

modelo <- paste0('hiv','_modelo_13mais_todos')
write.ss.prm(local,modelo)

## rodando modelo

result_hiv <-  satscan(local,modelo, sslocation='C:/Program Files/SaTScan/')
summary(result_hiv)

rcol_todos <- result_hiv$col %>% 
  select (CLUSTER,LOC_ID,RADIUS:NUMBER_LOC,REL_RISK,POPULATION,P_VALUE)


sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/rcol_TODOS.txt')
rcol_todos
sink()

micros_clus_brasil <-  result_hiv$gis %>%
  select (CLUSTER,LOC_ID) %>%
  mutate(MICROCOD = as.character(LOC_ID)) %>%
  mutate(REGIAO = str_sub(MICROCOD,1,1))

micros_clus_brasil

mapa.tmp <- mapa

mapa.hiv <- mapa.tmp %>% 
  left_join(result_hiv$gis ,by=c('CD_GEOCMI'='LOC_ID')) %>% 
  left_join(result_hiv$rr,by=c('CD_GEOCMI'='LOC_ID')) 
 

mapa.hiv <- mapa.hiv %>% mutate (SIGNI = ifelse(P_VALUE <= 0.05,TRUE,FALSE))

mapa_clus <- ggplot(mapa.hiv) + 
  geom_sf(aes(fill=factor(CLUSTER)),size=0.1) +
  #geom_sf(aes(fill=SIGNI)) +
  scale_fill_discrete(na.value = 'snow1') +
  #ggrepel::geom_label_repel(data=result_hiv$col,aes(x=LONGITUDE,y=LATITUDE,label=CLUSTER)) +
  theme_light() +
  theme(legend.position = 'none')
mapa_clus

mapa_clus_label <- ggplot(mapa.hiv) + 
  geom_sf(aes(fill=factor(CLUSTER)),size=0.1) +
  #geom_sf(aes(fill=SIGNI)) +
  scale_fill_discrete(na.value = 'snow1') +
  ggrepel::geom_label_repel(data=result_hiv$col,aes(x=LONGITUDE,y=LATITUDE,label=CLUSTER)) +
  theme_light() +
  theme(legend.position = 'none')
mapa_clus_label

mapa_rr <- ggplot(mapa.hiv) + 
  geom_sf(aes(fill=REL_RISK),size=0.1) +
  scale_fill_gradient2(low='snow',mid = 'white',high = 'red',midpoint = .8) +
  theme_light() 
  #theme(legend.position = 'none')
mapa_rr

ggsave('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_clus.png',mapa_clus,height = 8,width = 12)
ggsave('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_clus_TODOS.eps',mapa_clus,height = 8,width = 12)

ggsave('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_clus_label.png',mapa_clus_label,height = 8,width = 12)
ggsave('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_clus_label_TODOS.eps',mapa_clus_label,height = 8,width = 12)


ggsave('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_rr.png',mapa_rr,height = 8,width = 12)
ggsave('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_rr_TODOS.eps',mapa_rr,height = 8,width = 12)


ggplot(mapa.hiv) + 
  geom_sf(aes(fill=REL_RISK),size=0.1) +
  scale_fill_gradient2(low='#ffffffff',mid = '#ffffffa0',high = 'red',midpoint = 1) +
  geom_sf(aes(color=factor(CLUSTER)),alpha=0.3,size=0.1) +
   theme_light() +
  theme(legend.position = 'none')


mapa.hiv$risco <- cut(mapa.hiv$REL_RISK,breaks = c(0,1.1,1.5,2.0,10),labels = c('sem risco','baixo','medio','alto'))

ggplot(mapa.hiv) +
  geom_sf(aes(fill=risco),size=0.1) +
  scale_fill_manual(values = c('snow','yellow','orange','red')) +
  theme_light()
  
mapa.hiv$relrisk <- cut(mapa.hiv$REL_RISK,br=c(0,0.5,1,1.2,2,5),include.lowest = T)

risco_rel <- ggplot(mapa.hiv) +
  geom_sf(aes(fill=relrisk),size=0.1) +
  #scale_fill_gradient2(low='#00FF0040',mid = 'white',high = 'red',midpoint = 1.0) +
  scale_fill_brewer(palette = 'RdYlBu',type='div',direction = -1) +
  theme_light()
risco_rel

ggsave('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_risco_corte.png',risco_rel,height = 8,width = 12)
ggsave('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_risco_corte_TODOS.eps',risco_rel,height = 8,width = 12)

write_sf(mapa.hiv,dsn='C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_hiv_TODOS.geojson', delete_dsn=TRUE)

write_sf(mapa.hiv,dsn='C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/mapa_hiv_TODOS.kml' ,delete_dsn=TRUE)

save(result_hiv,file='C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/resultado_TODOS_satscan.RData')
