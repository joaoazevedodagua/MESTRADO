library(tidyverse)
library(readxl)
library(lubridate)
library(sf)


## passo 1 ler base
idh <- read_excel('C:/Users/Joao Azevedo/ownCloud/Joao/Bases/IDH_2010.xls')

## passo2 - ler arquivo municipo vs microregaio (cadmicro)
nomemicro <- read_csv('C:/Users/Joao Azevedo/ownCloud/Joao/Bases/nomes_microregiao.csv',locale = readr::locale(encoding = "latin1")) %>% 
  mutate(codmicro = as.character(codmicro)  )

cadmicro <-  read_csv2('C:/Users/Joao Azevedo/ownCloud/Joao/Bases/rl_municip_micibge.csv') %>% 
  filter(!str_detect(MUNCOD,'0000$')) %>% 
  left_join(nomemicro,by=c('MICROCOD' = 'codmicro'))


## passo3 - juntar idh com cadmicro e calcular por microregaio 
indices <- idh %>% 
            select(`Código do Município`,IDHM,`IDHM Educação`,`IDHM Longevidade`,`IDHM Renda`,`Índice de Gini`) %>%
            mutate(`Código do Município` = as.character(`Código do Município`)) %>%
            left_join(cadmicro,by=c('Código do Município'='MUNCOD'))
            
summary(indices)
  
  
idh2010_micro <-  indices  %>% 
            group_by(MICROCOD) %>%
            summarise(media_idh = mean(IDHM,na.rm = TRUE),
                      med_long = mean(`IDHM Longevidade`,na.rm = TRUE),
                      med_edu = mean(`IDHM Educação`, na.rm = TRUE),
                      med_renda = mean(`IDHM Renda`, na.rm = TRUE),
                      med_gini = mean(`Índice de Gini`, na.rm = TRUE))

      summary(idh2010_micro)      
## as vairiaveis IDH e Gini (media???/ usar summarise) e salvar como idh_micro


idh_cluster_BRASIL <- idh2010_micro %>%
        left_join(micros_clus_brasil,by='MICROCOD') %>%
        group_by(CLUSTER) %>%
        summarise(media_idh = mean(media_idh,na.rm = TRUE),
                  med_long = mean(med_long,na.rm = TRUE),
                  med_edu = mean(med_edu, na.rm = TRUE),
                  med_renda = mean(med_renda, na.rm = TRUE),
                  med_gini = mean(med_gini, na.rm = TRUE))
      
      
idh_cluster_BRASIL
      
sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/idh_cluster_BRASIL.txt')
idh_cluster_BRASIL
sink()
 
      
      
idh_cluster_NORTE <- idh2010_micro %>%
                left_join(micros_clus_norte,by='MICROCOD') %>%
                group_by(REGIAO,CLUSTER) %>%
                summarise(media_idh = mean(media_idh,na.rm = TRUE),
                          med_long = mean(med_long,na.rm = TRUE),
                          med_edu = mean(med_edu, na.rm = TRUE),
                          med_renda = mean(med_renda, na.rm = TRUE),
                          med_gini = mean(med_gini, na.rm = TRUE))

      
idh_cluster_NORTE

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_NORTE_todos/idh_cluster_NORTE.txt')
idh_cluster_NORTE
sink()



idh_cluster_NORDESTE <- idh2010_micro %>%
  left_join(micros_clus_nordeste,by='MICROCOD') %>%
  group_by(REGIAO,CLUSTER) %>%
  summarise(media_idh = mean(media_idh,na.rm = TRUE),
            med_long = mean(med_long,na.rm = TRUE),
            med_edu = mean(med_edu, na.rm = TRUE),
            med_renda = mean(med_renda, na.rm = TRUE),
            med_gini = mean(med_gini, na.rm = TRUE))


idh_cluster_NORDESTE

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_NORDESTE_todos/idh_cluster_NORDESTE.txt')
idh_cluster_NORDESTE
sink()



idh_cluster_SUDESTE <- idh2010_micro %>%
  left_join(micros_clus_sudeste,by='MICROCOD') %>%
  group_by(REGIAO,CLUSTER) %>%
  summarise(media_idh = mean(media_idh,na.rm = TRUE),
            med_long = mean(med_long,na.rm = TRUE),
            med_edu = mean(med_edu, na.rm = TRUE),
            med_renda = mean(med_renda, na.rm = TRUE),
            med_gini = mean(med_gini, na.rm = TRUE))


idh_cluster_SUDESTE

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_SUDESTE_todos/idh_cluster_SUDESTE.txt')
idh_cluster_SUDESTE
sink()


idh_cluster_SUL <- idh2010_micro %>%
  left_join(micros_clus_sul,by='MICROCOD') %>%
  group_by(REGIAO,CLUSTER) %>%
  summarise(media_idh = mean(media_idh,na.rm = TRUE),
            med_long = mean(med_long,na.rm = TRUE),
            med_edu = mean(med_edu, na.rm = TRUE),
            med_renda = mean(med_renda, na.rm = TRUE),
            med_gini = mean(med_gini, na.rm = TRUE))


idh_cluster_SUL

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_SUL_todos/idh_cluster_SUL.txt')
idh_cluster_SUL
sink()


idh_cluster_COESTE <- idh2010_micro %>%
  left_join(micros_clus_coeste,by='MICROCOD') %>%
  group_by(REGIAO,CLUSTER) %>%
  summarise(media_idh = mean(media_idh,na.rm = TRUE),
            med_long = mean(med_long,na.rm = TRUE),
            med_edu = mean(med_edu, na.rm = TRUE),
            med_renda = mean(med_renda, na.rm = TRUE),
            med_gini = mean(med_gini, na.rm = TRUE))


idh_cluster_COESTE

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_COESTE_todos/idh_cluster_COESTE.txt')
idh_cluster_COESTE
sink()



idh_clusters <- idh2010_micro %>%
  left_join(micros_clus_norte,micros_clus_nordeste,micros_clus_sudeste,micros_clus_sul,micros_clus_coeste, by='MICROCOD') %>%
  group_by(REGIAO,CLUSTER) %>%
  summarise(media_idh = mean(media_idh,na.rm = TRUE),
            med_long = mean(med_long,na.rm = TRUE),
            med_edu = mean(med_edu, na.rm = TRUE),
            med_renda = mean(med_renda, na.rm = TRUE),
            med_gini = mean(med_gini, na.rm = TRUE))


idh_cluster_COESTE[nrow(idh_cluster_COESTE),1:2] <- c('5','fora')

idh_clusters_ALL <- idh_cluster_COESTE %>% 
  bind_rows(idh_cluster_NORDESTE,idh_cluster_NORTE,
            idh_cluster_SUDESTE,idh_cluster_SUL)



idh_cluster_MULHER <- idh2010_micro %>%
  left_join(micros_clus_mulher,by='MICROCOD') %>%
  group_by(CLUSTER) %>%
  summarise(media_idh = mean(media_idh,na.rm = TRUE),
            med_long = mean(med_long,na.rm = TRUE),
            med_edu = mean(med_edu, na.rm = TRUE),
            med_renda = mean(med_renda, na.rm = TRUE),
            med_gini = mean(med_gini, na.rm = TRUE))


idh_cluster_MULHER

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_mulher/idh_cluster_MULHER.txt')
idh_cluster_MULHER
sink()


### passo 4 juntar resultado satscaan com idh_micro
## criar TABELA com idh e gini por cluster e resto (fora do cluster)
## novamente summarise por cluster do IDH e GINI





###### MEDIA E MEDIANA DE IDADE POR CLUSTER  ############


idade_cluster_BRASIL <- hiv.micro %>%
  left_join(micros_clus_brasil,by='MICROCOD') %>%
  group_by(CLUSTER) %>%
  summarise(media_idade_cluster = mean(idade,na.rm = TRUE),
            MEDIANA_idade_cluster = median(idade,na.rm = TRUE))

idade_cluster_BRASIL

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/idade_cluster_BRASIL.txt')
idade_cluster_BRASIL
sink()


idade_cluster_NORTE <- hiv.micro %>%
  left_join(micros_clus_norte,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) %>%
  summarise(media_idade_cluster = mean(idade,na.rm = TRUE),
            MEDIANA_idade_cluster = median(idade,na.rm = TRUE))

idade_cluster_NORTE

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_NORTE_todos/idade_cluster_NORTE.txt')
idade_cluster_NORTE
sink()



idade_cluster_NORDESTE <- hiv.micro %>%
  left_join(micros_clus_nordeste,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) %>%
  summarise(media_idade_cluster = mean(idade,na.rm = TRUE),
            MEDIANA_idade_cluster = median(idade,na.rm = TRUE))

idade_cluster_NORDESTE

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_NORDESTE_todos/idade_cluster_NORDESTE.txt')
idade_cluster_NORDESTE
sink()



idade_cluster_SUDESTE <- hiv.micro %>%
  left_join(micros_clus_sudeste,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) %>%
  summarise(media_idade_cluster = mean(idade,na.rm = TRUE),
            MEDIANA_idade_cluster = median(idade,na.rm = TRUE))

idade_cluster_SUDESTE

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_SUDESTE_todos/idade_cluster_SUDESTE.txt')
idade_cluster_SUDESTE
sink()



idade_cluster_SUL <- hiv.micro %>%
  left_join(micros_clus_sul,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) %>%
  summarise(media_idade_cluster = mean(idade,na.rm = TRUE),
            MEDIANA_idade_cluster = median(idade,na.rm = TRUE))

idade_cluster_SUL

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_SUL_todos/idade_cluster_SUL.txt')
idade_cluster_SUL
sink()



idade_cluster_COESTE <- hiv.micro %>%
  left_join(micros_clus_coeste,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) %>%
  summarise(media_idade_cluster = mean(idade,na.rm = TRUE),
            MEDIANA_idade_cluster = median(idade,na.rm = TRUE))

idade_cluster_COESTE

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_COESTE_todos/idade_cluster_COESTE.txt')
idade_cluster_COESTE
sink()


idade_cluster_MULHER <- hiv.micro %>%
  left_join(micros_clus_mulher,by='MICROCOD') %>%
  group_by(CLUSTER) %>%
  summarise(media_idade_cluster = mean(idade,na.rm = TRUE),
            MEDIANA_idade_cluster = median(idade,na.rm = TRUE))

idade_cluster_MULHER

sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_mulher/idade_cluster_MULHER.txt')
idade_cluster_MULHER
sink()

########### RAZAO DE SEXO POR CLUSTER ##########

razao_BRASIL <- hiv.micro %>%
  left_join(micros_clus_brasil,by='MICROCOD') %>%
  group_by(CLUSTER) 

BRASIL_F <- razao_BRASIL  %>% filter(sexo =='F') %>% count()
BRASIL_M <- razao_BRASIL  %>% filter(sexo =='M') %>% count()


razao_BRASIL <-  BRASIL_F %>%
  left_join(BRASIL_M,by='CLUSTER') %>%
  summarise(razao = n.y/n.x,
            prop = n.y/(n.y+n.x) )

razao_BRASIL  
sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_todos/razao_BRASIL.txt')
razao_BRASIL
sink()


razao_NORTE <- hiv.micro %>%
  left_join(micros_clus_norte,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) 

  NORTE_F <- razao_NORTE  %>% filter(sexo =='F') %>% count()
  NORTE_M <- razao_NORTE  %>% filter(sexo =='M') %>% count()
  
    
razao_NORTE <-  NORTE_F %>%
    left_join(NORTE_M,by='CLUSTER') %>%
    summarise(razao = n.y/n.x,
            prop = n.y/(n.y+n.x) )
  
razao_NORTE  
sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_NORTE_todos/razao_NORTE.txt')
razao_NORTE
sink()



razao_NORDESTE <- hiv.micro %>%
  left_join(micros_clus_nordeste,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) 

NORDESTE_F <- razao_NORDESTE  %>% filter(sexo =='F') %>% count()
NORDESTE_M <- razao_NORDESTE  %>% filter(sexo =='M') %>% count()


razao_NORDESTE <-  NORDESTE_F %>%
  left_join(NORDESTE_M,by='CLUSTER') %>%
  summarise(razao = n.y/n.x,
            prop = n.y/(n.y+n.x) )

razao_NORDESTE  
sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_NORDESTE_todos/razao_NORDESTE.txt')
razao_NORDESTE
sink()



razao_SUDESTE <- hiv.micro %>%
  left_join(micros_clus_sudeste,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) 

SUDESTE_F <- razao_SUDESTE  %>% filter(sexo =='F') %>% count()
SUDESTE_M <- razao_SUDESTE  %>% filter(sexo =='M') %>% count()

razao_SUDESTE <-  SUDESTE_F %>%
  left_join(SUDESTE_M,by='CLUSTER') %>%
  summarise(razao = n.y/n.x,
            prop = n.y/(n.y+n.x) )

razao_SUDESTE  
sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_SUDESTE_todos/razao_SUDESTE.txt')
razao_SUDESTE
sink()



razao_SUL <- hiv.micro %>%
  left_join(micros_clus_sul,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) 

SUL_F <- razao_SUL  %>% filter(sexo =='F') %>% count()
SUL_M <- razao_SUL  %>% filter(sexo =='M') %>% count()


razao_SUL <-  SUL_F %>%
  left_join(SUL_M,by='CLUSTER') %>%
  summarise(razao = n.y/n.x,
            prop = n.y/(n.y+n.x) )

razao_SUL  
sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_SUL_todos/razao_SUL.txt')
razao_SUL
sink()



razao_COESTE <- hiv.micro %>%
  left_join(micros_clus_coeste,by='MICROCOD') %>%
  group_by(REGIAO, CLUSTER) 

COESTE_F <- razao_COESTE  %>% filter(sexo =='F') %>% count()
COESTE_M <- razao_COESTE  %>% filter(sexo =='M') %>% count()


razao_COESTE <-  COESTE_F %>%
  left_join(COESTE_M,by='CLUSTER') %>%
  summarise(razao = n.y/n.x,
            prop = n.y/(n.y+n.x) )

razao_COESTE  
sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_COESTE_todos/razao_COESTE.txt')
razao_COESTE
sink()



razao_MULHER <- hiv.micro %>%
  left_join(micros_clus_mulher,by='MICROCOD') %>%
  group_by(CLUSTER) 

MULHER_F <- razao_MULHER  %>% filter(sexo =='F') %>% count()
MULHER_M <- razao_MULHER  %>% filter(sexo =='M') %>% count()


razao_MULHER <-  MULHER_F %>%
  left_join(MULHER_M,by='CLUSTER') %>%
  summarise(razao = n.y/n.x,
            prop = n.y/(n.y+n.x) )

razao_MULHER  
sink('C:/Users/Joao Azevedo/ownCloud/Joao/satscan_BR_mulher/razao_MULHER.txt')
razao_MULHER
sink()



 nomesBRASIL <- micros_clus_brasil %>%
                left_join(nomemicro,by=c('MICROCOD' = 'codmicro'))
nomesBRASIL 


nomesMULHER <- micros_clus_mulher %>%
  left_join(nomemicro,by=c('MICROCOD' = 'codmicro'))
nomesBRASIL