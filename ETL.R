library(sidrar)
library(tidyverse)

servicos_sanitarios_basicos <- #https://data360files.worldbank.org/data360-data/data/WB_WDI/WB_WDI_SH_STA_BASS_ZS.csv
  read_csv("WB_WDI_SH_STA_BASS_ZS.csv") %>%
  janitor::clean_names()

servicos_sanitarios_basicos_trabalho<-
  servicos_sanitarios_basicos %>%
  select(ref_area_id,ref_area_name, unit_measure_name, time_period, obs_value )


saveRDS(servicos_sanitarios_basicos_trabalho, "servicos_sanitarios_basicos_trabalho.rds")



####Condições dos domicílios

## Usar como referência para domicílios de um modo geral
#https://sidra.ibge.gov.br/pesquisa/censo-demografico/demografico-2022/universo-caracteristicas-dos-domicilios

## Usar como referência para domicílios em favelas
#https://sidra.ibge.gov.br/pesquisa/censo-demografico/demografico-2022/universo-favelas-e-comunidades-urbanas

estrutura_dados<- function(.data){
  .data<-
    .data%>%
    select(D1C, D1N, D2N, D3N, D4N, V)
  
  names(.data)<- c("codigo_ibge", "municipio", "ano", "escopo", "variavel", "valor")
  
  .data
}



info_sidra(6806, wb = TRUE)

domicilios_municipios_banheiro<-
  get_sidra(x = 6806,
            variable = c(1000381),
            #period = c("last" = 12),
            geo = "City",
            #geo.filter = "RS",
            classific = "C458",
            category =  list(c(12032 )), #, 72118,72119, 12046
            header = FALSE,
            format = 3)

banheiro_trabalho<- estrutura_dados(domicilios_municipios_banheiro)


saveRDS(banheiro_trabalho, "banheiro_trabalho.rds") 

info_sidra(6326, wb = TRUE)





domicilio_municipios_tipo_casa<-
  get_sidra(x = 6326,
            variable = c(1000381),
            #period = c("last" = 12),
            geo =  "City",
            #geo.filter = "RS",
            classific = "C125",
            category =  list(c(6815 )), 
            header = FALSE,
            format = 3)

casa_trabalho <- estrutura_dados(domicilio_municipios_tipo_casa)

saveRDS(casa_trabalho, "casa_trabalho.rds")


domicilio_municipios_tipo_apartamento<-
  get_sidra(x = 6326,
            variable = c(1000381),
            #period = c("last" = 12),
            geo =  "City",
            #geo.filter = "RS",
            classific = "C125",
            category =  list(c(3247 )), 
            header = FALSE,
            format = 3)


apartamento_trabalho <- estrutura_dados(domicilio_municipios_tipo_apartamento)

saveRDS(apartamento_trabalho,"apartamento_trabalho.rds")


info_sidra(6804, wb = TRUE)

domicilio_municipios_tipo_abastecimento_agua<-
  get_sidra(x = 6804,
            variable = c(1000381),
            #period = c("last" = 12),
            geo =  "City",
            #geo.filter = "RS",
            classific = "c301",
            category =  list(c(31471)), 
            header = FALSE,
            format = 3)

agua_trabalho<- estrutura_dados(domicilio_municipios_tipo_abastecimento_agua)

saveRDS(agua_trabalho,"agua_trabalho.rds")


domicilio_municipios_tipo_abastecimento_agua %>%
  mutate(tipo_municipio = ifelse(str_detect( str_to_lower(D1N),"água"), 
                                 "Município com água no nome",
                                 "Município sem água no nome")) %>%
  ggplot(aes(x=tipo_municipio, y= V)) +
  geom_boxplot() +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "lightblue"),
    panel.grid =  element_blank()
  ) +
  labs(title = "Abastecimento de água da rede geral de distribuição ",
       subtitle =  "Proporção de residências por cidade",
       y= "%",
       x="",
       caption= "Fonte: Censo 2022 - IBGE")
