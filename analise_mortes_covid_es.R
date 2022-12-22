library(data.table)
library(stringr)
library(ggplot2)
library(dplyr)
library(forcats)
library(pROC)
library(readr)

#Baixando os dados
readr::guess_encoding("https://bi.s3.es.gov.br/covid19/MICRODADOS.csv")
dados <- as.data.table(
  read.csv('https://bi.s3.es.gov.br/covid19/MICRODADOS.csv', fileEncoding = "UTF-8")
)

#Filtrando região metropolitana
dados_metro <- dados[Municipio %in% c("CARIACICA", "FUNDAO", "GUARAPARI", "SERRA", "VIANA", "VILA VELHA", "VITORIA")]

#Categorizando dados
dados_metro <- dados_metro[
  ,
  `:=` (
    grupo_etario = fcase(
      FaixaEtaria %in% c("0 a 4 anos", "05 a 9 anos", "10 a 19 anos"), "crianca",
      FaixaEtaria %in% c("20 a 29 anos", "30 a 39 anos", "40 a 49 anos"), "jovens",
      FaixaEtaria %in% c("50 a 59 anos", "60 a 69 anos"), "adultos",
      FaixaEtaria %in% c("70 a 79 anos", "80 a 89 anos", "90 anos ou mais"), "idosos"
    ),
    morte_covid = fcase(
      Evolucao == "Óbito pelo COVID-19", "Sim",
      !Evolucao == "Óbito pelo COVID-19", "Não"
    ),
    comorbidade = fcase(
      ComorbidadeCardio == "Sim" | ComorbidadePulmao == "Sim" |
        ComorbidadeRenal == "Sim" | ComorbidadeDiabetes == "Sim" |
        ComorbidadeTabagismo == "Sim" | ComorbidadeObesidade == "Sim", "Sim",
      ComorbidadeCardio == "Não" & ComorbidadePulmao == "Não" &
        ComorbidadeRenal == "Não" & ComorbidadeDiabetes == "Não" &
        ComorbidadeTabagismo == "Não" & ComorbidadeObesidade == "Não", "Não"
    ),
    sintomatico = fcase(
      DificuldadeRespiratoria == "Sim" | Febre == "Sim"  |
        Tosse == "Sim" | Coriza == "Sim" | DorGarganta == "Sim" |
        Diarreia == "Sim" | Cefaleia == "Sim",                               "Sim",
      DificuldadeRespiratoria == "Não" & Febre == "Não" &
        Tosse == "Não" & Coriza == "Não" & DorGarganta == "Não" &
        Diarreia == "Não" & Cefaleia == "Não",                               "Não"
    ),
    idade = as.numeric(
      str_trim(str_sub(IdadeNaDataNotificacao, end=2))
    )
  )
  ]

#Filtrando banco retirando pessoas menores que 19 anos e apenas pessoas que testaram positivo para covid 19
dados_metro <- dados_metro[grupo_etario != "crianca"]
dados_metro <- dados_metro[Classificacao == "Confirmados"]
dados_metro <- dados_metro[idade >= 20]

#Análise Descritiva
attach(dados_metro)
#Morte por grupo etário
dados_metro %>% 
  ggplot() +
  aes(x = morte_covid, fill = grupo_etario) + 
  geom_bar(position = 'fill') + 
  theme_minimal() + 
  scale_fill_grey() +
  labs(y = 'Frequência', 
       x = "Morte por Covid-19",
       title = "Morte por covid por grupo etário")

#Morte por sintomático
dados_metro %>% 
  ggplot() +
  aes(x = morte_covid, fill = sintomatico) + 
  geom_bar(position = 'fill') + 
  theme_minimal() + 
  scale_fill_grey() +
  labs(y = 'Frequência', 
       x = "Morte por Covid-19",
       title = "Morte por covid por Sintomático")

chisq.test(table(morte_covid,sintomatico))

#Sintomatico por Comorbidade
dados_metro %>% 
  ggplot() +
  aes(x = sintomatico, fill = comorbidade) + 
  geom_bar(position = 'fill') + 
  theme_minimal() + 
  scale_fill_grey() +
  labs(y = 'Frequência', 
       x = "Sintomático",
       title = "Sintomático por Comorbidade")

chisq.test(table(comorbidade,sintomatico))

#Idade por comorbidade
dados_metro%>% 
  ggplot() +
  aes(x = idade, fill = comorbidade) +
  geom_histogram(position = 'fill') +
  theme_minimal() + 
  scale_fill_grey() +
  labs(y = 'Frequência', 
       x = "Idade",
       title = "Idade por Comorbidade")

## TABELA 2X2 DO GRUPO ETARIO VERSUS MORTE POR COVID-19
dados_tabela1 <- janitor::clean_names(dados_metro) %>%
  mutate(grupo_etario = forcats::fct_recode(FaixaEtaria,
                                            Jovens = "20 a 29 anos",
                                            Jovens = "30 a 39 anos",
                                            Jovens = "40 a 49 anos",
                                            Adultos = "50 a 59 anos",
                                            Adultos = "60 a 69 anos",
                                            Idosos = "70 a 79 anos",
                                            Idosos = "80 a 89 anos",
                                            Idosos = "90 anos ou mais")) %>%
  filter(grupo_etario %in% c("Adultos", "Idosos")) %>%
  mutate(morte_covid = ifelse(evolucao == "Óbito pelo COVID-19",
                              "Sim", "Não"),
         grupo_etario = forcats::fct_drop(grupo_etario)) %>%
  mutate(grupo_etario = forcats::fct_relevel(grupo_etario,
                                             "Idosos", "Adultos"),
         morte_covid = forcats::fct_relevel(morte_covid,
                                            "Sim", "Não"))
tabela1 <- summarytools::ctable(dados_tabela1$grupo_etario,
                                dados_tabela1$morte_covid, RR = TRUE,
                                OR = TRUE, headings = FALSE)

tabela1

## TABELA 2X2 DE COMORBIDADE VERSUS MORTE POR COVID-19

dados_tabela2 <-janitor::clean_names(dados_metro) %>%
  mutate(comorbidade = forcats::fct_relevel(comorbidade,
                                            "Sim", "Não"),
         morte_covid = forcats::fct_relevel(morte_covid,
                                            "Sim", "Não"))

tabela2 <- summarytools::ctable(dados_tabela2$comorbidade,
                                dados_tabela2$morte_covid, RR = TRUE,
                                OR = TRUE, headings = FALSE)
tabela2

## LIMPEZA DOS DADOS PARA MODELAGEM
#Filtrando variaveis e criando dummy numérica para morte covid
dados_modelo <- dados_metro[
  ,
  `:=` (
    morte_covid_num = fcase(
      morte_covid == "Sim", 1L,
      morte_covid == "Não", 0L
    ),
    filtro = fcase(
      Sexo == "I" | RacaCor == "Ignorado" | 
        ProfissionalSaude == "Ignorado" | 
        PossuiDeficiencia == "Não Informado" | 
        MoradorDeRua == "Não Informado", "filtrar"
    )
  )
  ]

#Filtrando
dados_modelo <- dados_metro[is.na(filtro)]
dados_modelo$filtro <- NULL

## MODELAGEM

modelo1 <- glm(morte_covid_num ~ comorbidade + sintomatico +
                 idade, family=binomial, data=dados_modelo)

summary(modelo1)

require(MASS)

modelo1.aic<- stepAIC(modelo1, direction="both")
summary(modelo1.aic)

step(modelo1, direction = "both")

#variando idade

seq_idades <- 20:99
seq_idades_pred <- seq_idades - mean(dados_modelo$idade)

#comorbidade não
pred_comorbidadeNão1 <- predict(modelo1, 
                                data.frame(idade = seq_idades_pred,
                                           comorbidade='Não',sintomatico='Sim'),type = 'response')

#comorbidade sim
pred_comorbidadeSim1 <- predict(modelo1, 
                                data.frame(idade = seq_idades_pred,
                                           comorbidade='Sim',sintomatico='Sim'),type = 'response')

#gráfico
ggplot() + 
  geom_line(aes(x = seq_idades, y = pred_comorbidadeNão1, color='red')) +
  geom_line(aes(x = seq_idades, y = pred_comorbidadeSim1, color='blue')) +
  xlab('Idade') +
  ylab('Estimativa da probabilidade de morte por Covid-19')+
  theme_minimal()+scale_color_discrete(name='Comorbidade', labels=c('Sim', 'Não'))

## CURVA ROC

pred <- predict(modelo1,newdata=dados_modelo,type=c("response"))

roccurve <- roc(dados_modelo$morte_covid_num ~ pred)

library(plotly)

ggplotly(
  ggroc(roccurve, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(roccurve$auc, 3), 
                       "|",
                       "Coeficiente de Gini", 
                       round((roccurve$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)
