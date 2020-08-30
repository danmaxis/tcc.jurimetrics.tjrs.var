## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(jurimetrics) # Biblioteca do Professor Zabala contendo os dados e o seletor do melhor algoritmo de predição

library(dplyr) #Fornece a gramática de manipulação dos dados
library(tidyr)
#library(broom)

library(conflicted)

library(foreach)
#library(stringi)
library(RQuantLib) # Usado para feriados
library(lubridate)
library(rlang)
library(rlist)
library(plotly)

#library(fpp2)
library(xts)
library(timetk)
library(mFilter)
library(tsbox)
library(imputeTS)

library(vars)
# pareto chart
library(qcc)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Tabela original
count_year_month_type


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Chamada de função para conversão de texto
unescape_unicode <- function(x){
  #single string only
  stopifnot(is.character(x) && length(x) == 1)

  #find matches
  m <- gregexpr("(\\\\)+x[0-9a-z]{2}", x, ignore.case = TRUE)

  if(m[[1]][1] > -1){
    #parse matches
    p <- vapply(regmatches(x, m)[[1]], function(txt){
      gsub("\\", "\\\\", parse(text=paste0('"', txt, '"'))[[1]], fixed = TRUE, useBytes = TRUE)
    }, character(1), USE.NAMES = FALSE)

    #substitute parsed into original
    regmatches(x, m) <- list(p)
  }

  x
}

# Conversão do texto utf-8 para latin1 por conta da codificação do ambiente windows (Verificar se no linux é o mesmo caso)

# Aplicação em mês-ano
cymt<-count_year_month_type
cymt$type<- cymt$type %>% sapply(FUN = unescape_unicode) %>% sapply(iconv, from="UTF-8",to="LATIN1")

#Aplicação por dia
cdt <- count_day_type
cdt$type<- cdt$type %>% sapply(FUN = unescape_unicode) %>% sapply(iconv, from="UTF-8",to="LATIN1")

#Aplicação por dia da semana
cwdt <-count_week_day_type
cwdt$type<- cwdt$type %>% sapply(FUN = unescape_unicode) %>% sapply(iconv, from="UTF-8",to="LATIN1")

cymt


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(cymt, max.level=4)


## ----rows.print=20--------------------------------------------------------------------------------------------------------------------------------------------------------------
cymt %>% group_by(type) %>% summarize(sum(count))



## ----rows.print=20--------------------------------------------------------------------------------------------------------------------------------------------------------------
# Inserindo os dias de semana
cdt$dow <- wday(cdt$judgmentDate)

# Inserindo a avaliação de dia útil
cdt$businessday <- cdt$judgmentDate %>% sapply(FUN = strftime, "%u") %>% sapply(FUN = as.numeric) %>% sapply( FUN = (function(x) if(x<6){TRUE}else(FALSE)) )
# Mesma operação utilizando o RQuantLib
#cdt$businessday <- cdt$judgmentDate %>% sapply(FUN = isBusinessDay, calendar="Brazil")

# Separando dia, mês e ano para avaliações
cdt$day<- day(cdt$judgmentDate)
cdt$month<- month(cdt$judgmentDate)
cdt$year<- year(cdt$judgmentDate)

# Verificando se a data é feriado
cdt$holiday <- cdt$judgmentDate %>% sapply(FUN = isHoliday, calendar="Brazil")
 
cdt %>% arrange(judgmentDate)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
top5 <- cymt %>% group_by(type) %>% summarize(sum_count=sum(count)) %>% arrange(desc(sum_count)) %>% top_n(5)
top5

## Aplicando o filtro top5 para as distribuições de dados

# Observações diárias
cdt_top5 <- cdt %>% left_join(top5,by = "type") %>% drop_na(sum_count)

# Observações mês e ano
cymt_top5 <- cymt %>% left_join(top5,by = "type") %>% drop_na(sum_count)

# Observações por dia da semana
cwdt_top5 <- cwdt %>% left_join(top5,by = "type") %>% drop_na(sum_count)
 


## ---- fig.width=10, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------
# Mudar tons ***
p <- ggplot(cymt_top5, aes(x=cymt_top5$yearMonth, y=cymt_top5$count, group=cymt_top5$type)) + geom_line(aes(color=cymt_top5$type), lwd=1) + labs(title = "Distribuição Top 5", y="Quantidade",x="Tempo")
p + scale_color_brewer(palette="Dark2") + theme(legend.position="bottom",legend.text = element_text(size=8), axis.title.x=element_blank()) + guides(color=guide_legend(title="Tipo")) 

# Volume total com quebras x
# Comparação de modelos (FITS, facebook,...)
# Pareto (explicar significancia)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#qcc::




## ---- fig.width=10, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------

# Avaliação da frequência mensal dos registros
cdt_freq_month <- cdt_top5 %>% select(type,month,count) %>% group_by(type, month)%>% summarize(sum_count=sum(count)) %>% arrange(desc(sum_count))

ggplot(cdt_freq_month, aes(x=month, y=sum_count, group=type)) + geom_point(size=3,shape=19,aes(color=type),lwd=2) + labs(title = "Distribuição Frequência Meses", y="Quantidade",x="Mês") + scale_x_continuous( breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) + scale_color_brewer(palette="Dark2") + theme(legend.position="bottom",legend.text = element_text(size=8), axis.title.x=element_blank()) + guides(color=guide_legend(title="Tipo")) 




## ---- fig.width=10, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------
cdt_top5_ts <- xts(cdt_top5$count,cdt_top5$judgmentDate, type_name=cdt_top5$type)


plot(to.monthly(cdt_top5_ts) )
plot(to.quarterly(cdt_top5_ts))
plot(to.yearly(cdt_top5_ts))





## ---- fig.width=10, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------


# Criando um tibble com a soma e média das contagens anuais
ysm_tb <- inner_join(tk_tbl(apply.yearly(cdt_top5_ts,FUN=sum)), tk_tbl(apply.yearly(cdt_top5_ts,FUN=mean)), by = "index",suffix = c(".sum", ".mean"))

ysm_tb$norm_sum <- ysm_tb$value.sum %>% scale()
ysm_tb$norm_mean <- ysm_tb$value.mean %>% scale()

ggplot(ysm_tb, aes(x=index))+geom_line(aes(y = norm_sum,colour = "soma"))+geom_line(aes(y = norm_mean,colour = "média")) + labs(title = "Variação da média e do crescimento ao longo dos anos ", y="Normalizado",x="Anos")



## ---- fig.width=6, fig.height=6-------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(cdt_top5, aes(x=judgmentDate, y=count, group=type))+ geom_line(aes(color=type))+ theme(legend.position="bottom",legend.text = element_text(size=5), axis.title.x=element_blank()) + guides(color=guide_legend(title="Tipo")) 

#Valor máximo diário
max(cdt_top5$count)

#Valor máximo para cada top5
cdt_top5 %>% group_by(type) %>% summarize(max(count))

#Quantidade de dias atingindo o teto de 1000 registros
cdt_top5 %>% filter(count>999) %>% group_by(type) %>% summarise(qtd=n())

#Dias da semana quando os registros acima de 1000 ocorreram
cdt_top5 %>% filter(count>999) %>% group_by(dow) %>% summarise(qtd=n())

#Distribuição dos dias excedentes de 1000 por tipo
cdt_top5 %>% filter(count>999) %>% group_by(type,dow) %>% summarise(qtd=n())



## ---- fig.width=6, fig.height=6-------------------------------------------------------------------------------------------------------------------------------------------------

cdt_freq_dow <- cdt_top5 %>% select(type,dow,count) %>% group_by(type, dow)%>% summarize(sum_count=sum(count)) %>% arrange(desc(sum_count))

ggplot(cdt_freq_dow, aes(x=dow, y=sum_count, group=type))+ geom_point(size=3,shape=19,aes(color=type),lwd=2) + labs(title = "Distribuição Frequência Dia da Semana", y="Quantidade",x="Dia da semana")

# Pareto com distribuiçao por dia da semana 2x5 (com o geral)



## ---- fig.width=15, fig.height=6------------------------------------------------------------------------------------------------------------------------------------------------

# Agregar o volume de registros
cymt_top5_agg <- aggregate(x = cymt_top5[c("count")],by=list(Group.yearMonth = cymt_top5$yearMonth),FUN="sum")
cdt_top5_agg <- aggregate(x = cdt_top5[c("count")],by=list(Group.date = cdt_top5$judgmentDate),FUN="sum")

ggplot(cymt_top5_agg,aes(x=Group.yearMonth))+geom_line(aes(y = count,color=count),lwd=1)



## ----fig.width=10, fig.height=8-------------------------------------------------------------------------------------------------------------------------------------------------

# cdt_top5_ts <- xts(order.by = cymt_top5_agg$Group.yearMonth,cymt_top5_agg$count)


cymt_top5_agg.hp <- hpfilter(xts(order.by = cymt_top5_agg$Group.yearMonth,cymt_top5_agg$count),freq=1600)

## Exibição geral
cymt_top5_agg.hp$xname <- "Top5 Combinado" 
plot(cymt_top5_agg.hp)

#hpfilter(df_count_year_month_type_grp['count'],lamb=1600)


# Exibindo as avaliações para cada tipo
cat("Exibição por tipo")
list_cymt_top5_hpfo<- foreach(tipo=top5$type) %do% {
  filtro_tipo <- cymt_top5 %>% filter(type==tipo)
  
  hpfo_tipo_cymt <-hpfilter(xts(order.by = filtro_tipo$yearMonth,filtro_tipo$count),freq=1600)
  hpfo_tipo_cymt$xname <- tipo
  plot(hpfo_tipo_cymt)
  }





## ----fig.width=10, fig.height=8-------------------------------------------------------------------------------------------------------------------------------------------------

#sd_cymt_top5_agg <- decompose(ts_ts(xts(order.by = cymt_top5_agg$Group.yearMonth,cymt_top5_agg$count)), type = c("additive", "multiplicative"), filter = NULL)
#Usando stl para decomposição, segundo recomendação do Hyndman
sd_cymt_top5_agg <- stl(ts_ts(xts(order.by = cymt_top5_agg$Group.yearMonth,cymt_top5_agg$count)), s.window = 12)

plot(sd_cymt_top5_agg,main = "Decomposição - Top5 Agregado")


cat("Exibição por tipo")
list_stl_ts_cymt_top5_tipo<- foreach(tipo=top5$type) %do% {
  filtro_tipo <- cymt_top5 %>% filter(type==tipo)
  
  #Tratamento de série para limpar NAs
  ts_filtro_tipo<- ts_ts( xts(order.by = filtro_tipo$yearMonth,filtro_tipo$count))
  # Imputando NAs através de pesos de média móvel
  ts_filtro_tipo <- ts_filtro_tipo %>% na_seadec(algorithm = "ma")
  
  ##sd_cymt_tipo <-decompose(ts_filtro_tipo, type = c("additive", "multiplicative"), filter = NULL)
  sd_cymt_tipo <-stl(ts_filtro_tipo, s.window = 12)
  plot(sd_cymt_tipo,main = paste0("Decomposição - ",tipo))
  
  sd_cymt_tipo <- list.append(sd_cymt_tipo,nome=tipo)
  # Chamada para armazenar as séries decompostas  no vetor resulta e economizar chamadas futuras
  sd_cymt_tipo
  }



## ----fig.width=10, fig.height=8-------------------------------------------------------------------------------------------------------------------------------------------------
# result_ts_stl_cymt_top5_tipo contem as séries por tipo com a decomposição

# Teste com duas séries quaisquer, pelo componente trend
# pearson <- cor.test(list_stl_ts_cymt_top5_tipo[[3]]$time.series[,1], list_stl_ts_cymt_top5_tipo[[5]]$time.series[,1], method="pearson" )
# plot(pearson)

# ggscatter([result_ts_stl_cymt_top5_tipo[[3]]$time.series[,1], result_ts_stl_cymt_top5_tipo[[5]]$time.series[,1]], 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "tipo 3", ylab = "tipo 5")

# list_pearson <- list()
# 
# list_tlcc <- list()
# 
# position <- 1
# for (tipo_top5_ts in list_stl_ts_cymt_top5_tipo) {
#   # Cria uma cópia da lista de séries removendo a série em questão para comparar com as demais
#   resto_list <- duplicate(list_stl_ts_cymt_top5_tipo, shallow = FALSE)
#   resto_list[position]<-NULL
# 
#   for(compara in resto_list){
#     # print(length(tipo_top5_ts$time.series[,1]))
#     # print(length(compara$time.series[,1]))
#     #list_pearson<-c(list_pearson, cor.test(tipo_top5_ts$time.series[,1], compara$time.series[,1], method="pearson" ))
#     corobject<-ccf(tipo_top5_ts$time.series[,2],compara$time.series[,2],type="correlation",plot=FALSE)
#     corobject$series<-"Trend Mês-Ano"
#     corobject$snames<-paste(tipo_top5_ts$nome,"x",compara$nome)
#     list_pearson<- list.append(list_pearson,corobject) 
#     ggplotly(ggplot(x = corobject$acf, y = corobject$lag ))
#     # fig <- plot_ly(x = corobject$acf, y = corobject$lag )
#     # fig
#   }
#   position <- position+1
# }




## ----fig.width=10, fig.height=8-------------------------------------------------------------------------------------------------------------------------------------------------

list_tlcc <- list()

position <- 1
for (tipo_top5_ts in list_stl_ts_cymt_top5_tipo) {
  # Cria uma cópia da lista de séries removendo a série em questão para comparar com as demais
  resto_list <- duplicate(list_stl_ts_cymt_top5_tipo, shallow = FALSE)
  resto_list[position]<-NULL

  for(compara in resto_list){
    # print(length(tipo_top5_ts$time.series[,1]))
    # print(length(compara$time.series[,1]))
    #list_pearson<-c(list_pearson, cor.test(tipo_top5_ts$time.series[,1], compara$time.series[,1], method="pearson" ))
    corobject<-ccf(tipo_top5_ts$time.series[,2],compara$time.series[,2],type="correlation",plot=FALSE)
    corobject$series<-"Trend Mês-Ano"
    corobject$snames<-paste(tipo_top5_ts$nome,"x",compara$nome)
    list_tlcc<- list.append(list_tlcc,corobject) 
    
    par(mar=c(3,3,3,3)) #Correção de margens para não ocultar partes do gráfico
    plot(corobject)
    # ggplotly(ggplot(x = corobject$acf, y = corobject$lag ))
    # fig <- plot_ly(x = corobject$acf, y = corobject$lag )
    # fig
  }
  position <- position+1
}



# ccf(result_ts_stl_cymt_top5_tipo[[3]]$time.series[,1],result_ts_stl_cymt_top5_tipo[[5]]$time.series[,1],type="correlation")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
list_cymt_top5 <- foreach(tipo=top5$type) %do% {
  filtro_tipo <- cymt_top5 %>% filter(type==tipo)
  
  #Tratamento de série para limpar NAs
  ts_filtro_tipo<- ts_ts( xts(order.by = filtro_tipo$yearMonth,filtro_tipo$count))
}


ts_cymt_top5_as_vars<- list_cymt_top5[[1]] %>% merge.zoo(list_cymt_top5[[2]])
for (i in 3:5) {
  ts_cymt_top5_as_vars<- ts_cymt_top5_as_vars %>% merge.zoo(list_cymt_top5[[i]]) 
}

# Imputando NAs através de pesos de interpolação
ts_cymt_top5_as_vars<- ts_cymt_top5_as_vars %>% na_seadec(algorithm = "interpolation")
colnames(ts_cymt_top5_as_vars) = top5$type
plot(ts_cymt_top5_as_vars)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
list_cdt_top5 <- foreach(tipo=top5$type) %do% {
  filtro_tipo <- cdt_top5 %>% filter(type==tipo)
  
  #Tratamento de série para limpar NAs
  ts_filtro_tipo<- ts_ts( xts(order.by = filtro_tipo$judgmentDate,filtro_tipo$count))

}


ts_cdt_top5_as_vars<- list_cdt_top5[[1]] %>% merge.zoo(list_cdt_top5[[2]],all= TRUE,fill=0)
for (i in 3:5) {
  ts_cdt_top5_as_vars<- ts_cdt_top5_as_vars %>% merge.zoo(list_cdt_top5[[i]],all= TRUE,fill=0) 
}

# Imputando NAs através de pesos de interpolação
ts_cdt_top5_as_vars<- ts_cdt_top5_as_vars %>% na_seadec(algorithm = "interpolation")
colnames(ts_cdt_top5_as_vars) = top5$type
plot(ts_cdt_top5_as_vars)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# VAR()

var.1 <- VAR(ts_cymt_top5_as_vars,type = "none") 
var.1

VARselect(ts_cymt_top5_as_vars, lag.max = 8, type = "both", season=12)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
var.aic <- VAR(ts_cymt_top5_as_vars, type = "none", lag.max = 3, ic = "AIC")
summary(var.aic)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ir.1 <- irf(var.1, impulse = "Agravo", response = "Recurso.Cível", n.ahead = 20, ortho = FALSE)
plot(ir.1)

ir.1 <- irf(var.1, impulse = "Apelação.Cível", response = "Recurso.Cível", n.ahead = 20, ortho = FALSE)
plot(ir.1)

ir.2 <- irf(var.1,impulse="Apelação.Cível",response="Recurso.Cível",n.ahead = 20,ortho = FALSE,cumulative = TRUE)
plot(ir.2)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# pairs para top5
#pairs(xx)
plot(x,y)
pairs(data)
pairs(top5)



# VAR - Vector Auto Regressive
# Predição

# Paretos

# Hyndman




## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

citation("jurimetrics")

citation("dplyr")
citation("tidyr")
#citation("broom")

citation("conflicted")

citation("foreach")
#citation("stringi")
#citation("RQuantLib")
citation("lubridate")
citation("rlang")
citation("rlist")
citation("plotly")

#citation("fpp2")
citation("xts")
citation("timetk")
citation("mFilter")
citation("tsbox")
citation("imputeTS")

citation("vars")
# pareto chart
citation("qcc")


