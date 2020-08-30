#Imports
library(dplyr)
library(stringi)
library(NLP)
library(utf8)

library(SparkR)
library(zoo)

library(jurimetrics)


# installig and calling package
#devtools::install_github('filipezabala/jurimetrics', force=T)


# getting help
?fits
livestock
# example
fits(livestock)

# processual volume in TJ-RS (Brasil)
data("tjrs_year_month")

# forecasting
y <- ts(tjrs_year_month$count, start = c(2000,1), frequency = 12)
fits(y)


data(count_year_month_type)
count_year_month_type$type


y <- ts(count_year_month_type$count, start = c(2000,1), frequency = 12)
fits(y)
class(count_year_month_type)
colnames(count_year_month_type)
cymt<-count_year_month_type
#cymt %>% filter(cymt$type == "Agravo de Instrumento")
names(cymt)[names(cymt) == "yearMonth"] <- "ds"
names(cymt)[names(cymt) == "count"] <- "y"
cymt

###
# Duplicando para não mexer no original


cymt <- cymt %>% ungroup()
str(cymt)

cymt["type"]



### Teste do prophet com a base count_year_month_type
library(prophet)

m <- prophet(cymt %>% ungroup())
future <- make_future_dataframe(m, freq="year",periods = 3)
tail(future)
forecast <- predict(m, future)
#plot(m, forecast)

write.table(m$history , file = "m_history.csv")
write.table(forecast , file = "forecast.csv")
df <- prophet:::df_for_plotting(m, forecast)

df <- m$history %>% 
  dplyr::select(ds, y) %>% 
  dplyr::full_join(forecast, by = "ds") %>% 
  dplyr::arrange(ds) 

gg <- ggplot2::ggplot(df, ggplot2::aes(x = ds, y = y)) +
  ggplot2::labs(x = 'ds', y = 'y')

gg

gg <- gg +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = yhat_lower, ymax = yhat_upper),
    alpha = 0.2,
    fill = "#0072B2",
    na.rm = TRUE
  )

gg

gg <- gg +
  ggplot2::geom_point(na.rm=TRUE) +
  ggplot2::geom_line(
    ggplot2::aes(y = yhat), color = "#0072B2",
    na.rm = TRUE) +
  ggplot2::theme(aspect.ratio = 3 / 5)

gg


####
# Agrupar os registros por tipo

sumgroup <- cymt %>% group_by(type) %>% tally()

# Imprimir o top 5 dos registros por tipo
sumgroup %>% arrange(desc(n))
