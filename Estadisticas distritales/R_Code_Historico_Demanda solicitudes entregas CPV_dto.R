###### META SPEN 2021_HISTORICO DEMANDA SOLICITUDES-ENTREGA CREDENCIALES 2017-2021 ######
### INE-DERFE-COC-DE-EVALUACION DEMOGRAFICA ###
### Autor: Miguel David Alvarez Hernández
### Ultima versión : 28/04/2021


#----------------------------------------------------------------------------------#
################################# Paquetes y setup #################################
#----------------------------------------------------------------------------------#

library(pacman)
p_load(tidyverse,
       lubridate,
       plotly,
       ggthemes,
       RColorBrewer)

#Prevenir notación científica
options(scipen=999) 
#Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))
print(getwd())



#----------------------------------------------------------------------------------#
############################# Datos originales #####################################
#----------------------------------------------------------------------------------#

Datos <- read_csv("Datos_Tram_Dttos_20abr2021.csv", 
                                 col_types = cols(remesa = col_character()))
View(Datos)


Datos_clv_edo <- read_csv("Datos_clv-edo.csv")
#View(Datos_clv_edo)

#----------------------------------------------------------------------------------#
############################# Transformacion de datos ##############################
#----------------------------------------------------------------------------------#

#se crean columnas separadas para año y numero de semana
Datos$Año <- substr(Datos$remesa, 1, 4)
Datos$Semana <- substr(Datos$remesa, 5, 6)

#se crea columna Fecha para facilitar graficación
Datos$Fecha <- as.Date(paste(Datos$remesa,1),"%Y%W%u") 

#para las semanas extra se les asigna días comprendidos entre la última semana oficial y la primera semana del año siguiente
Datos$Fecha2 <- ifelse(Datos$remesa == "201753", "2017-12-31", as.character(Datos$Fecha))  
Datos$Fecha2 <- ifelse(Datos$remesa == "201854", "2019-01-01", as.character(Datos$Fecha2))  
Datos$Fecha2 <- ifelse(Datos$remesa == "201855", "2019-01-01", as.character(Datos$Fecha2))  
Datos$Fecha2 <- ifelse(Datos$remesa == "201856", "2019-01-01", as.character(Datos$Fecha2))  
Datos$Fecha2 <- ifelse(Datos$remesa == "201857", "2019-01-01", as.character(Datos$Fecha2))  
Datos$Fecha2 <- ifelse(Datos$remesa == "201858", "2019-01-01", as.character(Datos$Fecha2))  
Datos$Fecha2 <- ifelse(Datos$remesa == "201859", "2019-01-01", as.character(Datos$Fecha2))  
Datos$Fecha2 <- ifelse(Datos$remesa == "201953", "2019-12-31", as.character(Datos$Fecha2))  
Datos$Fecha2 <- ifelse(Datos$remesa == "201954", "2019-12-31", as.character(Datos$Fecha2))  
Datos$Fecha2 <- ifelse(Datos$remesa == "202053", "2020-12-29", as.character(Datos$Fecha2))  
Datos$Fecha2 <- ifelse(Datos$remesa == "202054", "2020-12-30", as.character(Datos$Fecha2))  

#para las semanas extra se les asigna semana 53 o 54
Datos$Semana <- ifelse(Datos$remesa == "201753", "53", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "201854", "54", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "201855", "54", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "201856", "54", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "201857", "54", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "201858", "54", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "201859", "54", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "201953", "53", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "201954", "53", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "202053", "53", Datos$Semana)  
Datos$Semana <- ifelse(Datos$remesa == "202054", "53", Datos$Semana)  


#Se añade la columna con el nombre de los estados
Datos2 <- left_join(Datos,Datos_clv_edo,by="CLV-EDO")

#se eliminan columnas innecesarias y se agrupan los registros con fechas repetidas 
Datos2 <- Datos2[-c(12:15,18)]%>% 
  group_by(Edo,`CLV-EDO`,`CLV-DTO`,Año,Semana,Fecha2) %>% 
  summarise(across(everything(), sum))

#renombramos columnas
Datos2 <- Datos2 %>% rename(Fecha = Fecha2, INSCRIPCION= `INS-SEM`,CORECCION_DATOS=`ERR-SEM` , CAMBIO_DOMICILIO=`CD-SEM` ,
                             REPOSICION=`REP-SEM`, CORRECCION_DOMICILIO=`ER-D-SEM`, REINCORPORACION=`REIN-SEM`, REEMPLAZO=`REEM-SEM`,
                             TOTAL_SOLICITUDES=`TOT-F-S`, ENTREGA_CPV=`CRED-SEM`)

#transformar de formato ancho a formato largo
Datos3 <- gather(Datos2, key = "Tramite", value = "Numero",
                 c(colnames(Datos2[,7:15])))

#se desagrupa el tibble para graficar bien
Datos3 <- Datos3 %>% ungroup()

#verificamos que el numero de valores vacíos en la columna Numero
sum(is.na(Datos3$Numero))

#crear id column para cada uno de los 300 dto
Datos3$ID  <- paste(Datos3$Edo, 'DTO',as.character(Datos3$`CLV-DTO`), sep = '-')


#NIVELES factor para entidades
estados <- unique(Datos3$Edo)[1:32]
niveles_edo <- c(estados)

#convertir a factor la columna estados
Datos3$Edo <- factor(Datos3$Edo, levels = niveles_edo)
#levels(Datos$Edo)

#convertir a factor la columna tramite
Datos3$Tramite <- as.factor(Datos3$Tramite)

#NIVELES factor para dto
dto <- unique(Datos3$ID)
niveles_dto <- c(dto)
levels(Datos3$ID) #numero de dto-niveles

#convertir a factor la columna ID
Datos3$ID <- as.factor(Datos3$ID)

#convertir a date la columna fecha
Datos3$Fecha <- as_date(Datos3$Fecha, format =  "%Y-%m-%d") 

#añadimos una columna que identifica el tipo de campaña (VER FECHAS)
INICIO_CAI_2017_2018 = as.Date("2017-09-01")
CIERRE_CAI_2017_2018 = as.Date("2018-01-31")
INICIO_CAP_2018 = as.Date("2018-07-02")
CIERRE_CAP_2018 = as.Date("2018-08-31")
INICIO_CAI_2018 = as.Date("2018-09-01")
CIERRE_CAI_2018 = as.Date("2018-12-15")
INICIO_CAP_2018_2019 = as.Date("2018-12-16")
CIERRE_CAP_2018_2019 = as.Date("2019-08-31")
INICIO_CAI_2019 = as.Date("2019-09-01")
CIERRE_CAI_2019 = as.Date("2019-12-15")
INICIO_CAP_2019_2020 = as.Date("2019-12-16")
CIERRE_CAP_2019_2020 = as.Date("2020-08-31")
INICIO_CAI_2020_2021 = as.Date("2020-09-01")
CIERRE_CAI_2020_2021 = as.Date("2021-02-10")
INICIO_CAP_2021 = as.Date("2021-06-07")


Datos3$Campaña <- ifelse(Datos3$Fecha >= INICIO_CAI_2017_2018 & Datos3$Fecha <= CIERRE_CAI_2017_2018, 'CAI_2017_2018',
                         ifelse(Datos3$Fecha >= INICIO_CAP_2018 & Datos3$Fecha <= CIERRE_CAP_2018, 'CAP_2018',
                                ifelse(Datos3$Fecha >= INICIO_CAI_2018 & Datos3$Fecha <= CIERRE_CAI_2018, 'CAI_2018',
                                       ifelse(Datos3$Fecha >= INICIO_CAP_2018_2019 & Datos3$Fecha <= CIERRE_CAP_2018_2019, 'CAP_2018_2019',
                                              ifelse(Datos3$Fecha >= INICIO_CAI_2019 & Datos3$Fecha <= CIERRE_CAI_2019, 'CAI_2019',
                                                     ifelse(Datos3$Fecha >= INICIO_CAP_2019_2020 & Datos3$Fecha <= CIERRE_CAP_2019_2020, 'CAP_2019_2020',
                                                            ifelse(Datos3$Fecha >= INICIO_CAI_2020_2021 & Datos3$Fecha <= CIERRE_CAI_2020_2021, 'CAI_2020_2021',
                                                                   'Proceso Electoral Federal')))))))

#convertir a factor la columna Campaña
Datos3$Campaña <- as.factor(Datos3$Campaña)
levels(Datos3$Campaña)
sapply(Datos3, class) #tipo de columnas
View(Datos3)

#guardamos solo total de solicitudes y entregas de CPV
Datos4 <- Datos3 %>% filter(Tramite %in% c('TOTAL_SOLICITUDES','ENTREGA_CPV')) %>% 
  droplevels() 
#write.csv(Datos4,"Datos_Solicitudes_Entregas CPV_proc.csv", row.names = FALSE)



#----------------------------------------------------------------------------------#
############################### Análisis estadístico #################################
#----------------------------------------------------------------------------------#

#estadísticas de tendencia central por dto, tipo de tramite y campaña
Resumen <- Datos3 %>% group_by(ID, Tramite, Campaña) %>% 
  summarise(Total_tramites_por_campaña = sum(Numero), 
            Numero_semanas_por_campaña = n(),
            Promedio_tramites_por_semana = round(mean(Numero),digits=2),
            Mediana = round(median(Numero),digits=2),
            Desviacion_Estandar = round(sd(Numero),digits=2),
            Rango_Intercuantil = IQR(Numero),
            Q1 = quantile(Numero, 0.25),
            Q3 = quantile(Numero, 0.75)) %>% ungroup()

#se filtra para solo mostrar el total de solicitudes y las entregas de CPV
Resumen2 <- Resumen %>% filter(Tramite %in% c('TOTAL_SOLICITUDES','ENTREGA_CPV'))%>% 
  droplevels() 

View(Resumen2)
#write.csv(Resumen2,"Resultados_Solicitudes_Entregas CPV_estadisticas.csv", row.names = FALSE)


#se convierte a formato largo considerando solo tipo de tramite y promedio de tramites por semana
Resumen3 <- Resumen2[,c(1:3,6)] %>% spread(Tramite, Promedio_tramites_por_semana) %>% 
  rename(PROMEDIO_SEMANAL_ENTREGA_CPV = ENTREGA_CPV,
         PROMEDIO_SEMANAL_TOTAL_SOLICITUDES = TOTAL_SOLICITUDES)

View(Resumen3)
#write.csv(Resumen3,"Resultados_Solicitudes_Entregas CPV_promedios-campana.csv", row.names = FALSE)


#----------------------------------------------------------------------------------#
########################### Estimacion para CAP 2021 ###############################
#----------------------------------------------------------------------------------#

#utilizando como punto de referencia el comportamiento de los distritos en la CAP 2018
#se establecen 3 escenarios para la CAP 2021, utilizando:
#la mediana (escenario medio), Q1 (escenario bajo), y Q3 (escenario alto)

Estimacion <- Resumen2[,c(1:3,5,7,10,11)] %>% 
  filter(Campaña == 'CAP_2018') %>% 
  mutate(Campaña = as.factor('CAP_2021'),
         Numero_semanas_por_campaña = 13,
         EMedia_Tramites_por_campaña = Mediana*Numero_semanas_por_campaña,
         EBaja_Tramites_por_campaña = Q1*Numero_semanas_por_campaña,
         EAlta_Tramites_por_campaña = Q3*Numero_semanas_por_campaña) %>% 
  rename(EMedia_Tramites_por_semana = Mediana,
         EBaja_Tramites_por_semana = Q1,
         EAlta_Tramites_por_semana = Q3)

View(Estimacion)
#write.csv(Estimacion,"Resultados_Solicitudes_Entregas CPV_estimacion_CAP2021.csv", row.names = FALSE)


#----------------------------------------------------------------------------------#
############################### Graficas de plotly #################################
#----------------------------------------------------------------------------------#

#opciones estéticas
f <- list(
  size = 14,
  color = "#7f7f7f"
)

xx <- list(
  title = "Fecha",
  titlefont = f
)

xx2 <- list(
  title = "Promedio semanal de solicitudes",
  titlefont = f
)

xx3 <- list(
  title = "Número de trámites por semana",
  titlefont = f
)

yy <- list(
  title = "Número de trámites",
  titlefont = f
)

yy2 <- list(
  title = "Promedio semanal de entregas de CPV",
  titlefont = f
) 

yy3 <- list(
  title = "Tipo de trámite",
  titlefont = f
) 

# funcion para generar la lista de opciones del menu 
#(ver: https://stackoverflow.com/questions/55075168/loop-plotly-menu)

get_menu_list <- function(names){
  n_names = length(names)
  buttons = vector("list",n_names)
  
  for(i in seq_along(buttons)){
    buttons[i] = list(list(method = "restyle",
                           args = list("transforms[0].value", names[i]),
                           label = names[i]))
  }
  
  return_list = list(
    list(
      type = 'dropdown',
      active = 0,
      buttons = buttons
    )
  )
  
  return(return_list)
}


#----------------------------------------------------------------------------------#
##### line chart (menu por dto, total solicitudes y entrega credenciales)  #####
#----------------------------------------------------------------------------------#

#se crea lista de renglones con fecha de suspencion de labores en los modulos por pandemia
m <- as.Date("2020-03-23")
m2 <- as.Date("2020-08-17")
vline <- function(x = 0, color = "gray") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color, width=2, dash="dashdot")
  )
}

#en caso de que sea muy grande el archivo html, hay que filtrar
#filter(Tramite %in% c('TOTAL_SOLICITUDES','ENTREGA_CPV')) %>% 

#figura en plotly
fig0 <- Datos4 %>%
  plot_ly(
    x = ~Fecha, 
    y = ~Numero,
    customdata = ~ID,
    color = ~factor(Tramite),
    text = ~paste(ID,
                  '<br>Campaña:', Campaña,
                  '<br>Fecha de corte:', Fecha,
                  '<br>Semana operativa:', Semana,
                  '<br>Número de trámites en la semana:', format(Numero,big.mark=",", trim=TRUE)
    ),
    hoverinfo = 'text',
    transforms = list(
      list(
        type = 'filter',
        target = "customdata",
        operation = '=',
        value = unique(Datos4$ID)[1]
      )
    )) %>% 
  add_trace(type = 'scatter',
            mode='lines+markers'
  )%>%
    layout(xaxis = xx, 
         yaxis = yy,
         legend = list(
           font = list(
             size = 12,
             color = "#000"),
           orientation = 'h',
           x = 0.4, 
           y = -0.2),
         updatemenus = list(
           list(direction = "down",
                xanchor = 'right',
                yanchor = "top",
                x = 0.1,
                y= 1.1,
                font = list(
                  size = 10),
                type = 'dropdown',
                active = 0,
                buttons = apply(as.data.frame(as.factor(unique(Datos4$ID))), 1, 
                                function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
         )
  )%>%
  toWebGL() %>% add_annotations(x = m,
                                y = 1,
                                text = "Cierre de módulos por emergencia sanitaria COVID-19",
                                xref = "x",
                                yref = "paper",
                                showarrow = TRUE,
                                arrowhead = 5,
                                ax = -20,
                                ay = -20,
                                font = list(color = 'gray', size = 8)) %>% 
  add_annotations(x = m2,
                  y = 1,
                  text = "Apertura de módulos con medidas sanitarias",
                  xref = "x",
                  yref = "paper",
                  showarrow = TRUE,
                  arrowhead = 5,
                  ax = 20,
                  ay = -20,
                  font = list(color = 'gray', size = 8)) %>% 
  layout(shapes = list(vline(m), vline(m2)))
  

#fig0
#se salva la grafica como html
#htmlwidgets::saveWidget(as_widget(fig0), "Rplot_Dem-Sol-EntCPV_line-chart_menu-edo_dto.html")


#----------------------------------------------------------------------------------#
##### scatter plot (menu por Campaña, total solicitudes y entrega credenciales)  #####
#----------------------------------------------------------------------------------#

#figura en plotly
fig1 <- Resumen3 %>% 
  plot_ly(
    x = ~PROMEDIO_SEMANAL_TOTAL_SOLICITUDES, 
    y = ~PROMEDIO_SEMANAL_ENTREGA_CPV,
    color = ~Campaña,
    customdata = ~ID,
    text = ~paste(ID,
                  '<br>Campaña:', Campaña,
                  '<br>Promedio semanal de solicitudes:', format(PROMEDIO_SEMANAL_TOTAL_SOLICITUDES,big.mark=",",trim=TRUE),
                  '<br>Promedio semanal de entregas de CPV:', format(PROMEDIO_SEMANAL_ENTREGA_CPV,big.mark=",", trim=TRUE)),
    hoverinfo = 'text') %>% 
  add_trace(type = 'scatter',
            mode='markers'
  )%>%
  layout(xaxis = xx2, 
         yaxis = yy2,
         legend = list(
           font = list(
             size = 12,
             color = "#000"),
           orientation = 'h',
           x = 0.2, 
           y = -0.1))%>%
  toWebGL() 


#fig1
#se salva la grafica como html
#htmlwidgets::saveWidget(as_widget(fig1), "Rplot_Dem-Sol-EntCPV_scatter-plot.html")


#----------------------------------------------------------------------------------#
##### box-plot (menu por Campaña, total solicitudes y entrega credenciales)  #####
#----------------------------------------------------------------------------------#

fig2 <- Datos4 %>%
  plot_ly(x = ~Tramite, 
          y = ~Numero,
          customdata = ~ID,
          color = ~Campaña, 
          type = "box",
          boxpoints = "suspectedoutliers",
          transforms = list(
            list(
              type = 'filter',
              target = "customdata",
              operation = '=',
              value = unique(Datos4$ID)[1]
            )
          )) %>% layout(xaxis = yy3, 
                        yaxis = xx3,
                        boxmode = "group",
                        legend = list(
                          font = list(
                            size = 12,
                            color = "#000")),
                        updatemenus = list(
                          list(direction = "down",
                               xanchor = 'right',
                               yanchor = "top",
                               x = 0.1,
                               y= 1.1,
                               font = list(
                                 size = 10),
                               type = 'dropdown',
                               active = 0,
                               buttons = apply(as.data.frame(as.factor(unique(Datos4$ID))), 1, 
                                               function(x) list(method = 'restyle',args = list('transforms[0].value',x),label = x)))
                        ))

#fig2
#se salva la grafica como html
#htmlwidgets::saveWidget(as_widget(fig2), "Rplot_Dem-Sol-EntCPV_box-plot.html")
