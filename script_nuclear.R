library(RSelenium); library(tidyverse); library(rvest)

# ==================== ZONA DE SETEO DE VARIABLES INICIALES ====================

# seteo en link inician
url <- "https://pris.iaea.org/PRIS/CountryStatistics/CountryDetails.aspx?current=AR"

# cargo los drivers, y abro la sesión (que es básicamente abrir el navegador)
driver <- rsDriver(browser = c("firefox"))
remote_driver <- driver[["client"]]
# si al correr el "driver" ya está en uso, solo abrir 
# la sesión
remote_driver$open()

# ==================== ZONA DE DEFINICIÓN DE FUNCIONES =================

get_nuclear_data <- function(url, rmDr){
  
  #' @param url caracter con el link de página madre
  #' @param rmDr el driver del sitio web con que se navegará
  #' @return data frame con el consolidado de plantas nucleares por país
  
  # sesión inicial en Rvest
  ses_rvest <- html_session(url)
  
  # listado con todos los países
  paises <- ses_rvest %>% 
    html_nodes("div.content > ul > li:not(.caption)") %>% # el :not() encierra una clase
    html_text() %>% str_remove("[\r\n]") %>%              # específica que quiero excluir
    str_trim(side = "both")
  
  # acumulador de df
  lista_final <- list()
  
  # itero a lo largo de los países
  for (i in 1:length(paises)) {
    # voy llenando la lista
    lista_final[[i]] <- get_matriz_pais(ses_rvest, paises[i], rmDr)
    
    #ir cerrando las conexiones 
    closeAllConnections()
    ses_rvest <- html_session(url)
  }
  # consolido matriz y la devuelvo
  mat_final <- do.call("rbind", lista_final)
  return(mat_final)
} # fin get_nuclear_data


get_matriz_pais <- function(sesion, pais, rmDr){
  
  #' @param sesion recibe la sesión iniciada para navegar
  #' @param pais recibe el texto con el país del que se obtendrá la data
  #' @param rmDr el driver del sitio web con que se navegará
  #' @return un data frame con las plantas nucleares del páis indicado
  
  # Voy al país indicado
  data.pais <- sesion %>% follow_link(pais)
  
  # Cargo el link en RSelenium también
  rmDr$navigate(data.pais$url)
  
  # identifico la tabla del país
  tabla <- rmDr$findElement(using = "class",
                             value = "tablesorter")
  
  # Estas estructuras alentan el código, para que no te rechacen la conexión
  Sys.sleep(2)
  
  # le saco el html de adentro, y la parseo a df con comands de Rvest
  front.table <- tabla$getElementAttribute("outerHTML")[[1]] %>%
    read_html() %>%
    html_node("table") %>%
    html_table() %>%
    as.data.frame()
  
  # print("ad portas de agregar país a data frame")
  # le adjunto el país
  front.table$pais <- pais
  
  # ejecuto la función para obtener df con detalle de plantas nucleares
  # y la retorno
  print(paste("Obtener datos del país", pais))
  df.plantas <- obtener_plantas(front.table, rmDr, data.pais$url)
  return(df.plantas)
}# fin get_matriz_pais


obtener_plantas <- function(df, rmDr, url.pais){
  
  #' @param df el data frame con cada planta del país
  #' @param rmDr el driver del sitio web con que se navegará
  #' @return el detalle de las plantas nucleares de cada país
  
  # creo acumulador de df de plantas
  lista.plantas <- list()
  
  # comienzo iteración, el listado de plantas parte en 0, por ello es largo -1
  for (i in 0:(nrow(df)-1)) {
    # construyendo la ruta de la planta específica
    valor <- paste0("MainContent_MainContent_rptCountryReactors_hypReactorName_", i)
    
    # capturando la ruta de la planta
    planta <- rmDr$findElement(using = "id", 
                               value = valor)
    Sys.sleep(1)
    # haciendo click a la ruta
    planta$clickElement()
    
    # posicionando el driver en el detalle de las plantas
    rmDr$navigate(planta$getCurrentUrl())
  
    # recupero el nombre de la planta
    nom <- rmDr$findElement(using = "id",
                            value = "MainContent_MainContent_lblReactorName") 
    
    Sys.sleep(1)
    # asigno el nombre a una variable
    nom <- nom$getElementAttribute("outerHTML")[[1]] %>%
      read_html() %>%
      html_node("span#MainContent_MainContent_lblReactorName") %>%
      html_text()
    
    print(paste("Obtener datos de la planta", nom))
    
    # Aplico un control de excepciones en caso de que no existan datos de esa planta
    tryCatch({
      # capturando tabla con detalle de planta
      tabla2 <- rmDr$findElement(using = "xpath", # hoja tiene muchas tablas, así que uso este selector que es más específico
                                 value = "/html/body/form/div[3]/div[3]/div/div[1]/div/div[3]/div[2]/table")
      
      # genero data frame
      back.table <- tabla2$getElementAttribute("outerHTML")[[1]] %>%
        read_html() %>%
        html_node("table") %>%
        html_table(fill = T) %>%
        as.data.frame()
      
      # identifico el nombre de la planta
      back.table$plant.name <- nom
      
      # le pego los datos del DF entrante
      back.table2 <- cbind(back.table, df[df$Name==nom,]) %>% as.data.frame()
      
    }, error = function(e){
      # imprimo esto en caso de error
        print(paste("no se encontró tabla para planta", nom))
      }
    )# anido en trycatch en caso que una planta no tenga datos
    
    if(exists("back.table")){
      # evaluo que tabla haya sido creada, antes de mandarla al acumulador
      # el cual se auto asigna según su largo actual
      lista.plantas[[length(lista.plantas)+1]] <- back.table2
    } 
    
    # devolviendo el driver al sitio del país
    rmDr$navigate(url.pais)
    Sys.sleep(1)
  }
  # consolido matriz de detalle de plantas, y lo retorno
  matriz <- do.call("rbind", lista.plantas)
  return(matriz)
} # fin de obtener_planta()


# ==================  ZONA DE EJECUIÓN DE FUNCIONES ==================

# creando consolidado
consolidado <- get_nuclear_data(url, remote_driver)

# exportamos el consolidado (si tienen miedo de hechar a perder el df después
# de tanto rato, exporten antes de limpiar)
write.table(consolidado, "data_nuclear.csv", sep = ";", row.names = F)


# ====================== ZONA DE LIMPIEZA ================

library(stringr)

# Sin exagerar poco más de 70% del tiempo de la analítica, se usa solo
# EN LIMPIAR LOS DATOS!!! Es una maldición ineludible en la era del big data,
# a lo menos por ahora, así que acá van algunos trucos

# Primero, los detalles de cada planta tienen doble nombre de columna, así que 
# se tomaron como filas, así que los eliminamos tomando cualquier palabra 
# que no debiera estar en esa fila, ejemplo, "Year", donde solo debieran haber años
consolidado <- consolidado[-c(which(consolidado$Year=="Year")),]

# El nombre de las columnas trae caracteres HTML que pueden ser molestos, así que
# eliminémoslos
nom_columnas <- consolidado %>% 
  colnames() %>%
  str_remove_all("[\n]") %>% 
  str_trim(side = "both") %>%
  str_squish() %>% 
  str_replace_all("[ ]", ".")

# una vez limpios, los volvemos a poner en su lugar
colnames(consolidado) <- nom_columnas

# cambiamos el nombre a las columnas 6 al 9, por el nombre apropiado que señala el sitio
# del PRIS (les agrego puntos, porque se ven horribles al invocarlos sin éstos)
colnames(consolidado)[6:9] <- c("Energy.Availability.Factor.Anual.[%]",
                                "Energy.Availability.Factor.Cumulative.[%]",
                                "Load.Factor.Anual.[%]",
                                "Load.Factor.Cumulative.[%]")

# hasta aquí todo bien, pero veamos que pasa con los tipos de datos
class(consolidado$Year)
 
# debiera ser numérico! Así que parseamos la data
# sabemos que de la columna 1 a la 9 debieran ser numéricas, así que
# hagamos un loop que las transforme
for (i in 1:9) {
  consolidado[,i] <- as.numeric(paste0(consolidado[,i]))
}

# algunas tablas tenían textos, así que, quedaron como valores NA, o nulos
# Por último, parsemos las fechas a formato fecha
consolidado$First.Grid.Connection <- consolidado$First.Grid.Connection %>% 
  as.Date("%Y-%m-%d")

# Si quieren pueden volver a exportar, para registrar los cambios
# write.table(consolidado, "data_nuclear.csv", sep = ";", row.names = F)

# =========================== ZONA DE ANÁLISIS ========================

# CONTINUARÁ......



