library(RSelenium); library(tidyverse); library(XML); library(rvest)

# NO SE PUEDE USAR RVEST PORQUE NO DEJA ENTRAR AL DETALLE DE CADA PLANTA
# CON follow_link ni jump_to (páginas dinámicas)

# seteo en link inician
url <- "https://pris.iaea.org/PRIS/CountryStatistics/CountryDetails.aspx?current=AR"

# cargo los drivers, y abro la sesión (que es básicamente abrir el navegador)
driver <- rsDriver(browser = c("firefox"))
remote_driver <- driver[["client"]]
# si al correr el "driver" ya está en uso, solo abrir 
# la sesión

remote_driver$open()

# voy al link inicial
remote_driver$navigate(url)

# identifico la tabla que quiero
tabla1 <- remote_driver$findElement(using = "class",
                                   value = "tablesorter")

# le saco el html de adentro, y la parseo a df con comands de Rvest
front.table <- tabla1$getElementAttribute("outerHTML")[[1]] %>%
  read_html() %>%
  html_node("table") %>%
  html_table() %>%
  as.data.frame()

# construyendo la ruta de la planta específica
valor <- paste0("MainContent_MainContent_rptCountryReactors_hypReactorName_", 0)

# capturando la ruta de la planta
planta <- remote_driver$findElement(using = "id", 
                                    value = valor)
# haciendo click a la ruta
planta$clickElement()

# # probando algo
remote_driver$navigate(planta$getCurrentUrl())

# capturando tabla con detalle de planta
tabla2 <- remote_driver$findElement(using = "xpath",
                                    value = "/html/body/form/div[3]/div[3]/div/div[1]/div/div[3]/div[2]/table")

# capturando tabla de detalle
back.table <- tabla2$getElementAttribute("outerHTML")[[1]] %>%
  read_html() %>%
  html_node("table") %>%
  html_table(fill = T) %>%
  as.data.frame()



