library(RSelenium); library(tidyverse); library(XML); library(rvest)

# NO SE PUEDE USAR RVEST PORQUE NO DEJA ENTRAR AL DETALLE DE CADA PLANTA
# CON follow_link ni jump_to (páginas dinámicas)

# seteo en link inician
url <- "https://pris.iaea.org/PRIS/CountryStatistics/CountryDetails.aspx?current=AR"

# cargo los drivers, y abro la sesión (que es básicamente abrir el navegador)
driver <- rsDriver(browser = c("firefox"))
remote_driver <- driver[["client"]]
remote_driver$open()

# voy al link inicial
remote_driver$navigate(url)

# identifico la tabla que quiero
tabla <- remote_driver$findElement(using = "class",
                                   value = "tablesorter")

# le saco el html de adentro, y la parseo a df con comands de Rvest
front.table <- tabla$getElementAttribute("outerHTML")[[1]] %>%
  read_html() %>%
  html_node("table") %>%
  html_table() %>%
  as.data.frame()




planta <- remote_driver$findElement(using = "id", 
                                    value = "MainContent_MainContent_rptCountryReactors_hypReactorName_0")

planta$clickElement()
planta$goBack()
