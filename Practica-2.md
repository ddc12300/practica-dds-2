---
title: "Actividad Evaluable 2: Data Driven Security – CyberSecurity Management"
author: "Daniel Díaz y Raquel Abad"
date: "2024-01-23"
output:
  html_document:
    keep_md: true
  pdf_document: default
editor_options: 
  markdown: 
    wrap: sentence
---

# Introducción

En este informe, abordaremos la práctica de Data Driven Security dentro del módulo de CyberSecurity Management, específicamente en la asignatura de Data Driven Security.
El objetivo principal de esta práctica es aplicar técnicas de web scraping, análisis de datos y visualización, así como el manejo de logs de servidor y clustering de datos utilizando el lenguaje de programación R.

Para ello, desarrollaremos un programa capaz de extraer información de una página web mediante técnicas de web scraping, analizar los datos obtenidos, y posteriormente, gestionar y analizar logs de un servidor Apache.
Además, aplicaremos técnicas de clustering para identificar patrones en los datos del servidor.

A continuación, presentamos el código desarrollado para cada una de las tareas, junto con una explicación detallada de los pasos seguidos y las decisiones tomadas.

# 1. Datos Elegantes + Análisis de Datos con Web Scrapping

Queremos programar un programa de tipo web scrapping con el que podamos obtener una página web, mediante su URL, y poder analizar su contenido HTML con tal de extraer datos e información específica.
Nuestro programa ha de ser capaz de cumplir con los siguientes pasos:

#### 1. Descargar la página web de la URL indicada, y almacenarlo en un formato de R apto para ser tratado.

Aquí, utilizamos las librerías `httr` y `XML` para descargar el contenido de la página web de MediaWiki y convertirlo en un formato XML que podamos analizar en R.
Verificamos que la descarga sea exitosa mediante el código de estado HTTP.


```r
# Cargar las librerías necesarias
library(httr)
library(XML)

# Definir la URL de la página a descargar
url <- "https://www.mediawiki.org/wiki/MediaWiki"

# Utilizar la función GET() para descargar el contenido de la página
pagina_web <- GET(url)

# Verificar que la descarga fue exitosa
if (status_code(pagina_web) == 200) {
  # Convertir el contenido de la página a un formato que pueda ser tratado en R
  contenido_html <- content(pagina_web, as = "text")
  # Parsear el contenido HTML a XML para facilitar su análisis
  contenido_xml <- htmlParse(contenido_html, asText = TRUE)
  # Guardar el contenido XML en un archivo .xml
  saveXML(contenido_xml, file = "pagina_web.xml")
} else {
  stop("La página web no pudo ser descargada correctamente.")
}
```

```
## [1] "pagina_web.xml"
```

#### 2. Analizar el contenido de la web, buscando el título de la página (que en HTML se etiqueta como “title”).

Mediante el uso de XPath, extraemos el título de la página web y lo imprimimos para confirmar que se ha obtenido correctamente.


```r
# Cargar librerias
library(XML)

# Utilizar XPath para encontrar el título de la página
titulo <- xpathSApply(contenido_xml, "//title", xmlValue)

# Imprimir el título para verificar que se ha extraído correctamente
print(titulo)
```

```
## [1] "MediaWiki"
```

#### 3. Analizar el contenido de la web, buscando todos los enlaces (que en HTML se etiquetan como “a”), buscando el texto del enlace, así como la URL.

Utilizamos nuevamente XPath para extraer todos los enlaces de la página, incluyendo el texto y la URL de cada uno.
Luego, limpiamos y reemplazamos los valores nulos o cadenas vacías con `NA` y creamos un data frame con esta información.


```r
# Cargar librerias
library(XML)

# Leer el contenido XML desde el archivo si no está ya cargado en la sesión
# contenido_xml <- readRDS(file = "pagina_web.rds")

# Utilizar xpathSApply para extraer todos los atributos 'href' de los enlaces
links_href <- xpathSApply(contenido_xml, "//a", xmlGetAttr, "href")

# Utilizar xpathSApply para extraer todo el texto de los enlaces
links_text <- xpathSApply(contenido_xml, "//a", xmlValue)

# Función para reemplazar NULL o cadenas vacías con "NA"
replace_with_na <- function(x) {
  sapply(x, function(item) {
    if (is.null(item) || trimws(item) == "") {
      return(NA)
    } else {
      return(item)
    }
  })
}

# Reemplazar valores NULL o cadenas vacías en href y texto
links_href <- replace_with_na(links_href)
links_text <- replace_with_na(links_text)

# Crear un data frame con los textos y URLs de los enlaces
df_enlaces <- data.frame(Texto = links_text, URL = links_href, stringsAsFactors = FALSE)

# Imprimir los primeros enlaces para verificar
View(df_enlaces)
```

#### 4. Generar una tabla con cada enlace encontrado, indicando el texto que acompaña el enlace, y el número de veces que aparece un enlace con ese mismo objetivo.

Agrupamos los enlaces por texto y URL, contamos las ocurrencias y ordenamos el data frame por frecuencia para facilitar la visualización.


```r
# Limpiar los espacios en blanco al principio y al final del texto de los enlaces
df_enlaces$Texto <- sapply(df_enlaces$Texto, trimws)

# Rellenamos valores na con NA texto
df_enlaces$Texto[is.na(df_enlaces$Texto)] <- 'NA'
df_enlaces$URL[is.na(df_enlaces$URL)] <- 'NA'

# Agrupar por Texto y URL y contar las ocurrencias
df_enlaces_con_frecuencia <- aggregate(cbind(Frecuencia = df_enlaces$URL) ~ Texto + URL, data = df_enlaces, FUN = length)

# Ordenar el data frame por Frecuencia de mayor a menor para mejor visualización
df_enlaces_con_frecuencia <- df_enlaces_con_frecuencia[order(-df_enlaces_con_frecuencia$Frecuencia), ]

# Imprimir la tabla para verificar
View(df_enlaces_con_frecuencia)
```

#### 5. Para cada enlace, seguirlo e indicar si está activo (podemos usar el código de status HTTP al hacer una petición a esa URL).

Iteramos sobre cada enlace para verificar su estado utilizando la función `HEAD` de la librería `httr`.
Clasificamos las URLs como absolutas o relativas y realizamos las peticiones correspondientes, teniendo cuidado de pausar entre cada petición para evitar ser bloqueados por el servidor.


```r
# Cargar libreria
library(httr)

# Asumiendo que ya tienes el data frame df_enlaces con las columnas Texto y URL
# Definir el dominio base para construir las URLs absolutas
base_url <- "https://www.mediawiki.org"

# Crear un nuevo data frame para almacenar los resultados, si no existe
if (!exists("df_enlaces_estado")) {
  df_enlaces_estado <- df_enlaces
  df_enlaces_estado$status_code <- NA  # Inicializar la columna de códigos de estado
}

# Iterar sobre cada enlace y verificar el estado si no se ha verificado previamente
for (i in seq_len(nrow(df_enlaces_estado))) {
  # Si ya se verificó el estado de este enlace, saltar a la siguiente iteración
  if (!is.na(df_enlaces_estado$status_code[i])) {
    next
  }
  
  # Extraer la URL actual
  url <- df_enlaces_estado$URL[i]
  
  # Verificar si la URL es relativa y convertirla a absoluta si es necesario
  if (startsWith(url, "//")) {
    url <- paste0("https:", url)
  } else if (startsWith(url, "/")) {
    url <- paste0(base_url, url)
  } else if (startsWith(url, "#")) {
    url <- base_url # URL interna con ancla
  }
  
  # Realizar una petición HEAD para obtener el código de estado
  response <- tryCatch({
    HEAD(url)
  }, error = function(e) {
    list(status_code = NA) # En caso de error, asignar NA
  })
  
  # Almacenar el código de estado en el nuevo data frame
  df_enlaces_estado$status_code[i] <- response$status_code
  
  # Imprimir el índice del enlace y el código de estado para seguimiento
  cat("Enlace", i, "de", nrow(df_enlaces_estado), "-", url, "Estado:", df_enlaces_estado$status_code[i], "\n")

}
```

```
## Enlace 1 de 172 - https://www.mediawiki.org Estado: 200 
## Enlace 2 de 172 - https://www.mediawiki.org/wiki/MediaWiki Estado: 200 
## Enlace 3 de 172 - https://www.mediawiki.org/wiki/Download Estado: 200 
## Enlace 4 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Category:Extensions Estado: 200 
## Enlace 5 de 172 - https://techblog.wikimedia.org/ Estado: 200 
## Enlace 6 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/How_to_contribute Estado: 200 
## Enlace 7 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Help:Contents Estado: 200 
## Enlace 8 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:FAQ Estado: 200 
## Enlace 9 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:Contents Estado: 200 
## Enlace 10 de 172 - https://www.mediawiki.org/wiki/Project:Support_desk Estado: 200 
## Enlace 11 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Communication Estado: 200 
## Enlace 12 de 172 - https://developer.wikimedia.org/ Estado: 200 
## Enlace 13 de 172 - https://www.mediawiki.org/wiki/Development_statistics Estado: 200 
## Enlace 14 de 172 - https://www.mediawiki.org/wiki/Project:Help Estado: 200 
## Enlace 15 de 172 - https://www.mediawiki.org/wiki/Special:RecentChanges Estado: 200 
## Enlace 16 de 172 - https://www.mediawiki.org/wiki/Special:LanguageStats Estado: 200 
## Enlace 17 de 172 - https://www.mediawiki.org/wiki/Special:Random Estado: 200 
## Enlace 18 de 172 - https://www.mediawiki.org/wiki/Project:Village_Pump Estado: 200 
## Enlace 19 de 172 - https://www.mediawiki.org/wiki/Project:Sandbox Estado: 200 
## Enlace 20 de 172 - https://www.mediawiki.org/wiki/MediaWiki Estado: 200 
## Enlace 21 de 172 - https://www.mediawiki.org/wiki/Special:Search Estado: 200 
## Enlace 22 de 172 - https://www.mediawiki.org Estado: 200 
## Enlace 23 de 172 - https://www.mediawiki.org/w/index.php?title=Special:CreateAccount&returnto=MediaWiki Estado: 200 
## Enlace 24 de 172 - https://www.mediawiki.org/w/index.php?title=Special:UserLogin&returnto=MediaWiki Estado: 200 
## Enlace 25 de 172 - https://www.mediawiki.org/w/index.php?title=Special:CreateAccount&returnto=MediaWiki Estado: 200 
## Enlace 26 de 172 - https://www.mediawiki.org/w/index.php?title=Special:UserLogin&returnto=MediaWiki Estado: 200 
## Enlace 27 de 172 - https://www.mediawiki.org/wiki/Help:Introduction Estado: 404 
## Enlace 28 de 172 - https://www.mediawiki.org/wiki/Special:MyContributions Estado: 200 
## Enlace 29 de 172 - https://www.mediawiki.org/wiki/Special:MyTalk Estado: 200 
## Enlace 30 de 172 - https://www.mediawiki.org/wiki/MediaWiki Estado: 200 
## Enlace 31 de 172 - https://www.mediawiki.org/wiki/Talk:MediaWiki Estado: 200 
## Enlace 32 de 172 - https://www.mediawiki.org/wiki/MediaWiki Estado: 200 
## Enlace 33 de 172 - https://www.mediawiki.org/w/index.php?title=MediaWiki&action=edit Estado: 200 
## Enlace 34 de 172 - https://www.mediawiki.org/w/index.php?title=MediaWiki&action=history Estado: 200 
## Enlace 35 de 172 - https://www.mediawiki.org/wiki/MediaWiki Estado: 200 
## Enlace 36 de 172 - https://www.mediawiki.org/w/index.php?title=MediaWiki&action=edit Estado: 200 
## Enlace 37 de 172 - https://www.mediawiki.org/w/index.php?title=MediaWiki&action=history Estado: 200 
## Enlace 38 de 172 - https://www.mediawiki.org/wiki/Special:WhatLinksHere/MediaWiki Estado: 200 
## Enlace 39 de 172 - https://www.mediawiki.org/wiki/Special:RecentChangesLinked/MediaWiki Estado: 200 
## Enlace 40 de 172 - https://commons.wikimedia.org/wiki/Special:UploadWizard Estado: 200 
## Enlace 41 de 172 - https://www.mediawiki.org/wiki/Special:SpecialPages Estado: 200 
## Enlace 42 de 172 - https://www.mediawiki.org/w/index.php?title=MediaWiki&oldid=6287429 Estado: 200 
## Enlace 43 de 172 - https://www.mediawiki.org/w/index.php?title=MediaWiki&action=info Estado: 200 
## Enlace 44 de 172 - https://www.mediawiki.org/w/index.php?title=Special:CiteThisPage&page=MediaWiki&id=6287429&wpFormIdentifier=titleform Estado: 200 
## Enlace 45 de 172 - https://www.mediawiki.org/w/index.php?title=Special:UrlShortener&url=https%3A%2F%2Fwww.mediawiki.org%2Fwiki%2FMediaWiki Estado: 200 
## Enlace 46 de 172 - https://www.wikidata.org/wiki/Special:EntityPage/Q5296 Estado: 200 
## Enlace 47 de 172 - https://www.mediawiki.org/w/index.php?title=Special:Book&bookcmd=book_creator&referer=MediaWiki Estado: 200 
## Enlace 48 de 172 - https://www.mediawiki.org/w/index.php?title=Special:DownloadAsPdf&page=MediaWiki&action=show-download-screen Estado: 200 
## Enlace 49 de 172 - https://www.mediawiki.org/w/index.php?title=MediaWiki&printable=yes Estado: 200 
## Enlace 50 de 172 - https://commons.wikimedia.org/wiki/Main_Page Estado: 200 
## Enlace 51 de 172 - https://foundation.wikimedia.org/wiki/Home Estado: 200 
## Enlace 52 de 172 - https://meta.wikimedia.org/wiki/Main_Page Estado: 200 
## Enlace 53 de 172 - https://outreach.wikimedia.org/wiki/Main_Page Estado: 200 
## Enlace 54 de 172 - https://wikisource.org/wiki/Main_Page Estado: 200 
## Enlace 55 de 172 - https://species.wikimedia.org/wiki/Main_Page Estado: 200 
## Enlace 56 de 172 - https://en.wikibooks.org/wiki/Main_Page Estado: 200 
## Enlace 57 de 172 - https://www.wikidata.org/wiki/Wikidata:Main_Page Estado: 200 
## Enlace 58 de 172 - https://www.wikifunctions.org/wiki/Wikifunctions:Main_Page Estado: 200 
## Enlace 59 de 172 - https://wikimania.wikimedia.org/wiki/Wikimania Estado: 200 
## Enlace 60 de 172 - https://en.wikinews.org/wiki/Main_Page Estado: 200 
## Enlace 61 de 172 - https://en.wikipedia.org/wiki/Main_Page Estado: 200 
## Enlace 62 de 172 - https://en.wikiquote.org/wiki/Main_Page Estado: 200 
## Enlace 63 de 172 - https://en.wikisource.org/wiki/Main_Page Estado: 200 
## Enlace 64 de 172 - https://en.wikiversity.org/wiki/Wikiversity:Main_Page Estado: 200 
## Enlace 65 de 172 - https://en.wikivoyage.org/wiki/Main_Page Estado: 200 
## Enlace 66 de 172 - https://en.wiktionary.org/wiki/Wiktionary:Main_Page Estado: 200 
## Enlace 67 de 172 - https://www.mediawiki.org/wiki/File:At_Wikimedia_Hackathon_Athens_(MP)_2023_001_(cropped).jpg Estado: 200 
## Enlace 68 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Wikimedia_Hackathon_2023 Estado: 200 
## Enlace 69 de 172 - https://www.mediawiki.org/wiki/Wikimedia_Hackathon_2023 Estado: 200 
## Enlace 70 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Sites_using_MediaWiki Estado: 200 
## Enlace 71 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/MediaWiki_testimonials Estado: 200 
## Enlace 72 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Localisation Estado: 200 
## Enlace 73 de 172 - https://en.wikipedia.org/wiki/FLOSS Estado: 200 
## Enlace 74 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:What_is_MediaWiki%3F Estado: 200 
## Enlace 75 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:Deciding_whether_to_use_a_wiki_as_your_website_type Estado: 200 
## Enlace 76 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Download Estado: 200 
## Enlace 77 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:Installation_guide Estado: 200 
## Enlace 78 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:System_administration Estado: 200 
## Enlace 79 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:Extensions Estado: 200 
## Enlace 80 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:Errors_and_symptoms Estado: 200 
## Enlace 81 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Manual:FAQ Estado: 200 
## Enlace 82 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Hosting_services Estado: 200 
## Enlace 83 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Professional_development_and_consulting Estado: 200 
## Enlace 84 de 172 - https://www.mediawiki.org/wiki/Professional_development_and_consulting Estado: 200 
## Enlace 85 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/MediaWiki_Stakeholders%27_Group Estado: 200 
## Enlace 86 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Help:Navigation Estado: 200 
## Enlace 87 de 172 - https://www.mediawiki.org/wiki/Help:Navigation Estado: 200 
## Enlace 88 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Help:Editing_pages Estado: 200 
## Enlace 89 de 172 - https://www.mediawiki.org/wiki/Help:Editing_pages Estado: 200 
## Enlace 90 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Help:Contents Estado: 200 
## Enlace 91 de 172 - https://www.mediawiki.org/wiki/Help:Contents Estado: 200 
## Enlace 92 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Developer_hub Estado: 200 
## Enlace 93 de 172 - https://developer.wikimedia.org Estado: 200 
## Enlace 94 de 172 - https://www.mediawiki.org/wiki/Project:Support_desk Estado: 200 
## Enlace 95 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/How_to_contribute Estado: 200 
## Enlace 96 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/How_to_report_a_bug Estado: 200 
## Enlace 97 de 172 - https://www.mediawiki.org/wiki/How_to_report_a_bug Estado: 200 
## Enlace 98 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Events/FOSDEM/2024 Estado: 200 
## Enlace 99 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Wikimedia_Hackathon_2024 Estado: 200 
## Enlace 100 de 172 - https://lists.wikimedia.org/hyperkitty/list/mediawiki-announce@lists.wikimedia.org/thread/OMDFHJ2SKKJH775RW4UTC754OY4TP7UU/ Estado: 200 
## Enlace 101 de 172 - https://lists.wikimedia.org/hyperkitty/list/mediawiki-l@lists.wikimedia.org/thread/72B3GNSDFUJEM23W54735OW34IHCZL6F/ Estado: 200 
## Enlace 102 de 172 - https://lists.wikimedia.org/hyperkitty/list/mediawiki-announce@lists.wikimedia.org/message/TDBUBCCOQJUT4SCHJNPHKQNPBUUETY52/ Estado: 200 
## Enlace 103 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/News Estado: 200 
## Enlace 104 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Project:Language_policy Estado: 200 
## Enlace 105 de 172 - https://www.mediawiki.org/wiki/Project:Language_policy Estado: 200 
## Enlace 106 de 172 - https://www.mediawiki.org/wiki/Template:Main_page Estado: 200 
## Enlace 107 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/af Estado: 200 
## Enlace 108 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/ar Estado: 200 
## Enlace 109 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/ast Estado: 200 
## Enlace 110 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/be Estado: 200 
## Enlace 111 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/be-tarask Estado: 200 
## Enlace 112 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/bg Estado: 200 
## Enlace 113 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/bn Estado: 200 
## Enlace 114 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/bs Estado: 200 
## Enlace 115 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/ca Estado: 200 
## Enlace 116 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/ckb Estado: 200 
## Enlace 117 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/cs Estado: 200 
## Enlace 118 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/da Estado: 200 
## Enlace 119 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/de Estado: 200 
## Enlace 120 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/es Estado: 200 
## Enlace 121 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/fa Estado: 200 
## Enlace 122 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/fi Estado: 200 
## Enlace 123 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/fr Estado: 200 
## Enlace 124 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/gl Estado: 200 
## Enlace 125 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/gu Estado: 200 
## Enlace 126 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/he Estado: 200 
## Enlace 127 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/hi Estado: 200 
## Enlace 128 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/hr Estado: 200 
## Enlace 129 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/hu Estado: 200 
## Enlace 130 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/hy Estado: 200 
## Enlace 131 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/id Estado: 200 
## Enlace 132 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/it Estado: 200 
## Enlace 133 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/ja Estado: 200 
## Enlace 134 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/jv Estado: 200 
## Enlace 135 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/kk Estado: 200 
## Enlace 136 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/ko Estado: 200 
## Enlace 137 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/ms Estado: 200 
## Enlace 138 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/mwl Estado: 200 
## Enlace 139 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/nl Estado: 200 
## Enlace 140 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/pl Estado: 200 
## Enlace 141 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/pt Estado: 200 
## Enlace 142 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/pt-br Estado: 200 
## Enlace 143 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/ro Estado: 200 
## Enlace 144 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/ru Estado: 200 
## Enlace 145 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/sc Estado: 200 
## Enlace 146 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/si Estado: 200 
## Enlace 147 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/sk Estado: 200 
## Enlace 148 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/sl Estado: 200 
## Enlace 149 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/so Estado: 200 
## Enlace 150 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/sq Estado: 200 
## Enlace 151 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/sr Estado: 200 
## Enlace 152 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/sv Estado: 200 
## Enlace 153 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/th Estado: 200 
## Enlace 154 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/tr Estado: 200 
## Enlace 155 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/uk Estado: 200 
## Enlace 156 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/vi Estado: 200 
## Enlace 157 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/yue Estado: 200 
## Enlace 158 de 172 - https://www.mediawiki.org/wiki/Template:Main_page/zh Estado: 200 
## Enlace 159 de 172 - https://www.mediawiki.org/w/index.php?title=MediaWiki&oldid=6287429 Estado: 200 
## Enlace 160 de 172 - https://www.mediawiki.org/wiki/Category:Languages_pages Estado: 200 
## Enlace 161 de 172 - https://creativecommons.org/licenses/by-sa/4.0/deed.en Estado: 200 
## Enlace 162 de 172 - https://foundation.wikimedia.org/wiki/Special:MyLanguage/Policy:Terms_of_Use Estado: 200 
## Enlace 163 de 172 - https://foundation.wikimedia.org/wiki/Special:MyLanguage/Policy:Privacy_policy Estado: 200 
## Enlace 164 de 172 - https://www.mediawiki.org/wiki/Project:About Estado: 200 
## Enlace 165 de 172 - https://www.mediawiki.org/wiki/Project:General_disclaimer Estado: 200 
## Enlace 166 de 172 - https://www.mediawiki.org/wiki/Special:MyLanguage/Code_of_Conduct Estado: 200 
## Enlace 167 de 172 - https://developer.wikimedia.org Estado: 200 
## Enlace 168 de 172 - https://stats.wikimedia.org/#/www.mediawiki.org Estado: 200 
## Enlace 169 de 172 - https://foundation.wikimedia.org/wiki/Special:MyLanguage/Policy:Cookie_statement Estado: 200 
## Enlace 170 de 172 - https://m.mediawiki.org/w/index.php?title=MediaWiki&mobileaction=toggle_view_mobile Estado: 200 
## Enlace 171 de 172 - https://wikimediafoundation.org/ Estado: 200 
## Enlace 172 de 172 - https://www.mediawiki.org/ Estado: 200
```

```r
# Imprimir los resultados para verificar
View(df_enlaces_estado)
```

## Pregunta 2: Infografía con gráficos

Elaborad, usando las librerías de gráficos base y qplot (ggplot2), una infografía sobre los datos obtenidos.
Tal infografía será una reunión de gráficos donde se muestren los siguientes detalles:

#### Un histograma con la frecuencia de aparición de los enlaces, pero separado por URLs absolutas (con “http…”) y URLs relativas.

Elaboramos una infografía utilizando las librerías de gráficos base y `qplot` (parte de `ggplot2`) para visualizar los datos obtenidos del web scraping.

Esta infografía incluye un histograma de la frecuencia de aparición de los enlaces, un gráfico de barras que compara enlaces a otros dominios frente a enlaces internos, y un gráfico de tarta que muestra los porcentajes de los códigos de estado HTTP.
Para diferenciar entre enlaces internos y externos, añadimos una columna `TipoURL` al data frame `df_enlaces_estado` que clasifica cada URL como "Absoluta" o "Relativa" basándose en si comienza con "http".


```r
library(ggplot2)

# Agregar una nueva columna que clasifique cada URL como absoluta o relativa
df_enlaces_estado$TipoURL <- ifelse(grepl("^http", df_enlaces_estado$URL), "Absoluta", "Relativa")

# Crear un gráfico de barras con ggplot2
grafico_barras_urls <- ggplot(df_enlaces_estado, aes(x = TipoURL, fill = TipoURL)) +
  geom_bar() + # Usar geom_bar para gráfico de barras
  geom_text(aes(label=after_stat(count)), stat='count', vjust=-0.5) + # Añadir el valor encima de las barras
  labs(title = "Frecuencia de URLs Absolutas y Relativas",
       x = "Tipo de URL",
       y = "Frecuencia") +
  scale_fill_manual(values = c("Absoluta" = "blue", "Relativa" = "red"))

# Mostrar el gráfico de barras
print(grafico_barras_urls)
```

![](Practica-2_files/figure-html/histograma-1.png)<!-- -->

#### Un gráfico de barras indicando la suma de enlaces que apuntan a otros dominios o servicios (distinto a <https://www.mediawiki.org> en el caso de ejemplo) vs. la suma de los otros enlaces.

El código para este gráfico de barras comienza definiendo la función `es_dominio_base`, que verifica si una URL dada apunta al dominio base de MediaWiki o a un dominio diferente.
Luego, aplicamos esta función a todas las URLs en `df_enlaces_estado` y creamos un conteo de enlaces por destino.
Finalmente, visualizamos este conteo en un gráfico de barras con `ggplot2`, donde las barras se colorean según el destino y se etiquetan con el número total de enlaces para cada categoría, proporcionando una comparación clara entre enlaces internos y externos.


```r
# Definir el dominio base para la comparación
dominio_base <- "https://www.mediawiki.org"

# Función para determinar si la URL apunta al dominio base o a otro dominio
es_dominio_base <- function(url) {
  # Comprobar si la URL es relativa
  if (startsWith(url, "/")) {
    return(TRUE)
  }
  # Comprobar si la URL es absoluta y pertenece al dominio base
  dominio <- sub("^(https?://[^/]+).*", "\\1", url)
  return(dominio == dominio_base)
}

# Aplicar la función a cada URL para clasificar los enlaces
df_enlaces_estado$Destino <- sapply(df_enlaces_estado$URL, es_dominio_base)

# Convertir los resultados booleanos a factores con etiquetas "Mediawiki" y "Otro Dominio"
df_enlaces_estado$Destino <- ifelse(df_enlaces_estado$Destino, "Mediawiki", "Otro Dominio")

# Contar la cantidad de enlaces que apuntan a cada destino
conteo_destinos <- table(df_enlaces_estado$Destino)

# Crear un data frame a partir del conteo para el gráfico
df_conteo_destinos <- as.data.frame(conteo_destinos)

# Crear un gráfico de barras con ggplot2
grafico_barras_destinos <- ggplot(df_conteo_destinos, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = -0.5) +
  labs(title = "Suma de Enlaces por Destino",
       x = "Destino",
       y = "Cantidad de Enlaces") +
  scale_fill_manual(values = c("Mediawiki" = "green", "Otro Dominio" = "orange"))

# Mostrar el gráfico de barras
print(grafico_barras_destinos)
```

![](Practica-2_files/figure-html/grafico barras-1.png)<!-- -->

#### Un gráfico de tarta (pie chart) indicando los porcentajes de Status de nuestro análisis.

Para representar visualmente la distribución de los códigos de estado HTTP obtenidos durante el análisis de enlaces, hemos creado un gráfico de tarta utilizando `ggplot2`.
El procedimiento comienza con la creación de un conteo de la frecuencia de cada código de estado, que luego convertimos en un data frame `df_conteo_status` para su uso.


```r
# Cargar librerias
library(ggplot2)

# Contar la frecuencia de cada código de estado
conteo_status <- table(df_enlaces_estado$status_code)

# Crear un data frame a partir del conteo para el gráfico
df_conteo_status <- as.data.frame(conteo_status)

# Renombrar las columnas para que sean más descriptivas
names(df_conteo_status) <- c("status_code", "count")

# Crear un gráfico de tarta con ggplot2
grafico_tarta_status <- ggplot(df_conteo_status, aes(x = "", y = count, fill = factor(status_code))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title = element_blank()) +
  labs(title = "Distribución de Códigos de Estado HTTP", # Añadir título aquí
       fill = "Status Code") +
  geom_text(aes(label = paste0(round(count / sum(count) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5))

# Mostrar el gráfico de tarta
print(grafico_tarta_status)
```

![](Practica-2_files/figure-html/pie chart-1.png)<!-- -->

Finalmente, utilizando `gridExtra`, combinamos el gráfico de tarta con los gráficos de barras anteriores en un solo gráfico compuesto para una presentación integrada de todos los hallazgos visuales.
Este enfoque facilita la comparación y el análisis conjunto de las diferentes métricas visuales generadas a partir de los datos de enlaces de la página web.


```r
# Cargar librerias
library(gridExtra)

# Combinar los gráficos en una sola figura
grid.arrange(grafico_barras_urls, grafico_barras_destinos, grafico_tarta_status, ncol = 2)
```

![](Practica-2_files/figure-html/grid graficos-1.png)<!-- -->

# 2: Análisis de logs de servidor usando R (parte II)

Queremos programar un script con el que podamos hacer una investigación forense sobre un fichero de logs de un servidor de tipo Apache.
Los datos del registro del servidor están en el formato estándar e incluyen miles de registros sobre las distintas peticiones gestionadas por el servidor web.
Nuestro programa ha de ser capaz de obtener las respuestas de forma dinámica a las siguientes preguntas utilizando instrucciones de código en R:

### Obtención y carga de los Datos

En esta sección, nos centramos en el análisis forense de un fichero de logs de un servidor Apache.
Realizamos la descompresión del fichero, la carga de los registros en un data frame y la limpieza de los datos para asegurar que estén en un formato adecuado para el análisis.

#### 1. Descomprimir el fichero comprimido que contiene los registros del servidor, y a partir de los datos extraídos, cargar en data frame los registros con las peticiones servidas.


```r
library(readr)
data <- read_table("epa-http.csv", col_names = FALSE)
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   X1 = col_character(),
##   X2 = col_character(),
##   X3 = col_character(),
##   X4 = col_character(),
##   X5 = col_character(),
##   X6 = col_double(),
##   X7 = col_character()
## )
```

```
## Warning: 21 parsing failures.
##  row col  expected    actual           file
## 7527  -- 7 columns 6 columns 'epa-http.csv'
## 7528  -- 7 columns 6 columns 'epa-http.csv'
## 7529  -- 7 columns 6 columns 'epa-http.csv'
## 7549  -- 7 columns 6 columns 'epa-http.csv'
## 7550  -- 7 columns 6 columns 'epa-http.csv'
## .... ... ......... ......... ..............
## See problems(...) for more details.
```

```r
colnames(data) <- c("IP", "Timestamp", "Tipo", "URL", "Protocolo", "Respuesta", "Bytes")
View(data)
```

En este primer apartado hemos decomprimido el fichero epa-http.csv con la función read_table de la libreria readr.
A continuación, hemos añadido el nombre de cada columna, en este caso: "IP", "Timestamp", "Tipo", "URL", "Protocolo", "Respuesta" y "Bytes".

#### 2. Incluid en el documento un apartado con la descripción de los datos analizados: fuente, tipología, descripción de la información contenida (los diferentes campos) y sus valores.

Al descomprimir el fichero y definirlo como un dataframe, podemos observar que tipo de dato tenemos en cada columna.
La primera columna se trata de de las IPs que hacen la petición al servidor, se trata de una variable de tipo "character".
La segunda columna contiene la fecha y la hora a la que se ha realizado la petición en formato [dia:hora:minuto:segundo] y también es de tipo "character".
La tercera columna contiene el tipo de petición que hace al servidor, también es tipo "character" y haciendo un unique(data$Tipo) vemos que esta variable solo puede tener tres valores: GET, POST o HEAD. La cuarta columna, contiene la URL de la petición, también es de tipo "character". La quinta columna nos muestra el protocolo HTTP utilizado para hacer la petición, como el resto de columnas es de tipo "character" y a partir de la función unique(data$Protocolo) vemos que la variable solo puede tener dos valores: HTTP/1.0 o HTTP/0.2.
La sexta columna contiene la respuesta de la petición HTTP, es de tipo "numeric" y vemos que los códigos de respuesta HTTP que podemos encontrar en el fichero son: 200 302 304 404 403 501 500 y 400.
Finalmente, la séptima columna contine la cantidad de Bytes enviados y es de tipo "character" porque contiene valores NA.

### Limpieza de los Datos

#### 3. Aprovechando que los datos a analizar son los mismos de la primera práctica, para esta entrega es imprescindible que los datos estén en formato de “datos elegantes”.


```r
# Código para limpiar y preparar los datos

data$IP <- trimws(data$IP)
data$Timestamp <- sub("^\\[", '', data$Timestamp)
data$Timestamp <- sub("\\]$", '', data$Timestamp)
data$Timestamp <- as.POSIXct(data$Timestamp, format = "%d:%H:%M:%S")
data$Tipo <- sub('"', '', data$Tipo)
data$URL <- trimws(data$URL)
data$Protocolo <- sub('"', '', data$Protocolo)
data$Respuesta <- as.numeric(data$Respuesta)
data$Bytes <- as.numeric(data$Bytes)
```

```
## Warning: NAs introduced by coercion
```

```r
View(data)
```

Para limpiar y preparar los datos para su posterior análisi hemos decidido realizar el siguiente procedimiento: En el caso de la columna IP hemos utilizado la función trimws() para eliminar cualquier espacio que haya al principio o al final del valor de la IP.
En la columna de Timestamp hemos eliminado lo "[" y "]" para poder obtener el valor en formato dia:hora:minuto:segundo y a partir de ese valor usar la función as.POSIXct() para obtener el valor de la fecha y hora en la que se ejecutó la petición.
En la columna Tipo hemos eliminado las " del inicio del valor. En la columna URL hemos usado la misma función que en la columna IP para limpiar cualquier espacio que haya al inicio o final de la petición. EN la columna Protocolo hemos eliminado las " que habia al final de todos los valores de la variable.
Las columnas Respuesta y Bytes las hemos definido como columnas de variables tipo "numeric"

### Exploración de Datos

#### 4. Identificar el número único de usuarios que han interactuado directamente con el servidor de forma segregada según si los usuarios han tenido algún tipo de error en las distintas peticiones ofrecidas por el servidor.

Identificamos el número único de usuarios que han interactuado con el servidor y aquellos que han experimentado errores, proporcionando una descripción de los diferentes tipos de errores encontrados en la muestra de datos.


```r
# Código para explorar los datos y obtener estadísticas
usuarios_unicos <- unique(data$IP)
print(paste("Hay", length(usuarios_unicos), "usuarios unicos"))
```

```
## [1] "Hay 2333 usuarios unicos"
```

```r
usuarios_con_error <- unique(data$IP[data$Respuesta >= 400 & data$Respuesta <= 599])
print(paste("Número de usuarios únicos con error:", length(usuarios_con_error)))
```

```
## [1] "Número de usuarios únicos con error: 192"
```

```r
usuarios_sin_error <- setdiff(usuarios_unicos, usuarios_con_error)
print(paste("Número de usuarios únicos sin error:", length(usuarios_sin_error)))
```

```
## [1] "Número de usuarios únicos sin error: 2141"
```

```r
codigos_error_unicos <- unique(data$Respuesta[data$Respuesta >= 400 & data$Respuesta <= 599])
print(paste("Código de error:", codigos_error_unicos) )
```

```
## [1] "Código de error: 404" "Código de error: 403" "Código de error: 501"
## [4] "Código de error: 500" "Código de error: 400"
```

En el apartado de exploración de datos hemos usado la función unique() para ver cuantos usuarios unicos hay en el fichero.
A continuación, hemos obtenido el número de usuarios con error en alguna petición asumiendo que cualquier respuesta HTTP con un valor entre 400 y 599 se considera un error.
Finalmente, hemos obtenido el número de usuarios sin error haciendo la diferencia entre el total y los que tienen error y hemos extraído los códigos de error que podemos encontrar en el fichero.

### Análisis de Datos

#### 5. Analizar los distintos tipos de peticiones HTTP (GET, POST, PUT, DELETE) gestionadas por el servidor, identificando la frecuencia de cada una de estas. Repetir el análisis, esta vez filtrando previamente aquellas peticiones correspondientes a recursos ofrecidos de tipo imagen.

Analizamos los distintos tipos de peticiones HTTP gestionadas por el servidor y filtramos aquellas correspondientes a recursos de tipo imagen.


```r
library(stringr)
# Código para analizar los tipos de peticiones HTTP
data_freq_tipo <- as.data.frame(table(data$Tipo))
colnames(data_freq_tipo) <- c("Tipo" , "Frecuencia")
View(data_freq_tipo)


# Filtrar datos donde el valor de data$URL termina en ".jpg o .gif o .xbm"
datos_gif <- subset(data, str_detect(data$URL, "\\.gif"))
datos_jpg <- subset(data, str_detect(data$URL, "\\.jpg"))
datos_xbm <- subset(data, str_detect(data$URL, "\\.xbm"))

datos_combinados <- rbind(datos_gif, datos_jpg, datos_xbm)


# Crear un data frame con la frecuencia de respuestas para los datos filtrados
data_freq_tipo_2 <- as.data.frame(table(datos_combinados$Tipo))
colnames(data_freq_tipo_2) <- c("Tipo", "Frecuencia")

# Ver el data frame resultante
View(data_freq_tipo_2)
```

En este apartado, primero hemos realizado la clasificación de la peticiones al servidor en función del tipo de petición que realiza y hemos generado el dataframe "data_freq_tipo".
A continuación, observando los datos hemos visto que podiamos encontrar tres tipos de formato de imagen: .gif, .jpg y .xbm, por lo tanto, hemos filtrado solo las URL que acababan con dichos formatos y hemos generado un data frame similar al anterior clasificando las peticiones por tipo.

### Visualización de Resultados

#### 6. Generar al menos 2 gráficos distintos que permitan visualizar alguna característica relevante de los datos analizados.

Generamos gráficos que permiten visualizar características relevantes de los datos analizados, incluyendo un gráfico de tarta para las peticiones HTTP y un gráfico circular para comparar peticiones de imagen con otras peticiones.


```r
library(ggplot2)


# Crear un gráfico de tarta con ggplot2
grafico_tarta_tipo <- ggplot(data_freq_tipo, aes(x = "", y = Frecuencia, fill = factor(Tipo))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.title = element_blank()) +
  labs(title = "Distribución de Códigos de Tipo de peticion HTTP", # Añadir título aquí
       fill = "Status Code") +
  geom_text(aes(label = paste0(round(Frecuencia / sum(Frecuencia) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5))

# Mostrar el gráfico de tarta
print(grafico_tarta_tipo)
```

![](Practica-2_files/figure-html/grafico 1 parte 2-1.png)<!-- -->

En este primer apartado, hemos generado un gráfico tipo tarta que separa las peticiones por tipo de petición mostrando en que porcentaje se usa cada una en el conjunto de datos


```r
library(ggplot2)

peticiones_total <- sum(data_freq_tipo$Frecuencia)

peticiones_imagen <- sum(data_freq_tipo_2$Frecuencia)

otras_peticiones <- peticiones_total - peticiones_imagen


# Datos de ejemplo
valores <- c(peticiones_imagen, otras_peticiones)
categorias <- c("Peticiones imagen", "Otras peticiones")

# Calcular porcentajes
porcentajes <- round(valores / sum(valores) * 100, 1)

# Crear el gráfico circular
pie(valores, labels = paste(categorias, "\n", porcentajes, "%"), col = c("red", "blue"), main = "Gráfico Circular")

# Agregar leyenda
legend("topright", legend = categorias, fill = c("red", "blue"))
```

![](Practica-2_files/figure-html/grafico 2 parte 2-1.png)<!-- -->

En este segundo apartado hemos mostrado en un gráfico circular el porcentaje de peticiones que son de imagen, en relación al número de peticiones total

#### 7. Generar un gráfico que permita visualizar el número de peticiones servidas a lo largo del tiempo.


```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following object is masked from 'package:gridExtra':
## 
##     combine
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
# Ajustar la conversión de la columna Timestamp al formato correcto
# Asumiendo que el formato de la fecha es "YYYY-MM-DD HH:MM:SS"
data$Timestamp <- ymd_hms(data$Timestamp)
```

```
## Warning: 1 failed to parse.
```

```r
# Agrupar los datos por intervalos de tiempo (por ejemplo, por hora)
# y contar el número de peticiones en cada intervalo
data_agrupada <- data %>%
  mutate(Hour = floor_date(Timestamp, "hour")) %>%
  group_by(Hour) %>%
  summarise(NumeroPeticiones = n())

# Crear un gráfico de líneas para visualizar el número de peticiones a lo largo del tiempo
ggplot(data_agrupada, aes(x = Hour, y = NumeroPeticiones)) +
  geom_line() +
  labs(title = "Número de peticiones servidas a lo largo del tiempo",
       x = "Hora",
       y = "Número de Peticiones") +
  theme_minimal()
```

```
## Warning: Removed 1 row containing missing values (`geom_line()`).
```

![](Practica-2_files/figure-html/timeline peticiones-1.png)<!-- -->

En este apartado, el código convierte las fechas y horas en la columna "Timestamp" a un formato adecuado, utilizando la libraria lubridate y luego agrupa los datos por hora, contando el número de peticiones en cada intervalo de tiempo.
Finalmente, se crea el gráfico que muestra la cantidad de peticiones hechas en cada hora.

### Clústering de datos

#### 8. Utilizando un algoritmo de aprendizaje no supervisado, realizad un análisis de clústering con k-means para los datos del servidor.

#### 9. Representad visualmente en gráficos de tipo scatter plot el resultado de vuestros clústering y interpretad el resultado obtenido (describid las características de los distintos grupos) con los 2 valores distintos de k probados en el apartado anterior en función de los valores de las variables y el número de clúster asignado.

Para realizar un análisis de clústering de los datos del servidor web Apache, primero llevamos a cabo una serie de pasos preliminares con el objetivo de preparar los datos para el modelado.

Cargamos el conjunto de datos desde un archivo CSV, asignamos nombres descriptivos a las columnas y realizamos una limpieza de datos que incluye la eliminación de comillas y la asignación de 0 a los valores faltantes en la columna de 'Bytes'.

Además, creamos una nueva columna numérica `resource_length` que representa la longitud de la URL servida.
Para manejar las variables categóricas 'Tipo' y 'Protocolo', aplicamos la técnica de one-hot encoding, transformándolas en columnas numéricas que son adecuadas para su uso en algoritmos de aprendizaje automático que requieren entradas numéricas.

Con los datos preparados, ejecutamos el algoritmo k-means para dos números diferentes de clústeres (k=2 y k=3) y agregamos las asignaciones de clúster resultantes al data frame original.
Para visualizar los resultados del clústering, generamos gráficos de dispersión con `ggplot2`, utilizando la longitud de la URL y los bytes enviados como ejes, y coloreando los puntos según el clúster asignado.


```r
library(dplyr)
library(data.table)
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
##     yday, year
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```r
library(mltools)  # one_hot
library(readr)
library(ggplot2)

# Cargar los datos
epa_http <- read_table("./epa-http.csv", col_names = FALSE, col_types = cols(
  col_character(),  # IP
  col_datetime(format = ""),  # TIMESTAMP
  col_character(),  # Tipo
  col_character(),  # URL
  col_character(),  # Protocolo
  col_skip(),       # Respuesta (omitida si no es relevante para el clústering)
  col_double()      # Bytes
), na = c("-", ""))
```

```
## Warning: 47769 parsing failures.
## row col   expected        actual             file
##   1  X2 date like  [29:23:53:25] './epa-http.csv'
##   2  X2 date like  [29:23:53:36] './epa-http.csv'
##   3  X2 date like  [29:23:53:53] './epa-http.csv'
##   4  X2 date like  [29:23:54:15] './epa-http.csv'
##   5  X2 date like  [29:23:54:16] './epa-http.csv'
## ... ... .......... ............. ................
## See problems(...) for more details.
```

```r
# Asignar nombres a las columnas
colnames(epa_http) <- c("IP", "TIMESTAMP", "Tipo", "URL", "Protocolo", "Bytes")

# Limpiar los datos
epa_http <- epa_http %>%
  mutate(
    Tipo = gsub('[\"]', '', Tipo),
    Protocolo = gsub('[\"]', '', Protocolo),
    Tipo = as.factor(Tipo),
    Protocolo = as.factor(Protocolo),
    Bytes = ifelse(is.na(Bytes), 0, Bytes),  # Reemplazar NA en Bytes con 0
    resource_length = nchar(URL)
  )

# Realizar one-hot encoding para las columnas categóricas
epa_http_one_hot <- one_hot(as.data.table(epa_http), cols = c("Tipo", "Protocolo"), sparsifyNAs = TRUE)

# Descartar columnas no numéricas
epa_http_one_hot <- epa_http_one_hot %>% select_if(is.numeric)

# Ejecutar k-means para k=2
set.seed(123)  # Establecer una semilla para reproducibilidad
results_k2 <- kmeans(epa_http_one_hot, centers = 2)

# Ejecutar k-means para k=3
results_k3 <- kmeans(epa_http_one_hot, centers = 3)

# Agregar las asignaciones de clúster al conjunto de datos original
epa_http$cluster_k2 <- results_k2$cluster
epa_http$cluster_k3 <- results_k3$cluster

# Visualizar los resultados del clústering para k=2
ggplot(epa_http, aes(x = resource_length, y = Bytes, color = as.factor(cluster_k2))) +
  geom_point() +
  labs(title = "Scatter Plot para k=2",
       x = "Longitud de la URL",
       y = "Bytes Enviados",
       color = "Cluster") +
  theme_minimal()
```

![](Practica-2_files/figure-html/clustering-1.png)<!-- -->

```r
# Visualizar los resultados del clústering para k=3
ggplot(epa_http, aes(x = resource_length, y = Bytes, color = as.factor(cluster_k3))) +
  geom_point() +
  labs(title = "Scatter Plot para k=3",
       x = "Longitud de la URL",
       y = "Bytes Enviados",
       color = "Cluster") +
  theme_minimal()
```

![](Practica-2_files/figure-html/clustering-2.png)<!-- -->

Siguiendo el análisis de clústering realizado con el algoritmo k-means y basándonos en los resultados para k=2 y k=3, hemos descubierto patrones interesantes en la distribución de nuestros datos.
Los gráficos de dispersión generados muestran una agrupación predominante de puntos en la región inferior para ambos grupos de clústeres, lo cual revela que un gran número de URLs, a pesar de su corta longitud, están asociadas con una cantidad considerable de bytes enviados.

Para el clúster 1, que posiblemente incluye la mayoría de los puntos, observamos que las URLs cortas podrían estar vinculadas a recursos de gran tamaño, tales como imágenes de alta resolución o archivos multimedia.

El clúster 2, por otro lado, aunque también caracterizado por URLs cortas, podría estar relacionado con páginas web estáticas o documentos de texto que, en general, tienen tamaños de archivo menores y, por ende, generan menos bytes enviados.

Con respecto al clúster 3, se nota una dispersión más amplia en el gráfico, lo cual puede sugerir una variedad de tipos de recursos servidos, desde páginas dinámicas y servicios interactivos hasta descargas directas, cada uno con diferentes longitudes de URL y volúmenes de datos transmitidos.

La concentración de la mayoría de los puntos en áreas específicas del gráfico sugiere un patrón de comportamiento común en cómo los usuarios interactúan con el servidor o cómo los recursos están estructurados y se accede a ellos.

