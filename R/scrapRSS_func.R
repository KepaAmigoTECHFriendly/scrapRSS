#' @title Scrapea el RSS de un periódico y devuelve un json con las noticas del día actual.
#'
#' @description Recibe la URL RSS de un periódico, lo scrapea, devuelve un json con las noticas del día actual y lo envía a la plataforma Smart City.
#'
#' @param url
#'
#' @return json
#'
#' @examples  scrap_RSS('https://www.deia.eus/rss/economia.xml')
#'
#' @import plyr
#' xml2
#' purrr
#' tidyverse
#' stringr
#' tidyRSS
#' data.table
#' janitor
#' anytime
#' jsonlite
#' httr
#'
#' @export

scrap_RSS <- function(url){

  url <- as.character(url)
  #url <- str_replace_all(url,"[_]","/")

  # COMPORBACI?N RSS DESCARGABLE
  rss_desc_webs <- c("gara", "goiena", "durangon")

  if(grepl(rss_desc_webs,url)){
    archivo_temporal <- tempfile(pattern = "", tmpdir = tempdir(), fileext = ".file")
    # mode = wb es en binary
    download.file(url, destfile = archivo_temporal, mode = "wb")
    url_modif <- archivo_temporal
  }else{
    url_modif <- url
  }

  # INICIO FUNCI?N SCRAPP RSS "PROPIA"
  df_final <- tryCatch(
    {
      #########################################################3
      # FUNCI?N PRINCIPAL

      # COMPORBACI?N RSS DESCARGABLE
      rss_desc_webs <- c("gara", "goiena")

      if(grepl(rss_desc_webs,url)){
        archivo_temporal <- tempfile(pattern = "", tmpdir = tempdir(), fileext = ".file")
        # mode = wb es en binary
        download.file(url, destfile = archivo_temporal, mode = "wb")
        url_modif <- archivo_temporal
      }else{
        url_modif <- url
      }

      ##############################################################
      # DECLARACI?N OBJETOS PARA POSTRIOR USO

      if(any(str_detect(url,"feed"))){
        xml_doc <- read_html(url)
        tree <- xml_children(xml_children(xml_doc))
        tree <- xml_children(xml_children(tree))
      }else{
        xml_doc <- read_xml(url_modif)
        tree <- xml_children(xml_children(xml_doc))
      }

      # Documento XML
      #xml_doc <- xml2::read_xml(url)

      # Estructura XML general y de tag item
      #tree <- xml_children(xml_children(xml_doc))
      tree_item <- xml_find_all(xml_doc, ".//item")

      #Etiquetas de la estructura general XML
      tree_tags <- xml_name(tree)

      #Identificiaci?n de los tags del feed y de items
      feed <- which(tree_tags != "item")
      item <- which(tree_tags == "item")

      # Vector de etiquetas: llenado tags feed
      feed_tags <- tree_tags[feed]

      # Item_tags == item sin duplicados
      item_tags <- unique(xml_name(xml_children(tree)))

      # Vector con el total de etiquetas sin duplicados
      tags <- unique(c(feed_tags, item_tags))

      # Feed_tags requeridos
      feed_tags_req <- c("title", "link", "date", "category")

      # Item_tags requeridos
      item_tags_req <-  c("title", "link", "date", "description", "categories")

      #############################################################
      # EXTRACCI?N DATOS
      #############################################################
      # CAMBIO FORMATO TAGS

      # Tags en formato para xml_find_all() --> ".//"
      str_feed_tags <- paste(".//",feed_tags,sep="")
      str_item_tags <- paste(".//",item_tags,sep="")
      # Tags en formato para cambio de nombre
      name_feed_tags <- paste("feed_",feed_tags,sep="")
      name_item_tags <- paste("item_",item_tags,sep="")

      # Declaraci?n de listas item_tags, feed_tags y category_tags
      list_item_tags <- list()
      list_feed_tags <- list()
      list_category_tags <- list()

      # Identificaci?n fechas item (noticias)
      if(any(grepl("date", str_item_tags, ignore.case = TRUE))){
        item_date_get <- list()
        # Identificaci?n y extraci?n de fechas para modificar formato item
        item_date_ident <- grep("date", str_item_tags, ignore.case = TRUE, value = TRUE)
        item_date_position <- grep("date", str_item_tags, ignore.case = TRUE)
        # Obtiene la fecha en formato dd mmm yyyy (12 Jul 2019) y la pasa a formato Y-m-d (2019-07-12)
        #item_date_get <- list(as.character(anytime(ifelse(nchar(substr((xml_find_all(tree_item, ".//pubDate") %>% xml_text()), 6, 16) %>% str_trim()) == 10,
        #                                                  paste("0", substr((xml_find_all(tree_item, ".//pubDate") %>% xml_text()), 6, 16) %>% str_trim(),
        #                                                        sep=""), substr((xml_find_all(tree_item, ".//pubDate") %>% xml_text()), 6, 16) %>% str_trim()))))

        #item_date_get <- list(format(anytime(substr((xml_find_all(tree_item, ".//pubDate") %>% xml_text()), 6, 16) %>% str_trim()), "%Y-%m-%d"))

        item_date_get <- list(substr(anytime(xml_find_all(tree_item, str_item_tags[grep("pubdate",tolower(str_item_tags))]) %>% xml_text()),1,19))

        # Identificaci?n posici?n noticias actuales
        v_item_date_get <- unlist(item_date_get)
        pos_noticias_actuales <- grep(anydate(Sys.Date()), v_item_date_get, ignore.case = TRUE)

        #Comprueba si no hay noticas con fecha actual para no modificar el ?rbol de items
        if(!any(grepl(anydate(Sys.Date()), v_item_date_get, ignore.case = TRUE))){

          #Parámetros envío JSON a plataforma
          TB_token <- "prueba_noticias_kepa"
          TB_url   <- paste("http://88.99.184.239:8080/api/v1/",TB_token,"/telemetry",sep="")

          ts <- format(as.numeric(anytime(Sys.time()))*1000,scientific = F)
          json_noticias <- paste('{"AVISO": "NO HAY NOTICIAS PARA LA FECHA DE HOY PARA: ',url,'"}')

          print(json_noticias)

          #Formato json con modificación de timestamp de thingsboard.
          json_envio_plataforma <- paste('{"ts":',ts,', "values":',json_noticias,"}",sep="")

          #Envío JSON a plataforma
          POST(url=TB_url,body=json_envio_plataforma)

          return(0)
          tree_item <- tree_item[pos_noticias_actuales]
        }

        #item_date_get <- list(v_item_date_get[pos_noticias_actuales])
      }


      # Llenado de list_item_tags con datos de los tags items
      for(i in 1:length(str_item_tags)){
        list_item_tags[[i]] <- xml_find_all(tree_item, str_item_tags[i]) %>% xml_text()
      }



      ########################################
      # MODIFICACI?N FORMTO FECHAS ITEM

      # Eliminiaci?n de la lista item
      list_item_tags <- list_item_tags[- grep("date", str_item_tags, ignore.case = TRUE)]

      # Inserci?n fecha modificada en misma posici?n item
      list_item_tags <- append(list_item_tags, item_date_get, (item_date_position - 1))
      list_item_tags[list_item_tags == ""] <- NA_character_



      ########################################
      # ADECUACI?N DATA FRAME item_tags

      if(any(grepl("category",tolower(str_item_tags)))){
        # Llenado de list_category_tags con datos de las categor?as por cada item
        for(i in 1:length(tree_item)){
          list_category_tags[[i]] <- c(xml_find_all(tree_item[[i]], str_item_tags[grep("category",tolower(str_item_tags))]) %>% xml_text())
        }
      }

      # Data frame item_tags
      # Con ldply se aplica la funci?n rbind (situa en filas los datos de la lista devolviendo una matriz) a list_item_tags
      df_item <- as.data.frame(t(ldply(list_item_tags, rbind)))
      setnames(df_item, old=colnames(df_item), new=name_item_tags)

      if(any(grepl("category",tolower(str_item_tags)))){
        # Elimina columna item_category
        delete <- match(name_item_tags[grep("category",tolower(name_item_tags))], colnames(df_item))
        if(!is.na(delete)){
          df_item <- df_item[,-delete]
        }
      }

      # Eliminaci?n columnas item != a item_tags_req
      df_item_ident <- list()
      col_num <- c()
      pos_manetener <- c()
      # Guarda en una lista los tags requeridos y elimina del df_item_final los no necesarios
      for(i in 1:min(length(item_tags_req), length(colnames(df_item)))){
        df_item_ident[[i]] <- grep(item_tags_req[[i]], colnames(df_item), ignore.case = TRUE, value = TRUE)
        if(any(grepl(item_tags_req[[i]], colnames(df_item), ignore.case = TRUE))){
          pos_manetener <- c(pos_manetener, which(grepl(item_tags_req[[i]], colnames(df_item), ignore.case = TRUE)))
        }
      }
      df_item <- df_item[,pos_manetener]


      # Elimina filas vacias
      df_item <- remove_empty(df_item, which = "rows")

      if(any(grepl("category",tolower(str_item_tags)))){
        # Reemplazo de valores "character(0)" a NA
        list_category_tags[lengths(list_category_tags) == 0] <- NA_character_

        # Generaci?n data frame de 1 columna donde cada fila es un vector de los valores de category por item
        #col_category <- as.data.frame(t(do.call("rbind",list_category_tags)),stringsAsFactors = FALSE)
        #col_category <- vapply(col_category, paste, collapse = ", ", character(1L))


        # Nueva columna item_catgeory
        #df_item$item_categories <- col_category
        df_item$item_categories <- vapply(list_category_tags, paste, collapse = ", ", character(1L))
        # Convierte la columna col_category de tipo lista a tipo vector de cahracter
      }


      #################################################

      # Llenado de list_feed_tags con datos de los tags feed
      for(i in 1:length(str_feed_tags)){
        list_feed_tags[[i]] <- rep(xml_find_first(xml_doc, str_feed_tags[i]) %>% xml_text(), nrow(df_item))
      }

      #################################################
      # MODIFICACI?N FORMATO FECHAS FEED

      if(any(grepl("date", str_feed_tags, ignore.case = TRUE))){
        feed_date_get <- list()
        # Identificaci?n y extraci?n de fechas para modificar formato feed
        feed_date_ident <- grep("date", str_feed_tags, ignore.case = TRUE, value = TRUE)
        feed_date_position <- grep("date", str_feed_tags, ignore.case = TRUE)
        for(i in 1:length(feed_date_ident)){
          feed_date_get[[i]] <- rep(sub("CEST","",anytime(xml_find_first(xml_doc, feed_date_ident[[i]]) %>% xml_text())), nrow(df_item))
        }

        # Posici?n noticias de fecha actual
        pos_noticias_actuales <- grep(anydate(Sys.time()), feed_date_get, ignore.case = TRUE)

        # Eliminiaci?n de la lista feed
        list_feed_tags <- list_feed_tags[- grep("date", str_feed_tags, ignore.case = TRUE)]

        # Inserci?n fecha modificada en misma posici?n feed
        list_feed_tags <- append(list_feed_tags, feed_date_get, (feed_date_position - 1))
      }

      #################################################
      # ADECUACI?N DATA FRAME feed_tags

      # Data frame feed_tags
      df_feed <- as.data.frame(t(ldply(list_feed_tags, rbind)),stringsAsFactors = FALSE)
      setnames(df_feed, old=colnames(df_feed), new=name_feed_tags)

      ########################################################################
      # Resultado

      #Data frame resultado
      df_final <- cbind(df_feed, df_item)

    },

    # En caso de error se utiliza la funci?n tydyfeed()
    error = function(e){
      df_final <- tidyfeed(url)
    }
  )

  #####################################################
  # FILTRADO COLUMNAS DATA FRAME

  # Posici?n tags feed e item
  pos_df_feed_final <- grep("feed", colnames(df_final), ignore.case = TRUE)
  pos_df_item_final <- grep("item", colnames(df_final), ignore.case = TRUE)

  # Obtenci?n tags feed e item
  df_feed_final <- df_final[,1:max(pos_df_feed_final)]
  df_item_final <- df_final[,(max(pos_df_feed_final)+1):max(pos_df_item_final)]

  # # Eliminaci?n columnas feed != a feed_tags_req
  df_feed_ident <- list()
  col_num <- c()
  pos_manetener <- c()
  # Guarda en una lista los tags requeridos y elimina del df_feed_final los no necesarios
  for(i in 1:min(length(feed_tags_req), length(colnames(df_feed_final)))){
    df_feed_ident[[i]] <- grep(feed_tags_req[[i]], colnames(df_feed_final), ignore.case = TRUE, value = TRUE)
    if(any(grepl(feed_tags_req[[i]], colnames(df_feed_final), ignore.case = TRUE))){
      pos_manetener <- c(pos_manetener, which(grepl(feed_tags_req[[i]], colnames(df_feed_final), ignore.case = TRUE)))
    }
  }
  df_feed_final <- df_feed_final[,pos_manetener]



  # Cambia los nombres de las columnas que tengan la palabra date a feed_pubDate
  df_feed_ident <- unlist(df_feed_ident)
  if(any(grepl("date", df_feed_ident, ignore.case = TRUE))){
    str_a_cambiar <- grep("date", df_feed_ident, ignore.case = TRUE, value = TRUE)
    for(i in 1:length(str_a_cambiar)){
      df_feed_ident <- gsub(str_a_cambiar[i], "feed_pubDate", df_feed_ident)
    }
    setnames(df_feed_final, old=colnames(df_feed_final), new=df_feed_ident)
  }

  # Nuevo df_feed_final con columnas requeridas
  df_feed_final <- df_feed_final[,df_feed_ident[nchar(df_feed_ident)>5]]

  # # Eliminaci?n columnas item != a item_tags_req
  df_item_ident <- list()
  col_num <- c()
  pos_manetener <- c()
  # Guarda en una lista los tags requeridos y elimina del df_item_final los no necesarios
  for(i in 1:min(length(item_tags_req), length(colnames(df_item_final)))){
    df_item_ident[[i]] <- grep(item_tags_req[[i]], colnames(df_item_final), ignore.case = TRUE, value = TRUE)
    if(any(grepl(item_tags_req[[i]], colnames(df_item_final), ignore.case = TRUE))){
      pos_manetener <- c(pos_manetener, which(grepl(item_tags_req[[i]], colnames(df_item_final), ignore.case = TRUE)))
    }
  }
  df_item_final <- df_item_final[,pos_manetener]

  # Cambia los nombres de las columnas que tengan la palabra date a item_pubDate
  df_item_ident <- unlist(df_item_ident)
  if(any(grepl("date", df_item_ident, ignore.case = TRUE))){
    str_a_cambiar <- grep("date", df_item_ident, ignore.case = TRUE, value = TRUE)
    for(i in 1:length(str_a_cambiar)){
      df_item_ident <- str_replace(df_item_ident, str_a_cambiar[i], "item_pubdate")
    }
    setnames(df_item_final, old=colnames(df_item_final), new=df_item_ident)
  }

  # Nuevo df_item_final con columnas requeridas
  df_item_final <- df_item_final[,df_item_ident[nchar(df_item_ident)>5]]


  ######################################################################
  # RESULTADO FINAL

  # Uni?n df_feed_final y df_item_final
  df_final <- cbind(df_feed_final, df_item_final)
  #Eliminar filas y columnas duplicadas
  df_final <- df_final[!duplicated(as.list(df_final))]


  #EXTRACCIÓN NOTICIAS FECHA ANTERIOR
  #Extracci?n posici?n filas con fecha anterior en data frame noticias
  posicion_filas_fecha_actual <- grep(anydate(Sys.Date()),unlist(df_final[grep("item_pubdate",tolower(names(df_final)))]))
  #Creaci?n de data frame con noticas publicadas en fecha anterior
  data_frame_noticias_fecha_actual <- df_final[posicion_filas_fecha_actual,]



  nombres_viejos <- names(data_frame_noticias_fecha_actual)

  nombres_nuevos <- nombres_viejos %>%
    gsub("feed", "Periódico", .) %>%
    gsub("_", " ", .) %>%
    gsub("item", "Noticia", .) %>%
    gsub("title", "Título", .) %>%
    gsub("category", "Categoría", .) %>%
    gsub("categories", "Categorías", .) %>%
    gsub("description", "Descripción", .) %>%
    gsub("pubDate", "Fecha publicación", .) %>%
    gsub("pubdate", "Fecha publicación", .)


  #Cambio de nombres a data frame noticias fecha actual
  data_frame_noticias_fecha_actual <- setnames(data_frame_noticias_fecha_actual, nombres_viejos, nombres_nuevos)


  json_noticias_return <- toJSON(data_frame_noticias_fecha_actual,pretty=T)

  #Parámetros envío JSON a plataforma
  TB_token <- "prueba_noticias_kepa"
  TB_url   <- paste("http://88.99.184.239:8080/api/v1/",TB_token,"/telemetry",sep="")

  for(i in 1:nrow(data_frame_noticias_fecha_actual)){
    #Extracción timestamp en formato unix de item_pubDate
    ts <- format(as.numeric(anytime(as.vector(data_frame_noticias_fecha_actual$`Noticia Fecha publicación`[i])))*1000,scientific = F)

    #Creación de JSON noticias y eliminación de ][ para cumplir con el formato json con modificación de timestamp de thingsboard.
    json_noticias <- toJSON(data_frame_noticias_fecha_actual[i,],pretty=T)
    json_noticias <- sub("[[]","",json_noticias)
    json_noticias <- sub("[]]","",json_noticias)

    #Formato json con modificación de timestamp de thingsboard.
    json_envio_plataforma <- paste('{"ts":',ts,', "values":',json_noticias,"}",sep="")

    #Envío JSON a plataforma
    POST(url=TB_url,body=json_envio_plataforma)

    Sys.sleep(5)
  }

  return(json_noticias_return)
}

