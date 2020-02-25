#' @title Escrape la RSS de uno o más periódicos.
#'
#' @description Escrape la RSS de uno o más periódicos, devuelve el json con las noticias (título, fecha publicación, descripción...) y lo envía a la plataforma Smart City.
#' En caso de introducir como parámetro más de una URL, se debe introducir separadas por una coma (url1,url2,url3,...).
#'
#' @param links
#'
#' @return json
#'
#' @examples  scrap_RSS('https://www.deia.eus/rss/economia.xml')
#'
#' @import stringr
#' jsonlite
#' xml2
#'
#' @export

N_scrap_RSS <- function(links){

  #Desconcatenación links
  links_vector <- str_split(links,",")

  #Bucle para ejecutar la función N veces
  for(i in 1:length(links_vector[[1]])){
    #Llamada a la función scrap_RSS
    scrap_RSS(links_vector[[1]][i])
  }

  respuesta <- paste("Número de periódicos leídos: ", i, sep = "")
  respuesta_json <- toJSON(respuesta, pretty = T)

  return(respuesta_json)
}
