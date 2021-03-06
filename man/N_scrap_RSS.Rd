% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NscrapRSS_func.R
\name{N_scrap_RSS}
\alias{N_scrap_RSS}
\title{Escrape la RSS de uno o más periódicos.}
\usage{
N_scrap_RSS(links)
}
\arguments{
\item{links}{}
}
\value{
json
}
\description{
Escrape la RSS de uno o más periódicos, devuelve el json con las noticias (título, fecha publicación, descripción...) y lo envía a la plataforma Smart City.
En caso de introducir como parámetro más de una URL, se debe introducir separadas por una coma (url1,url2,url3,...).
}
\examples{
 scrap_RSS('https://www.deia.eus/rss/economia.xml')

}
