\name{soilandfauna}
\alias{soilandfauna}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Soil physico chemical properties and abundance of fauna
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~
Soil physico-chemical properties and soil fauna of three sites in the Pampean plain 
(Buenos Aires, Argentina), used to test the Ecoindicators package.  
Samples were taken on three different sites regarding the history use: 
A naturalized grassland (NG), a grazing field that changed to agriculture 
two years before the start of the samplings (CG), 
and a site of continuous intensive agriculture for at least 40 years (AG)
}
\usage{data("soilandfauna")}
\format{
  soilandfauna is a data frame with measurements of  fifteen  physical  and 
  chemical  parameters  and  the  abundance  of  forty-three soil fauna taxonomic units.
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Rosana Sandler, 2020, “Indicadores de sustentabilidad del suelo basados en la estructura y funcionamiento de la fauna edáfica.” Ph.D. thesis
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(soilandfauna)

soil <- soilandfauna[,3:17]

fauna <- soilandfauna[,18:60]
}
\keyword{datasets}
