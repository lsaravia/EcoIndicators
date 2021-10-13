\name{soilandfauna}
\alias{soilandfauna}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Soil physico chemical properties and abundance of fauna
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Soil physico-chemical properties and soil fauna of three sites in the Pampean plain (Buenos Aires, Argentina). Each sample in this database collects measurements of  fifteen  physical  and  chemical  parameters  and  the  presence  or  absence  of  forty-three  taxonomic units. Samples were taken on three different sites regarding the history use: A naturalized grassland (NG), a grazing field that passed onto agriculture two years beforethe start of the samplings (CG), and a site of continuous intensive agriculture for at least 40 years (AG)
}
\usage{data("soilandfauna")}
\format{
  The format is:
 chr "data"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
Rosana Sandler 2020, Tesis de doctorado
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
