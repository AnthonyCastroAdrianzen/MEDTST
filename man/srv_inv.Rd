\name{srv_inv}
\alias{srv_inv}
\title{Generar los inputs para el nivel de servicio e inventario para distintos modelos y estimados}
\description{
Esta función calcula los niveles de servicio e inventario de los modelos en sus distintos momentos, así como otros estimados alternos.}
\usage{
srv_inv(base_testeo, models=c("LIM_INF", "Q_MOD_30", "Q_MOD_50", "Q_MOD_70","LIM_SUP","Q_BESTY","Q_RG3","Q_MKT","Q_FALT")
}
\arguments{
  \item{base_testeo}{la base de datos para el análisis, constituida por "n" columnas donde tienen que haber 2 específicas:}
    \itemize{
      \item CAMP_CODI_VENT: Llave de la campaña y el código de venta (ej. 201718_23453..201909_245243)
      \item REAL: Cantidad pedida de la referencia, medida en unidades.
      }
  \item{models}{Son los percentiles que ingresarán al cálculo de los imputs para generar el nivel de servicio e inventario La función utiliza por defecto el LIM_INF, Q_30, Q_50, Q_70, LIM_SUP,
  MKT, RG3 y FALT que es para el cálculo del Real. Pero se pueden espeficar los que se deseen}}

\examples{
## Base de datos generada
BASE01<- data.frame(PAIS= c(rep("PER",50), rep("COL",50)),
                    CAMP_CODI_VENT=c(paste(rep(c(202001:202010),10),c(1234:1333),sep="_")),
                    CAMP= rep(c(202001:202010),10),
                    DESCRIPCION= paste("SKU", c(1234:1333), sep="_"),
                    CODI_VENT= c(1234:1333),
                    MODELO= "PRENAC",
                    Q_REAL= 1000+(1:100)*50+ rnorm(100,400,200),
                    Q_RG3= 1000+(1:100)*50+ rnorm(100,300,300),
                    Q_MKT= 1000+(1:100)*50+ rnorm(100,300,400),
                    LIM_INF= 1000+(1:100)*50+ rnorm(100,50,10),
                    Q_30= 1000+(1:100)*50+ rnorm(100,100,20),
                    Q_50= 1000+(1:100)*50+ rnorm(100,300,20),
                    Q_70= 1000+(1:100)*50+ rnorm(100,500,20),
                    LIM_SUP= 1000+(1:100)*50+ rnorm(100,600,10),
                    Q_FALT= ifelse(rnorm(100,200,100)<0, 0,rnorm(100,200,100))
)


# Es necesario que se especifique todo como "Q_" seguido del modelo o de RG3, MKT, o REAL:

# Primero generamos el Q_BESTY, si no lo especificamos nos arrojará el más cercano al real entre Q_30, Q_50 y Q_70
BASE02<- gen_q_besty(BASE01)

# Ahora hacemos el cálculo del servicio e inventario

BASE03<- srv_inv(BASE02)

# Podemos hacer el cálculo solo de algunos percentiles, pero siempre con el Q_FATL para el cálculo del real
BASE04<- srv_inv(BASE02, models = c("Q_30", "Q_50","Q_70", "Q_FALT"))
}
\keyword{accuracy}
