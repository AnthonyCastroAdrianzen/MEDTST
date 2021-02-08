\name{gen_q_besty}
\alias{gen_q_besty}
\title{Generar la mejor elección por referencia}
\description{
Esta función escoge la menor distancia absoluta entre el valor real y los distintos momentos del modelo, que por defecto son los persentiles 30, 50 y 70.
}
\usage{
gen_q_besty(df_base, models=c("Q_30", "Q_50", "Q_70")
}
\arguments{
  \item{df_base}{la base de datos para el análisis, constituida por "n" columnas donde tienen que haber 2 específicas:}
    \itemize{
      \item CAMP_CODI_VENT: Llave de la campaña y el código de venta (ej. 201718_23453..201909_245243)
      \item REAL: Cantidad pedida de la referencia, medida en unidades.
      }
  \item{models}{Son los modelos que se desea poner a prueba para la elección del Q_Besty. Por defecto es el percentil 30, 50 y 70, pero se pueden escoger más. Si se quiere mantener el defecto, no se incluye nada}}

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
                    Q_30= 1000+(1:100)*50+ rnorm(100,100,20),
                    Q_50= 1000+(1:100)*50+ rnorm(100,300,20),
                    Q_70= 1000+(1:100)*50+ rnorm(100,500,20),
                    Q_90= 1000+(1:100)*50+ rnorm(100,600,10)
)

# Es necesario que se especifique todo como "Q_" seguido del modelo o de RG3, MKT, o REAL:

# Primero corremos sin identificar los percentiles que necesito, la función me generará el Besty solamente evaluando el Q_30, Q_50, Q_70
BASE02<- gen_q_besty(BASE01)

# También le podemos especificar algun percentil más, por ejemplo que el Besty lo halle también hasta el Q_90.

BASE04<- gen_q_besty(BASE01,models = c("Q_30", "Q_50", "Q_70", "Q_90"))

}
\keyword{accuracy}
