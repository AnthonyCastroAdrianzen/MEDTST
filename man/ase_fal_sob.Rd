\name{ase_fal_sob}
\alias{ase_fal_sob}
\title{La función adiciona a la data imput el rango de asertividad y calcula el sobrante y faltante para cada obsevación}
\description{La función permite evaluar e lrango de asertividad, faltante, sobrante de los modelos que se le especifiquen}
\usage{
srv_inv(base_testeo, models=c("LIM_INF", "Q_MOD_30", "Q_MOD_50", "Q_MOD_70","LIM_SUP","Q_BESTY","Q_RG3","Q_MKT","Q_FALT")
}
\arguments{
  \item{base}{Base de datos con los valores reales y predichos para cada observacion}
    \itemize{
      \item Q_REAL: Cantidad pedida de la referencia, medida en unidades.
      \item Q_30: Cantidad estimada del modelo, segun percentil 30.
      \item Q_50: Cantidad estimada del modelo, segun percentil 50.
      \item Q_70: Cantidad estimada del modelo, segun percentil 70.
      }
  \item{p_model}{ Vector que contiene los nombres de las columnas que almacenan las predicciones de los modelos a evaluar}}

\examples{
## Base de datos generada
DATA = data.frame( DESCRIPCION = c("CAMISETA ANTHONY","VESTIDO JENNY", "POLERA JOSELYN",
                                   "PANTALON PAUL", "CAMISA PAVEL","POLO VLADIMIR", "BLUSA LISBY"),
                   Q_REAL = c(200,100,20,15,350,180,400),
                   Q_30 = c(100,80,2,12,200,140,500),
                   Q_50 = c(150,90,5,28,300,180,600),
                   Q_70 = c(180,102,10,32,400,200,850)
)
p_model= c("Q_30","Q_50","Q_70")

RESULTADO= ase_fal_sob(DATA,p_model,"Q_REAL")

head(RESULTADO)

}
\keyword{accuracy}
