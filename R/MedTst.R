#' @title Medidas de testeo para Modelos de Regresion en Venta Directa
#'
#' @description Este paquete permitirá ubicar en que rango de asertividad se encuentran los distintos momentos de modelo, encontrara la mejor elección individual y brindara inputs para encontrar los niveles de servicio e inventario.
#'
#' @param base
#' @param p_model
#' @param real_name
#' @param df_base
#' @param models
#' @param base_testeo
#' @param models
#'
#' @return NULL
#'
#' @export
#' @examples

ase_fal_sob = function(base,p_model,real_name){

  # FALTANTE Y SOBRANTE
  base_1 = base[,c(p_model,real_name)]

  faltante_sobrante<- data.frame(FALT_SBR = apply(base_1[,p_model], 2, function(x){-base_1[,real_name]+x}))

  columns_name = paste("FALT_SBR",p_model,sep = "_")

  names(faltante_sobrante) =  columns_name

  # RANGO DE ASERTIVIDAD
  cal_rango = function(Q){

    Q_1 = ifelse(Q<=0.5,"a) 0-50%",
                 ifelse(Q<=0.8,"b) 50-80%" ,
                        ifelse(Q<=1.2,"c) 80-120%" ,
                               ifelse(Q<=1.8,"d) 120-180%" ,
                                      ifelse(Q<=2.5,"e) 180-250%" ,
                                             "f) >250%" )))))


    return(Q_1)


  }


  ratio <- data.frame(ASERT = apply(base_1[,p_model], 2, function(x){base_1[,real_name]/x}))
  rango <- data.frame(RANGO = apply(ratio, 2, function(x){cal_rango(x)}))

  rango_names = paste("RANGO",p_model,sep = "_")
  names(rango) = rango_names

  datos_finales = cbind(faltante_sobrante,rango)

  final = cbind(base,datos_finales)
  return(final)


}



# Librerías a usar:

library(dplyr)

# La función es:

gen_q_besty<- function(df_base, models=c("Q_30", "Q_50", "Q_70")) {


  n_prod_total <- nrow(df_base)
  nomb_col_pre<- colnames(df_base)
  nomb_col<- nomb_col_pre
  #nomb_col[grepl("REAL",nomb_col)]<- "Q_REAL"
  perc<- c("2.5",seq(10,90,by=10),"97.5")
  models_1<- models

  #for (i in 1: length(perc)){
    #i=1
   # nomb_col[grepl(perc[i],nomb_col)]<- paste0("Q_",perc[i])
   # models_1[grepl(perc[i],models_1)]<- paste0("Q_",perc[i])
  #}

  colnames(df_base)<- nomb_col

  df_dlt <- df_base[(complete.cases(df_base)==F | df_base$Q_REAL==0), ]
  df_base <- df_base[(complete.cases(df_base)==F | df_base$Q_REAL==0)==F, ]

  n_prod_compl <- nrow(df_base)

  n_models <- length(models_1)

  df_base_1_pre<- cbind(df_base[,c("CAMP_CODI_VENT","Q_REAL",models_1)])

  df_base_1<- data.frame(df_base_1_pre,
                         COCIENTE= abs(apply(df_base_1_pre[,3:(2+n_models)],2,function(x){(df_base_1_pre$Q_REAL/x)})-1))

  df_base_2<-df_base_1

  names(df_base_2)

  df_base_3<- data.frame(df_base_2,
                         COL_BESTY= apply(df_base_2[,(3+n_models):ncol(df_base_2)],1, function(x){which.min(x)}))

  n_besty= df_base_3$COL_BESTY + 2

  for (i in 1:nrow(df_base_3)){
    #i=1
    df_base_3$Q_BESTY[i]<- df_base_3[i,n_besty[i]]
  }

  X_RESULTADO <- list()
  X_RESULTADO[["DF_TOT"]] <- df_base
  X_RESULTADO[["DF_DTL"]] <- df_dlt
  X_RESULTADO[["DF_COC"]] <- df_base_1
  X_RESULTADO[["DF_COC_CER"]] <- df_base_2
  X_RESULTADO[["DF_BESTY"]] <- df_base_3$Q_BESTY
  print(paste("Productos totales ingresados:", nrow(df_base), sep=" "))
  print(paste("Productos incompletos eliminados:", nrow(df_dlt), sep=" "))

  df_base_4<- cbind(df_base, Q_BESTY= df_base_3[,c("Q_BESTY")])
  return(df_base_4)
}



# La función es:
srv_inv<- function(base_testeo, models=c("LIM_INF", "Q_30", "Q_50", "Q_70","LIM_SUP","Q_BESTY","Q_RG3","Q_MKT","Q_FALT")){


  nomb_col_pre<- colnames(base_testeo)
  nomb_col<- nomb_col_pre
  #nomb_col[grepl("REAL",nomb_col)]<- "Q_REAL"
  perc<- c("2.5",seq(10,90,by=10),"97.5", "RG3", "MKT", "FALT")
  models_1<- models

  #for (i in 1: length(perc)){
    #i=1
   # nomb_col[grepl(perc[i],nomb_col)]<- paste0("Q_",perc[i])
    #models_1[grepl(perc[i],models_1)]<- paste0("Q_",perc[i])
  #}

  colnames(base_testeo)<- nomb_col
  drop_falt<- which(models_1 =="Q_FALT")
  models_2<- models_1[-drop_falt]
  base_testeo<- as.data.frame(base_testeo)

  base_testeo_1<- data.frame(base_testeo,
                             SRV= apply(base_testeo[,models_2], 2, function(x){ifelse(base_testeo$Q_REAL>x, x, base_testeo$Q_REAL)}),
                             SRV_REAL= base_testeo$Q_REAL- base_testeo$Q_FALT,
                             INV= apply(base_testeo[,models_2], 2, function(x){ifelse(base_testeo$Q_REAL>x, 0, x-base_testeo$Q_REAL)})
  )

  nomb_cols_1<- paste0("SRV_",models_2)
  nomb_cols_2<- paste0("INV_",models_2)

  nomb_cols_tot<- c(nomb_col_pre, nomb_cols_1,"SRV_REAL",nomb_cols_2)
  colnames(base_testeo_1)<- nomb_cols_tot


  return(base_testeo_1)

}

