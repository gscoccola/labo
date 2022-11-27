rm( list=ls() )  #remove all objects
gc() 

require("data.table")

temp = list.files(pattern="*resultados.csv")
for (i in 1:length(temp)) assign(paste0("df_", i), read.csv(temp[i]))


df <- df_1[, c("numero_de_cliente", "prob", "rank")]


for (i in 2:length(temp)){
  nam <- paste0("df_", i)
  df[, c("prob", "rank")] <- df[,c("prob", "rank")] + get(nam)[,c("prob", "rank")]
  
  
}

df[, c("prob", "rank")] <- df[,c("prob", "rank")]/20

tb_entrega <- as.data.table(copy(df))
setorder( tb_entrega, rank )

#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 4500, 5750, by=250 )
for( envios  in  cortes ){
  tb_entrega[  , Predicted := 0 ]
  tb_entrega[ 1:envios, Predicted := 1 ]
  
  fwrite( tb_entrega[ , list(numero_de_cliente, Predicted)], 
          file= paste0(  "prediccion_rank_", envios, ".csv" ),
          sep= "," )
}






