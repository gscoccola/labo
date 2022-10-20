#Modificado para correr en la computadora local

# son varios archivos, subirlos INTELIGENTEMENTE a Kaggle

#limpio la memoria
#rm( list=ls() )  #remove all objects
#gc()             #garbage collection

require("data.table")
require("lightgbm")
require("ggplot2")

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento  <- "originalrank"

PARAM$input$dataset       <- "./competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202101 )
PARAM$input$future        <- c( 202103 )

#Estan puestos los de la BO, comentados los de la BO sin varaibles adicionales
PARAM$finalmodel$max_bin           <-     31
PARAM$finalmodel$learning_rate     <-      0.0116443534251682   #0.0437907293266036
PARAM$finalmodel$num_iterations    <-    354  #164
PARAM$finalmodel$num_leaves        <-   376  #1024
PARAM$finalmodel$min_data_in_leaf  <-   535  #1239
PARAM$finalmodel$feature_fraction  <-      0.99762223135447  #0.324462828627375
PARAM$finalmodel$semilla           <- 348431
#100153, 671017, 202061, 348431, 448139
PARAM$finalmodel$lambda_l1           <-     0
PARAM$finalmodel$lambda_l2           <-     0


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa


#cargo el dataset donde voy a entrenar
dataset  <- fread(PARAM$input$dataset, stringsAsFactors= TRUE)

dataset[,c("mcuentas_saldo_noise", "mcaja_ahorro_noise", "mpasivos_margen_noise", "mpayroll_noise")] <- 
  dataset[,c("mcuentas_saldo", "mcaja_ahorro", "mpasivos_margen", "mpayroll")]

dataset$mcuentas_saldo_noise <- jitter(dataset$mcuentas_saldo, amount=30)
dataset$mcaja_ahorro_noise <- jitter(dataset$mcaja_ahorro, amount=30)
dataset$mpasivos_margen_noise <- jitter(dataset$mpasivos_margen, amount=30)
dataset$mpayroll_noise <- jitter(dataset$mpayroll, amount=30)

#--------------------------------------

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#--------------------------------------

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )

#--------------------------------------



#establezco donde entreno
dataset[ , train  := 0L ]
dataset[ foto_mes %in% PARAM$input$training, train  := 1L ]

#--------------------------------------
#creo las carpetas donde van los resultados
#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0("./exp/", PARAM$experimento, "/" ), showWarnings = FALSE )
setwd( "C:\\Users\\PC\\Desktop\\DMEyF\\Experimento Colectivo" )   #Establezco el Working Directory DEL EXPERIMENTO



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ train==1L, campos_buenos, with=FALSE]),
                        label= dataset[ train==1L, clase01] )

#genero el modelo
#estos hiperparametros  salieron de una laaarga Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=          "binary",
                                   max_bin=            PARAM$finalmodel$max_bin,
                                   learning_rate=      PARAM$finalmodel$learning_rate,
                                   num_iterations=     PARAM$finalmodel$num_iterations,
                                   num_leaves=         PARAM$finalmodel$num_leaves,
                                   min_data_in_leaf=   PARAM$finalmodel$min_data_in_leaf,
                                   feature_fraction=   PARAM$finalmodel$feature_fraction,
                                   seed=               PARAM$finalmodel$semilla
                      )
)

#--------------------------------------
#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "impo.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

#--------------------------------------


#aplico el modelo a los datos sin clase
dapply  <- dataset[ foto_mes== PARAM$input$future ]

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) ) 



#genero la tabla de entrega
tb_entrega  <-  dapply[ , list( numero_de_cliente, foto_mes, clase_ternaria ) ]
tb_entrega[  , prob := prediccion ]

#grabo las probabilidad del modelo
fwrite( tb_entrega,
        file= "prediccion.txt",
        sep= "\t" )

#ordeno por probabilidad descendente
setorder( tb_entrega, -prob )

ganancias <- list()
#genero archivos con los  "envios" mejores
#deben subirse "inteligentemente" a Kaggle para no malgastar submits
cortes <- seq( 5000, 15000, by=250 )
for( envios  in  cortes )
{
  tb_entrega[  , Predicted := 0L ]
  tb_entrega[ 1:envios, Predicted := 1L ]
  ganancia <- ((-2000)*length(tb_entrega[Predicted==1 & clase_ternaria!="BAJA+2", Predicted])
               + 78000*length(tb_entrega[Predicted==1 & clase_ternaria=="BAJA+2", Predicted]))/1000000
  
  ganancias <- append(ganancias, ganancia)
  
  
  
}

envios1=as.list(cortes)


resultados <- data.frame(envios=unlist(envios1), ganancias=unlist(ganancias))


intervalo_max <- resultados[as.integer(which.max(resultados$ganancias)-5):as.integer(which.max(resultados$ganancias)+5),]

ganancia_aprox <- round(mean(intervalo_max$ganancias), 4)



#mean(c(24.8091, 24.9036, 25.2764, 24.6527, 25.0273))
print(ganancia_aprox)
r5 <- resultados


