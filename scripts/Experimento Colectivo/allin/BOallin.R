# source( "~/labo/src/lightgbm/z723_lightgbm_binaria_BO.r" )
# Este script esta pensado para correr en Google Cloud
#   8 vCPU
#  32 GB memoria RAM
# 256 GB espacio en disco

# se entrena con POS =  { BAJA+1, BAJA+2 }
# Optimizacion Bayesiana de hiperparametros de  lightgbm, con el metodo TRADICIONAL de los hiperparametros originales de lightgbm
# 5-fold cross validation
# la probabilidad de corte es un hiperparametro

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})



#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
  makeNumericParam("learning_rate",    lower=    0.005, upper=    0.3),
  makeNumericParam("feature_fraction", lower=    0.2  , upper=    1.0),
  makeIntegerParam("min_data_in_leaf", lower=    0L   , upper=  8000L),
  makeIntegerParam("num_leaves",       lower=   16L   , upper=  1024L),
  makeIntegerParam("envios",           lower= 5000L   , upper= 15000L)
)

#defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM  <- list()

PARAM$experimento  <- "feBADallIn"

PARAM$input$dataset       <- "./datasets/competencia2_2022.csv.gz"
PARAM$input$training      <- c( 202101 )

PARAM$trainingstrategy$undersampling  <-  1.0   # un undersampling de 0.1  toma solo el 10% de los CONTINUA
PARAM$trainingstrategy$semilla_azar   <- 202061  #Aqui poner la propia semilla

PARAM$hyperparametertuning$iteraciones <- 100
PARAM$hyperparametertuning$xval_folds  <- 5
PARAM$hyperparametertuning$POS_ganancia  <- 78000
PARAM$hyperparametertuning$NEG_ganancia  <- -2000

PARAM$hyperparametertuning$semilla_azar  <- 671017  #Aqui poner la propia semilla, PUEDE ser distinta a la de trainingstrategy

#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./exp/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#esta funcion calcula internamente la ganancia de la prediccion probs

fganancia_logistic_lightgbm  <- function( probs, datos) 
{
  vpesos   <- get_field(datos, "weight")
  
  #vector de ganancias
  vgan  <- ifelse( vpesos == 1.0000002, PARAM$hyperparametertuning$POS_ganancia, 
                   ifelse( vpesos == 1.0000001, PARAM$hyperparametertuning$NEG_ganancia, 
                           PARAM$hyperparametertuning$NEG_ganancia / PARAM$trainingstrategy$undersampling ) )
  
  tbl  <- as.data.table( list( "vprobs" = probs, "vgan" = vgan ) )
  setorder( tbl,  -vprobs )
  ganancia <- tbl[ 1:GLOBAL_envios, sum( vgan ) ]
  
  return( list( "name"= "ganancia", 
                "value"=  ganancia,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  gc()  #libero memoria
  
  #llevo el registro de la iteracion por la que voy
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1
  
  #para usar en fganancia_logistic_lightgbm 
  GLOBAL_envios <<- as.integer(x$envios/PARAM$hyperparametertuning$xval_folds)   #asigno la variable global
  
  kfolds  <- PARAM$hyperparametertuning$xval_folds   # cantidad de folds para cross validation
  
  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          verbosity= -100,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE,   #para que los alumnos no se atemoricen con tantos warning
                          seed= PARAM$hyperparametertuning$semilla_azar
  )
  
  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )
  
  param_completo  <- c( param_basicos, param_variable, x )
  
  set.seed( PARAM$hyperparametertuning$semilla_azar )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
  )
  
  #obtengo la ganancia
  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]
  
  ganancia_normalizada  <-  ganancia* kfolds     #normailizo la ganancia
  
  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL     #elimino de la lista el componente  "early_stopping_rounds"
  
  #Voy registrando la importancia de variables
  if( ganancia_normalizada >  GLOBAL_gananciamax )
  {
    GLOBAL_gananciamax  <<- ganancia_normalizada
    modelo  <- lgb.train( data= dtrain,
                          param= param_completo,
                          verbose= -100
    )
    
    tb_importancia  <- as.data.table( lgb.importance(modelo ) )
    archivo_importancia  <- paste0( "impo_", GLOBAL_iteracion,".txt")
    fwrite( tb_importancia,
            file= archivo_importancia,
            sep= "\t" )
  }
  
  
  #el lenguaje R permite asignarle ATRIBUTOS a cualquier variable
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra
  
  #logueo 
  xx  <- param_completo
  xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
  xx$iteracion <- GLOBAL_iteracion
  loguear( xx, arch= klog )
  
  return( ganancia_normalizada )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

#Aqui se debe poner la carpeta de la computadora local
setwd("~/buckets/b1/")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread( PARAM$input$dataset )

#Único feature Engineering

#agrego 30 canaritos
for( i in 1:100 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dataset[,c("mcuentas_saldo_noise", "mcaja_ahorro_noise", "mpasivos_margen_noise", "mpayroll_noise")] <- 
  dataset[,c("mcuentas_saldo", "mcaja_ahorro", "mpasivos_margen", "mpayroll")]

dataset$mcuentas_saldo_noise <- jitter(dataset$mcuentas_saldo, amount=30)
dataset$mcaja_ahorro_noise <- jitter(dataset$mcaja_ahorro, amount=30)
dataset$mpasivos_margen_noise <- jitter(dataset$mpasivos_margen, amount=30)
dataset$mpayroll_noise <- jitter(dataset$mpayroll, amount=30)

dataset[,c("mcuentas_saldo_noise2", "mcaja_ahorro_noise2", "mpasivos_margen_noise2", "mpayroll_noise2")] <- 
  dataset[,c("mcuentas_saldo", "mcaja_ahorro", "mpasivos_margen", "mpayroll")]

dataset$mcuentas_saldo_noise2 <- jitter(dataset$mcuentas_saldo, amount=60)
dataset$mcaja_ahorro_noise2 <- jitter(dataset$mcaja_ahorro, amount=60)
dataset$mpasivos_margen_noise2 <- jitter(dataset$mpasivos_margen, amount=60)
dataset$mpayroll_noise2 <- jitter(dataset$mpayroll, amount=60)

dataset[,c("ctrx_quarter_rank", "mcuentas_saldo_rank", "mcaja_ahorro_rank")] <- setDT(dataset[,c("ctrx_quarter", "mcuentas_saldo", "mcaja_ahorro")])[, lapply(.SD, frankv, order=-1L,
                                                                                                                                                              ties.method="random")]

#creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

#variable extraida de una tesis de maestria de Irlanda
dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

#se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
#varias formas de combinar Visa_status y Master_status
dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
dataset[ , mv_status02       := Master_status +  Visa_status ]
dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                        ifelse( is.na(Master_status), 10, Master_status), 
                                        Visa_status)  ]

dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                        ifelse( is.na(Visa_status), 10, Visa_status), 
                                        Master_status)  ]


#combino MasterCard y Visa
dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

#a partir de aqui juego con la suma de Mastercard y Visa
dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

#Aqui debe usted agregar sus propias nuevas variables

#valvula de seguridad para evitar valores infinitos
#paso los infinitos a NULOS
infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
infinitos_qty  <- sum( unlist( infinitos) )
if( infinitos_qty > 0 )
{
  cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
  dataset[mapply(is.infinite, dataset)] <- NA
}


#valvula de seguridad para evitar valores NaN  que es 0/0
#paso los NaN a 0 , decision polemica si las hay
#se invita a asignar un valor razonable segun la semantica del campo creado
nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
nans_qty  <- sum( unlist( nans) )
if( nans_qty > 0 )
{
  cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
  cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
  dataset[mapply(is.nan, dataset)] <- 0
}



#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd( paste0( "./exp/", PARAM$experimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO

#en estos archivos quedan los resultados
kbayesiana  <- paste0( PARAM$experimento, ".RDATA" )
klog        <- paste0( PARAM$experimento, ".txt" )


GLOBAL_iteracion  <- 0   #inicializo la variable global
GLOBAL_gananciamax <- -1 #inicializo la variable global

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
  tabla_log  <- fread( klog )
  GLOBAL_iteracion  <- nrow( tabla_log )
  GLOBAL_gananciamax  <- tabla_log[ , max( ganancia ) ]
}



#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ foto_mes %in% PARAM$input$training, clase01 := ifelse( clase_ternaria=="CONTINUA", 0L, 1L) ]


#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", "azar", "training" ) )

set.seed( PARAM$trainingstrategy$semilla_azar )
dataset[  , azar := runif( nrow( dataset ) ) ]
dataset[  , training := 0L ]
dataset[ foto_mes %in% PARAM$input$training & 
           ( azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c( "BAJA+1", "BAJA+2" ) ),
         training := 1L ]

#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ training == 1L, campos_buenos, with=FALSE]),
                        label= dataset[ training == 1L, clase01 ],
                        weight=  dataset[ training == 1L, ifelse( clase_ternaria=="BAJA+2", 1.0000002, ifelse( clase_ternaria=="BAJA+1",  1.0000001, 1.0) )],
                        free_raw_data= FALSE  )



#Aqui comienza la configuracion de la Bayesian Optimization
funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar, #la funcion que voy a maximizar
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,     #definido al comienzo del programa
  has.simple.signature = FALSE   #paso los parametros en una lista
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= PARAM$hyperparametertuning$iteraciones )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if( !file.exists( kbayesiana ) ) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}


#quit( save="no" )