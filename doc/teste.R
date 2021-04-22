# Carrega pacotes------------------------------------------------

# Carrega pacotes


if(!require(PSF)) remotes::install_github("neerajdhanraj/PSF")

pacotes <- c(
  "here",
  "usethis",
  "data.table",
  "HEobs",
  "PSF",
  "tidyverse",
  "lubridate",
  "fs",
  "checkmate",
  "xts",
  "hydroGOF",
  "decomposedPSF",
  "ForecastTB",
  "ModelMetrics"
)
# Carregar os pacotes
easypackages::libraries(pacotes)



# Carrega funções auxiliares-----------------------------------------
source(here('R', 'load-data.R'))
source(here('R', 'utils.R'))


# Carrega dados-----------------------------------------------------
qnat_data <- qnat_dly_ons() %>%
  select(date, qnat, code_stn) %>%
  lhmetools::complete_dates(group = "code_stn")
glimpse(qnat_data)
# View(qnat_data)



# Funções auxiliares para análise do PSF em todos os postos-----------
sel_posto <- function(df,posto){
  # Função para selecionar um posto específico via código
  df %>% 
    dplyr::filter(code_stn == posto) 
}

apply_cmonth <- function(df){
  # Função para filtrar os meses completos dos dados 
  df <- df %>% 
    dplyr::group_by(date = floor_date(date, "month"), code_stn) %>% 
    dplyr::summarise(
      qnat_obs = mean_wise(qnat), 
      valid = nvalid(qnat), 
      N = n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(valid >= 28) %>% 
    select(date,code_stn,qnat_obs)
  return(df)
}

get_cyears <- function(df){
  # Função para pegar os anos completos dos dados
  cyrs <- df %>% 
    group_by(ano = lubridate::year(date)) %>% 
    tally() %>% 
    filter(n == 12) %>% 
    pull(ano)
  return(cyrs)
}

apply_cyears  <- function(df){
  # Função para filtrar os dados com anos completos
  # Usar depois de agrupar os dados
  cyrs <- get_cyears(df)
  df_cyrs <- df %>% 
    filter(lubridate::year(date) %in% cyrs)
  return(df_cyrs)
}


get_traindt <- function(df){
  # Função para pegar os dados de treinamento
  # Padrão: 2 últimos anos de observações são removidos
  cyrs <- get_cyears(df)
  leave_out <- tail(cyrs,n = 2) # retira 2 últ. anos por padrão
  data <- df %>% 
    filter(!lubridate::year(date) %in% leave_out)
  return(data)
}


get_testdt <- function(df){
  # Função para pegar os dados de teste
  # Padrão: 2 últimos anos de observações são usados como teste
  nahead = 24
  inds <- (nrow(df) - nahead + 1):(nrow(df))
  df <- df[inds,] %>% 
    select(date,qnat_obs)
    #df[inds,"qnat_obs"] # df[inds,2]
}

psf_model <- function(df){
  # Função para aplicar o psf, retorna as predições do modelo
  # set.seed(1) #p/ reprodutibilidade
  model <- psf(df[,"qnat_obs"],cycle = 12) #df[,2]
  preds <- predict(model,n.ahead = 24)
  return(preds)
}


ensemble_preds <- function(df){
  # Retorna uma lista com as predições do psf com difs seeds
  list_preds <- list()
  # list_model <- list()
  for(i in 1:5){
    set.seed(i)
    list_preds[[i]] <- psf_model(df)
  }
  return(list_preds)
}



ensemble_models <- function(df){
  # Retorna uma lista com os modelos do psf com difs seeds
  list_model <- list()
  for(i in 1:5){
    set.seed(i)
    model <- psf(df[,"qnat_obs"],cycle = 12)
    list_model[[i]] <- model
  }
  return(list_model)
  
}

ensemble_mpar <- function(df,params){
  # Retorna predições feitas com os valores médios de k e w
  # extraídos usando a função get_mpar
  set.seed(1)
  model <- psf(df[,"qnat_obs"],
               k = params[[1]][[1]],
               w = params[[1]][[2]],
               cycle = 12)
  pred <- predict(model,n.ahead = 24)
  return(pred)
}

get_mpar <- function(modelo){
  # Média dos parâmetros k e w dos modelos
  list_k <- list()
  list_w <- list()
  teste <- for(i in 1:5){
    list_k[[i]] <- modelo[[i]]$k
    list_w[[i]] <- modelo[[i]]$w
  }
  # Usar apenas números inteiros no modelo
  k_mean <-  round(Reduce("+",list_k)/length(list_k),0)
  w_mean <- round(Reduce("+",list_w)/length(list_w),0)
  list_param <- list()
  list_param[[1]] <- c(k_mean,w_mean)
}


get_mpred <- function(lista){
  # Faz a média das predições dos modelos
  pred_mean <- list(
    rowMeans(as.data.frame(lista))
  )
}

# Teste do ensemble para apenas um posto(74)--------------------------------
posto <- 74
qnat_posto74 <- qnat_data %>% 
  sel_posto(.,posto) %>% 
  apply_cmonth() %>% 
  apply_cyears()


test_data74 <- qnat_posto74 %>% 
  get_testdt(.)
train_data74 <- qnat_posto74 %>% 
  get_traindt(.)
  

ensemble <- train_data74 %>% 
  group_by(code_stn) %>% 
  nest()%>% 
  mutate(
    ensemble_model = map(data,ensemble_models), # parâmetros modelos
    ensemble_preds = map(data, ensemble_preds) # preds dos modelos
  )


# Faz a média das predições dos modelos
pred_mean <- list(
  rowMeans(as.data.frame(ensemble$ensemble_preds))
)
# Alternativa é usar a função reduce duas vezes
# Reduce("+",teste3$ensemble_preds) # uma só lista (lista[[i]])

# mean_ens$ensemble_model[[1]][[3]]$k # acessar modelo

# Média dos parâmetros k e w dos modelos
list_k <- list()
list_w <- list()
teste <- for(i in 1:5){
  list_k[[i]] <- ensemble$ensemble_model[[1]][[i]]$k
  list_w[[i]] <- ensemble$ensemble_model[[1]][[i]]$w
}


# Usar apenas números inteiros no modelo
k_mean <-  round(Reduce("+",list_k)/length(list_k),0)
w_mean <- round(Reduce("+",list_w)/length(list_w),0)



psf_param_mean<- function(df){
  # Função para aplicar o psf, retorna as predições do modelo
   set.seed(1) #p/ reprodutibilidade
  model <- psf(df[,"qnat_obs"],
               k = 2,
               w = 1,
               cycle = 12) #df[,2]
  preds <- predict(model,n.ahead = 24)
  return(preds)
}


mean_ens <- ensemble %>% 
  mutate(
    ensemble_mean = pred_mean,
    param_mean = map(data,psf_param_mean)
  ) %>% 
  select(ensemble_mean,param_mean) %>% 
  ungroup() %>% 
  unnest()
  #unnest(cols = c(ensemble_mean, param_mean))

aval74 <- test_data74 %>% 
  mutate(qnat_obs,
         pred_mean = NA,
         param_mean = NA,
         pred_mean = replace(
           pred_mean,
           values = mean_ens$ensemble_mean
         ),
         param_mean = replace(
           param_mean,
           values = mean_ens$param_mean
         )
  )

aval74_ts <- xts(aval74[,-1], order.by = as.Date(aval74[["date"]]))

forecast::autoplot(aval74_ts, facets = FALSE)


aval_metricas <- mean_ens %>% 
  group_by(code_stn) %>% 
  mutate(ensemble_mean,param_mean,
         qnat_obs = aval74$qnat_obs)



metricas <- aval_metricas %>% 
  group_by(code_stn) %>%
  summarise(KGE_param = KGE(sim = param_mean, obs = qnat_obs),
            KGE_mean = KGE(sim = ensemble_mean, obs = qnat_obs)
  ) %>% 
  arrange(-KGE_param)

# Teste do ensemble para 5 postos ----------------------------------

# Seleciona postos específicos e extrai os dados válidos
qnat_postos <- qnat_data %>%
  dplyr::filter(code_stn == 74|
                code_stn == 287|
                code_stn == 145|
                code_stn == 281|
                code_stn == 278|
                code_stn == 291) %>% 
  apply_cmonth(.) %>% 
  group_by(code_stn) %>%
  nest() %>% 
  mutate(
    data = map(data, apply_cyears)
  )
  
# Inclui os dois últimos anos de qnat_obs para cada estação
test_data <- qnat_postos %>% 
  mutate(
    test_data = map(data,get_testdt)
    ) %>% 
  select(code_stn,test_data)

# Inclui as observações de qnat_obs com exceção dos dois últimos anos 
train_data <- qnat_postos %>% 
  mutate(
    train_data = map(data,get_traindt)
    )%>% 
  select(code_stn,train_data)

# Modelos e predições resultantes de 5 iterações para cada posto
ensemble_postos <- train_data %>% 
  mutate(
    models = map(train_data,ensemble_models),
    preds = map(train_data,ensemble_preds)
  )

# unlist(teste,recursive=FALSE) --> tira um level da lista

# Adiciona os parâmetros médios k e w, predições médias (5 iterações) e 
# predições usando k e w médios
pred_data <- ensemble_postos %>% 
  mutate(
    model_params = map(models,get_mpar), # parâmetros k e w médios
    mean_preds = unlist(map(preds,get_mpred),
                        recursive = FALSE), # predições médias das iter
    pred_mpar = map(train_data, # predições usando k e w médios
                    ~ensemble_mpar(.x,model_params))
  )

# Teste
# set.seed(1)
# modelo_teste <- psf(df_teste$train_data[[1]][[2]],
#                     k = 2, 
#                     w = 3,
#                     cycle = 12)
# pred_teste <- predict(modelo_teste, n.ahead = 24)

# Seleciona observações, predições médias e predições usando k e w médios   
pobs_postos <- pred_data %>%  # predições e observações 
  inner_join(.,
             test_data,
             by = "code_stn") %>% 
  select(test_data,mean_preds,pred_mpar) %>% 
  unnest() 



aval_postos <-  pobs_postos %>%  
  summarise(KGE_mpreds = KGE(sim = mean_preds , obs = qnat_obs),
            KGE_predmpar = KGE(sim = pred_mpar, obs = qnat_obs)
  ) %>% 
  arrange(-KGE_mpreds)
  

# Gráfico 
posto287 <- pobs_postos %>% 
  filter(code_stn == "287") %>% 
  select(date,qnat_obs,pred_mpar)# melhor KGE usando pred_mpar

posto287_ts <- xts(posto287[,3:4], 
                   order.by = as.Date(posto287[["date"]]))
forecast::autoplot(posto287_ts, facets = FALSE)




# Teste ensemble para todos os postos -------------------
qnat_all <- qnat_data %>%
  apply_cmonth(.) %>% 
  group_by(code_stn) %>%
  nest() %>% 
  mutate(
    data = map(data, apply_cyears)
  )

# Inclui os dois últimos anos de qnat_obs para cada estação
test_data_all <- qnat_all %>% 
  mutate(
    test_data = map(data,get_testdt)
  ) %>% 
  select(code_stn,test_data)

# Inclui as observações de qnat_obs com exceção dos dois últimos anos 
train_data_all <- qnat_all %>% 
  mutate(
    train_data = map(data,get_traindt)
  )%>% 
  select(code_stn,train_data)

# Modelos e predições resultantes de 5 iterações para cada posto
start_time <- Sys.time()
ensemble_postos_all <- train_data_all %>% 
  mutate(
    models = map(train_data,ensemble_models),
    preds = map(train_data,ensemble_preds)
  )
end_time <- Sys.time() # Demorou 8.775542 mins

#saveRDS(ensemble_postos_all, file = here('doc', 'ensemble_all.rds'))
#load_ensall <- readRDS(here('doc', 'ensemble_all.rds'))

# unlist(teste,recursive=FALSE) --> tira um level da lista

# Adiciona os parâmetros médios k e w, predições médias (5 iterações) e 
# predições usando k e w médios
pred_data_all <- ensemble_postos_all %>% 
  mutate(
    model_params = map(models,get_mpar), # parâmetros k e w médios
    mean_preds = unlist(map(preds,get_mpred),
                        recursive = FALSE), # predições médias das iter
    pred_mpar = map(train_data, # predições usando k e w médios
                    ~ensemble_mpar(.x,model_params))
  )


# Teste
# set.seed(1)
# modelo_teste <- psf(df_teste$train_data[[1]][[2]],
#                     k = 2, 
#                     w = 3,
#                     cycle = 12)
# pred_teste <- predict(modelo_teste, n.ahead = 24)

# Seleciona observações, predições médias e predições usando k e w médios   
pobs_postos_all <- pred_data_all %>%  # predições e observações 
  inner_join(.,
             test_data_all,
             by = "code_stn") %>% 
  select(test_data,mean_preds,pred_mpar) %>% 
  unnest() 



aval_postos_all <-  pobs_postos_all %>%  
  summarise(KGE_mpreds = KGE(sim = mean_preds , obs = qnat_obs),
            KGE_predmpar = KGE(sim = pred_mpar, obs = qnat_obs),
            PBIAS = pbias(sim = mean_preds, obs = qnat_obs),
            NRMSE = nrmse(sim = mean_preds, obs = qnat_obs),
            NSE = NSE(sim = mean_preds, obs = qnat_obs),
  ) %>% 
  arrange(-KGE_mpreds)

# Gráfico 
posto287 <- pobs_postos_all %>% 
  filter(code_stn == "287") %>% 
  select(date,qnat_obs,pred_mpar)# melhor KGE usando pred_mpar

posto287_ts <- xts(posto287[,3:4], 
                   order.by = as.Date(posto287[["date"]]))
forecast::autoplot(posto287_ts, facets = FALSE)

# Crossvalidation usando apenas um posto 287 ------------------------
# Baseado em: https://robjhyndman.com/hyndsight/tscvexample/
# Faz previsões 12 meses a frente

library(tsbox)

posto <- 287
qnat_posto287 <- qnat_data %>% 
  sel_posto(.,posto) %>% 
  apply_cmonth() %>% 
  apply_cyears()


order.by = as.Date(posto287[["date"]])


xts_qnat287 <- xts(
  qnat_posto287[,"qnat_obs"],
  order.by = as.Date(qnat_posto287[["date"]])
)

ts_qnat287 <- ts_ts(xts_qnat287)


k <- 24
n <- nrow(qnat_posto287)
st <- tsp(ts_qnat287)[1]+(k-2)/12
n_ahead = 12

# for (i in 1:(n - k))
# {
#   train <- subset(ts_qnat287, end = n - (n_ahead) * i + 1)
#   test <- subset(auscafe, start = length(auscafe) - 60)
# }



