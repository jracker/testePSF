

#' Contabiliza observações válidas
#'
#' @param x vetor numérico
#'
#' @return total de observações não faltantes em x.
#'
nvalid <- function(x) {
  # if(all(is.na(x))) return(0)
  sum(!is.na(x))
}

#' Média com tratamento para caso de todos dados faltantes
#'
#' @param x um vetor numérico
#'
#' @return valor médio de x
#'

mean_wise <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  }
  mean(x, na.rm = TRUE)
}


#' Calcula a moda 
#' 
#' @param x um vetor numérico
#' 
#' @return um inteiro correspondente a moda do vetor numérico
#' 
moda <- function(x){
  which.max(tabulate(x))
}


#' Seleciona dados de um posto
#'
#' @param df um tibble ou data frame
#' 
#' @param station código da estação
#' 
#' @return tibble ou data frame da estação selecionada
#'
sel_station <- function(df, station) {
  # Função para selecionar um posto específico via código
  df %>%
    dplyr::filter(code_stn == station)
}

#' Filtra os meses completos dos dados
#' 
#'  @param df um tibble ou data frame
#'  
#'  @param ndays_thresh  um inteiro
#'  
#'  @return um tibble ou data frame com os meses completos
#'  
apply_cmonth <- function(df, ndays_thresh = 28) {
  df <- df %>%
    dplyr::group_by(date = floor_date(date, "month"), code_stn) %>%
    dplyr::summarise(
      qnat_obs = mean_wise(qnat),
      valid = nvalid(qnat),
      N = n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(valid >= ndays_thresh) %>%
    select(date, code_stn, qnat_obs)
  return(df)
}

#' Identifica anos completos, ou seja, com 12 meses de observações válidas
#'  
#' @param df um tibble ou data frame
#' 
#' @return vetor com os anos que apresentam 12 meses de observações válidas
#' 
get_cyears <- function(df) {
  cyrs <- df %>%
    group_by(ano = lubridate::year(date)) %>%
    tally() %>%
    filter(n == 12) %>%
    pull(ano)
  return(cyrs)
}

#' Filtragem dos dados com anos completos
#'  
#'  @param df um tibble ou data frame
#'  
#'  @param ndays_thresh  um inteiro
#'  
#'  @return df um tibble ou data frame com os meses completos
#'  
apply_cyears <- function(df) {
  # Usar depois de agrupar os dados caso selecionado mais de um posto
  cyrs <- get_cyears(df)
  df_cyrs <- df %>%
    filter(lubridate::year(date) %in% cyrs)
  return(df_cyrs)
}

#' Seleciona dados de treinamento para aplicação do PSF
#'
#' @param df data frame com série mensal dos dados de vazão
#' 
#' @param yrs número de anos que serão removidos das observações para teste do 
#' PSF.
#' Valor pré-definido como 2 anos.
#' 
get_traindt <- function(df, yrs = 2) {
  cyrs <- get_cyears(df)
  leave_out <- tail(cyrs, n = yrs)
  data <- df %>%
    filter(!lubridate::year(date) %in% leave_out)
  return(data)
}


#' Selecionar dados de teste para avaliação do PSF
#'
#' @param df data frame com dados de vazão mensal
#' @param n número de meses à frente. Pré-definido como 24.
#'
#' @return data frame com dados do período de teste
#' @export
#'
#' @examples
#' 
get_testdt <- function(df, n = 24) {
  inds <- (nrow(df) - n + 1):(nrow(df))
  df <- df[inds, ] %>%
    select(date, qnat_obs)
}


#' Aplica PSF e retorna o modelo ou suas previsões
#'
#' @param df um tibble ou data frame
#' @param n número de meses à frente. Pré-definido como 24.
#' @param predict valor lógico pré-definido para retornar as previsões
#'
#' @return vetor numérico com as previsões ou objeto da classe psf com 
#' informações do modelo
#' @export
#'
#' @examples
#' 
psf_reprod <- function(df, n = 24, predict = TRUE) {
  
  set.seed(1) # p/ reprodutibilidade
  
  model <- psf(df[, "qnat_obs"], cycle = 12)
  preds <- predict(model, n.ahead = n)
  
  if (predict) return(preds) 
  
  model
}


#' Aplica o PSF e retorna o modelo ou as previsões
#' Adaptado para uso no ensemble
#'
#' @param df um tibble ou data frame
#' @param n número de meses à frente. Pré-definido como 24.
#' @param predict valor lógico pré-definido para retornar as previsões
#'
#' @return vetor numérico com as previsões ou objeto da classe psf com 
#' informações do modelo 
#' @export
#'
#' @examples

psf4ensemble <- function(df, n = 24, predict = TRUE) {
  model <- psf(df[, "qnat_obs"], cycle = 12)
  preds <- predict(model, n.ahead = n)
  if (predict) {
    return(preds)
  } else {
    return(model)
  }
}


#' Aplica o PSF um número de vezes e retorna os modelos ou as 
#' previsões
#' @param df um tibble ou data frame
#' 
#' @param niter iterações que definem número de modelos ou de previsões
#' 
#' @param predict valor lógico pré-definido para retornar as previsões
#'
#' @return lista das previsões ou objetos de classe psf com 
#' informações do modelos 
#' 
#' @export
#'
#' @examples
#' 
ensemble_psf <- function(df, niter = 5, predict = TRUE) {
  list_preds <- list()
  list_model <- list()
  if (predict) {
    for (i in 1:niter) {
      set.seed(i)
      list_preds[[i]] <- psf4ensemble(df)
    }
    return (list_preds)
  } else {
    for (i in 1:niter) {
      set.seed(i)
      list_model[[i]] <- psf4ensemble(df, predict = FALSE)
    }
    return(list_model)
  }
}

#' Aplica o PSF utilizando os parâmetros k e w selecionados
#' 
#' @param df um tibble ou data frame
#' 
#' @param params lista de tibbles contendo os valores k e w
#' 
#' @param predict valor lógico pré-definido para retornar as previsões
#'
#' @return lista das previsões ou objetos de classe psf com 
#' informações do modelos 
#' @export
#'
#' @examples
#' 
ensemble_mpar<- function(df, params, n = 24) {
  set.seed(1)
  model <- psf(df[, "qnat_obs"],
               k = params[[1]]["k"][[1]],
               w = params[[1]]["w"][[1]],
               cycle = 12
  )
  pred <- predict(model, n.ahead = n)
  return(pred)
}

#' Seleciona os parâmetros k e w com base na moda
#' 
#' @param modelo uma lista de objetos da classe psf
#' 
#' @param niter número de modelos ou de previsões
#' 
#'
#' @return lista de tibbles com os valores k e w selecionados
#' 
#' @export
#'
#' @examples
#' 

get_modpar <- function(modelo) {
  tibble(
    k = map_dbl(modelo, "k"),
    w = map_dbl(modelo, "w")
  ) %>%
  #map_dfr(~.x) %>%
  summarise(across(c(k, w), moda))
}

get_meanpar <- function(modelo){
  tibble(
    k = map_dbl(modelo,"k"),
    w = map_dbl(modelo,"w")
  ) %>% 
    summarize(
      mean = across(c(k,w), mean_wise),
      mean = round(mean,0)
    )
}



get_mpred <- function(lista) {
  # Faz a média das predições dos modelos retornados pelo ensemble
  pred_mean <- list(
    rowMeans(as.data.frame(lista))
  )
}


# For time series cross validation -----------------------------

get_cvpar <- function(model) {
  # Parâmetros após validação cruzada
  unl_model <- unlist(model, recursive = FALSE)
  params_psf <- c(k = unl_model$k, w = unl_model$w)
}

psf_cvparam <- function(df, n = 12, params = NULL) {
  # df = train287_qmly; n = 12; params = cvm_params
  set.seed(1)
  model <- psf(df[["qnat_obs"]],
    k = params[[1]],
    w = params[[2]],
    cycle = 12
  )
  preds <- predict(model, n.ahead = n)
}
