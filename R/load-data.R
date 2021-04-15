
qnat_dly_ons <- function() {
  
  qnat_file <- here("input", "qnat_dly_ons.RDS")
  
  if(fs::file_exists(qnat_file)){
    return(readr::read_rds(qnat_file))
  }
  
  # Link dos dados de vazões naturais
  dropbox_link <- file.path('https://www.dropbox.com/s/d40adhw66uwueet/',
                'VazoesNaturaisONS_D_87UHEsDirceuAssis_2018.dat?dl=1')
  # Retorna nome do arquivo que irá ser salvo
  tmp_file <- fs::file_temp()

  # Fazer o download do arquivo
  download.file(
    url = dropbox_link,
    destfile = tmp_file,
    mode = "wb"
  )

  # Importar os dados
  qnat_data <- HEobs::import_qnat(
    file = tmp_file,
    complete = TRUE,
    add_stn_info = TRUE
  )

  readr::write_rds(qnat_data, file = qnat_file)
  checkmate::assert_file_exists(qnat_file)
  qnat_data
}

