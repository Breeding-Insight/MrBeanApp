#' QBMS conexion
#'
#' @param url go to QBMS documentation
#' @param engine "bms" or "breedbase"
#' @param path go to QBMS documentation
#' @param time_out go to QBMS documentation
#' @param no_auth go to QBMS documentation
#' @param username username
#' @param password password
#'
#' @return a list
#' @noRd
qbmsbrapi <- function(url = "https://sandbox.breedinginsight.net/v1/programs/025a8e6e-15fc-4488-8d26-41eb16107a95",
                      engine = '',
                      path = '',
                      time_out = 300,
                      no_auth = FALSE,
                      brapi_ver = 'v2',
                      username = NULL,
                      password = NULL) {
  if (is.null(url) | url == "") {
    return()
  }
  bmsbase <- QBMS::set_qbms_config(
    url = url,
    path = path,
    brapi_ver = 'v2',
    engine = engine
  )

  if (is.null(password) | password == "") {
    return()
  }
  QBMS::set_token(password)

  crops <- QBMS::list_crops()
  return(list(bmsbase = bmsbase,  crops = crops))
}


#' Get Programs
#'
#' @param crop crop
#'
#' @return a list with programs
#' @noRd
qbmsprograms <- function(crop = NULL) {
  programs <- QBMS::list_programs()
  return(programs)
}

#' Get trials
#'
#' @param program program
#'
#' @return a list with trials
#' @noRd
qbmstrials <- function(program = NULL) {
  if (is.null(program)) {
    return()
  }
  QBMS::set_program(program)
  trials <- QBMS::list_trials()
  return(trials)
}

#' Get studies
#'
#' @param trial trial
#'
#' @return a list with studies
#' @noRd
#'
qbmsstudies <- function(trial = NULL) {
  if (is.null(trial)) {
    return()
  }
  QBMS::set_trial(trial)
  studies <- QBMS::list_studies()
  return(studies)
}

#' Get dataset
#'
#' @param studies string studies
#' @param dt_studies data.frame studies
#'
#' @return data.frame
#' @noRd
dataqbms <- function(studies = NULL, dt_studies = NULL) {
  if (is.null(studies)) {
    return()
  }
  if (is.null(dt_studies)) {
    return()
  }

  trial_study <- function(study, dt_studies) {
    trial <- dt_studies[dt_studies$studyName == study, "trial"] %>% as.character()
    QBMS::set_trial(trial)
    QBMS::set_study(study)
    data <- QBMS::get_study_data() %>%
      data.frame(check.names = TRUE, stringsAsFactors = T) %>%
      utils::type.convert(as.is = FALSE)
    return(data)
  }

  mult_dt <- lapply(studies, trial_study, dt_studies = dt_studies)
  engine <- QBMS::debug_qbms()$config$engine
  if(engine %in% "breedbase") {
    mult_dt <- data.table::rbindlist(
      l = mult_dt,
      fill = TRUE
    ) %>% 
      as.data.frame()
  } else {
    names(mult_dt) <- dt_studies$trial
    mult_dt <- data.table::rbindlist(
      l = mult_dt,
      fill = TRUE,
      idcol = "trial"
    ) %>% 
      as.data.frame()
  }

  
  return(mult_dt)
}
