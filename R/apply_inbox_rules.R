#' Apply inbox rules
#'
#' @param inbox_rules_path path to the file of inbox rules
#' @param log_message message for adding to logs
#'
#' @importFrom logger log_debug
#' @importFrom logger log_info
#' @importFrom logger log_error
#' @importFrom logger log_warn
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom stringr str_remove
#' @importFrom stringr str_c
#' @importFrom stringr str_extract
#' @importFrom gmailr gm_threads
#' @importFrom gmailr gm_thread
#' @importFrom gmailr gm_id
#' @importFrom gmailr gm_from
#' @importFrom gmailr gm_to
#' @importFrom gmailr gm_subject
#' @importFrom gmailr gm_date
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate now
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom dplyr slice
#' @importFrom dplyr rename
#' @importFrom dplyr pull
#'
#' @export

apply_inbox_rules <- function(inbox_rules_path = getOption("otteRagent_path_to_inbox_rules"),
                              daily_report_interval = NA,
                              log_message = "Выполняю правила обработки почты") {

  logger::log_debug("📨  Запуск умения `apply_inbox_rules`")

  # проверка параметров -----------------------------------------------------

  logger::log_debug("📨  проверка параметров")

  "report_inbox_logs"

  if(exists("inbox_rules_path")){
    logger::log_debug("📨  параметр `inbox_rules_path` есть")
  } else {
    logger::log_error("📨  нет параметра `inbox_rules_path`")
    stop()
  }

  if(file.exists(inbox_rules_path)){
    logger::log_debug("📨  файл с правилами оброботки почты есть")
  } else {
    logger::log_error("📨  нет файла с правилами оброботки почты")
    stop()
  }

  readr::read_csv(inbox_rules_path,
                  show_col_types = FALSE,
                  progress = FALSE,
                  n_max = 0,
                  col_types = list(
                    id = "d",
                    from = "c",
                    to = "c",
                    subject = "c",
                    add_labels = "c",
                    remove_labels = "c")) |>
    colnames() ->
    inbox_rules_colnames

  expected_colnames <- c("id", "from", "to", "subject", "add_labels", "remove_labels")

  absent_colnames <- expected_colnames[which(!(expected_colnames %in% inbox_rules_colnames))]

  if(length(absent_colnames) == 0){
    logger::log_debug("📨  в файле с правилами верные колонки")
  } else {
    logger::log_error("📨  в файле с правилами нет колонки {absent_colnames}")
    stop()
  }

  readr::read_csv(inbox_rules_path,
                  show_col_types = FALSE,
                  progress = FALSE,
                  n_max = 1,
                  col_types = list(
                    id = "d",
                    from = "c",
                    to = "c",
                    subject = "c",
                    add_labels = "c",
                    remove_labels = "c")) |>
    nrow() ->
    inbox_rules_emptyness

  if(inbox_rules_emptyness == 1){
    logger::log_debug("📨  в файле с правилами есть правила")
  } else {
    logger::log_error("📨  в файле с правилами нет правил")
    stop()
  }

  if(is.null(gmailr::gm_profile())){
    logger::log_error("📨  Not logged in as any specific Google user.")
    stop()
  } else {
    logger::log_debug("📨  OAuth client set up")
  }

  if(exists("daily_report_interval")){
    logger::log_debug("📨  параметр `daily_report_interval` есть")
  } else {
    logger::log_error("📨  нет параметра `daily_report_interval`")
    stop()
  }

  if(!is.na(daily_report_interval)){

    if(str_detect(daily_report_interval, "\\d{1,2}:\\d{2}-\\d{1,2}:\\d{2}")){
      logger::log_debug("📨  параметр `daily_report_interval` имеет правильный формат")
    } else {
      logger::log_error("📨  параметр `daily_report_interval` должен быть либо NA, либо в формате `4:30-5:30`")
      stop()
    }


  }


  # начало работы функции ---------------------------------------------------

  logger::log_info("📨  {log_message}")

  inbox_rules <- readr::read_csv(inbox_rules_path,
                                 show_col_types = FALSE,
                                 progress = FALSE,
                                 col_types = list(
                                   id = "d",
                                   from = "c",
                                   to = "c",
                                   subject = "c",
                                   add_labels = "c",
                                   remove_labels = "c"))

  getOption("otteRagent_path_to_logs") |>
    stringr::str_remove("logs\\.txt") |>
    stringr::str_c("inbox_rules_logs.csv") ->
    inbox_rules_logs

  my_threads <- gmailr::gm_threads(search = "is:unread")

  seq(1, length(my_threads[[1]]$threads)) |>
    purrr::map(function(i){

      thread <- gmailr::gm_thread(gmailr::gm_id(my_threads)[[i]])

      n_messages <- thread$messages |> length()

      tibble::tibble(from = gmailr::gm_from(thread$messages[[n_messages]]),
                     to = gmailr::gm_to(thread$messages[[n_messages]]),
                     subject = gmailr::gm_subject(thread$messages[[n_messages]]),
                     date = gmailr::gm_date(thread) |>
                       lubridate::parse_date_time(orders = "amdHMSy"),
                     snippet = thread$messages[[n_messages]]$snippet,
                     labels = thread$messages[[n_messages]]$labelIds |>
                       unlist() |>
                       stringr::str_c(collapse = ";"),
                     thread_id = thread$id)
    }) |>
    purrr::list_rbind() ->
    result

  seq_along(inbox_rules$id) |>
    purrr::map(function(i){

      if(is.na(inbox_rules$from[i])){
        index_from <- TRUE
      } else {
        index_from <- stringr::str_detect(result$from, inbox_rules$from[i])
      }

      if(is.na(inbox_rules$to[i])){
        index_to <- TRUE
      } else {
        index_to <- stringr::str_detect(result$to, inbox_rules$to[i])
        index_to <- ifelse(is.na(index_to), TRUE, index_to)
      }

      if(is.na(inbox_rules$subject[i])){
        index_subject <- TRUE
      } else {
        index_subject <- stringr::str_detect(result$subject, inbox_rules$subject[i])
      }

      result |>
        dplyr::slice(which(index_from & index_to & index_subject)) |>
        dplyr::mutate(add_labels = inbox_rules$add_labels[i],
                      remove_labels = inbox_rules$remove_labels[i],
                      rule_id = inbox_rules$id[i])
    }) |>
    purrr::list_rbind() |>
    mutate(reported = FALSE) ->
    changes

  if(nrow(changes) > 0){

    if(file.exists(inbox_rules_logs)){
      changes |>
        readr::write_csv(inbox_rules_logs, append = TRUE, na = "")
    } else {
      changes |>
        readr::write_csv(inbox_rules_logs, na = "")
    }

    readr::read_csv(inbox_rules_logs,
                    n_max = 0,
                    show_col_types = FALSE,
                    progress = FALSE) |>
      colnames() ->
      inbox_rules_logs_colnames

    expected_colnames <- c("from", "to", "subject", "date", "snippet", "labels",
                           "thread_id", "rule_id", "subject",
                           "add_labels", "remove_labels")

    absent_colnames <- expected_colnames[which(!(expected_colnames %in% inbox_rules_logs_colnames))]

    if(length(absent_colnames) == 0){
      logger::log_debug("📨  в файле с логами применения правил верные колонки")
    } else {
      logger::log_error("📨  в файле с логами применения правил нет колонки {absent_colnames}")
      stop()
    }

    changes$rule_id |>
      unique() |>
      purrr::walk(function(rule_id){
        changes |>
          dplyr::filter(rule_id == rule_id) |>
          dplyr::pull(thread_id) |>
          purrr::walk(function(thread_id){
            i <- which(changes$thread_id == thread_id)
            gm_modify_thread_fixed(id = thread_id,
                                   add_labels = changes$add_labels[i],
                                   remove_labels = inbox_rules$remove_labels[i])
          })
      })
  } else {

    logger::log_info("📨  Не найдено писем для применения правил")
  }

  if(!is.na(daily_report_interval)){

    daily_report_interval |>
      stringr::str_split("-") |>
      unlist() |>
      stringr::str_split(":") |>
      unlist() |>
      as.double() ->
      for_intervals

    interval_start <- lubridate::now()
    lubridate::hour(interval_start) <- for_intervals[1]
    lubridate::minute(interval_start) <- for_intervals[2]

    interval_end <- lubridate::now()
    lubridate::hour(interval_end) <- for_intervals[3]
    lubridate::minute(interval_end) <- for_intervals[4]

    if(lubridate::int_overlaps(
      lubridate::interval(
        lubridate::now(),
        lubridate::now()),
      lubridate::interval(
        interval_start,
        interval_end))){

      report_inbox_logs()

    }
  }

  logger::log_debug("📨  Завершение запуска умения `apply_inbox_rules`")
}

