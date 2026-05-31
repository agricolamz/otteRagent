#' Apply inbox rules
#'
#' @param path_to_tasks path to tasks
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


apply_inbox_rules <- function(inbox_rules_path = getOption("otteRagent_path_to_inbox_rules"),
                              log_message = "Выполняю правила обработки почты") {

  logger::log_debug("📨  Запуск умения `apply_inbox_rules`")

  # проверка параметров -----------------------------------------------------

  logger::log_debug("📨  проверка параметров")

  if(exists("inbox_rules_path")){
    logger::log_debug("📨  параметр `inbox_rules_path` есть")
  } else {
    logger::log_error("📨  нет параметра `inbox_rules_path`")
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
                    str_detect_subject = "c",
                    add_labels = "c",
                    remove_labels = "c")) |>
    colnames() ->
    inbox_rules_colnames

  expected_colnames <- c("id", "from", "to", "str_detect_subject", "add_labels", "remove_labels")

  absent_colnames <- expected_colnames[which(!(expected_colnames %in% inbox_rules_colnames))]

  if(length(absent_colnames) == 0){
    logger::log_debug("📨  в файле с правилами правильные колонки")
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
                    str_detect_subject = "c",
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

  # начало работы функции ---------------------------------------------------

  logger::log_info("📨  {log_message}")

  inbox_rules <- readr::read_csv(inbox_rules_path,
                                 show_col_types = FALSE,
                                 progress = FALSE,
                                 col_types = list(
                                   id = "d",
                                   from = "c",
                                   to = "c",
                                   str_detect_subject = "c",
                                   add_labels = "c",
                                   remove_labels = "c"))

  getOption("otteRagent_path_to_inbox_rules") |>
    stringr::str_remove("\\.csv") |>
    stringr::str_c("_logs.csv") ->
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
      result |>
        dplyr::left_join(inbox_rules[i, ], by = c("from", "to")) |>
        dplyr::filter(!is.na(add_labels),
                      !is.na(remove_labels),
                      stringr::str_detect(subject, inbox_rules$str_detect_subject[i]))
    }) |>
    purrr::list_rbind() ->
    changes

  changes |>
    readr::write_csv(inbox_rules_logs, append = TRUE)

  logger::log_debug("📨  Завершение запуска умения `run_task`")
}


#
#
# result |>
#   mutate(labels = str_split(labels, ";")) |>
#   unnest_longer(labels) |>
#   count(labels, sort = TRUE)
#
#
#
#
# seq_along(inbox_rules$id) |>
#   walk(function(i){
#     result |>
#       filter(from %in% inbox_rules$from[i],
#              to %in% inbox_rules$to[i],
#              str_detect(subject, inbox_rules$str_detect_subject[i])) |>
#       pull(id) |>
#       walk(ids, function(id){
#         gm_modify_thread_fixed(id = id,
#                                add_labels = inbox_rules$add_labels,
#                                remove_labels = inbox_rules$remove_labels)
#       })
#   })
