#' Run the task
#'
#' @param path_to_tasks path to tasks
#' @param log_message message for adding to logs
#'
#' @importFrom logger log_debug
#' @importFrom logger log_info
#' @importFrom logger log_error
#' @importFrom logger log_warn
#' @importFrom readr read_csv
#'

apply_inbox_rules <- function(inbox_rules_path = getOption("otteRagent_path_to_inbox_rules"),
                              log_message = "Выполняю правила обработки почты") {

  logger::log_debug("📨  Запуск умения `apply_inbox_rules`")

  # проверка параметров -----------------------------------------------------

  inbox_rules <- readr::read_csv(inbox_rules_path)

  # начало работы функции ---------------------------------------------------

  logger::log_info("📨  {log_message}")


  logger::log_debug("📨  Завершение запуска умения `run_task`")
}




# my_threads <- gmailr::gm_threads(search = "is:unread")
#
# seq(1, length(my_threads[[1]]$threads)) |>
#   purrr::map(function(i){
#
#     thread <- gmailr::gm_thread(gmailr::gm_id(my_threads)[[i]])
#
#     tibble::tibble(from = gmailr::gm_from(thread$messages[[1]]),
#                    to = gmailr::gm_to(thread$messages[[1]]),
#                    subject = gmailr::gm_subject(thread$messages[[1]]),
#                    date = gmailr::gm_date(thread),
#                    snippet = thread$messages[[1]]$snippet,
#                    labels = thread$messages[[1]]$labelIds |>
#                      unlist() |>
#                      stringr::str_c(collapse = ";"),
#                    id = thread$id) |>
#       dplyr::mutate(year = stringr::str_extract(date, "\\d{4}$"),
#                     date = stringr::str_remove(date, "\\d{4}$"),
#                     date = stringr::str_remove(date, "^\\w{3} "),
#                     date = stringr::str_c(year, " ", date),
#                     date = lubridate::ymd_hms(date))
#   }) |>
#   purrr::list_rbind() ->
#   result
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
