#' Read inbox logs and sent a mail about them
#'
#' @param log_message message for adding to logs
#' @param path_to_tasks path to tasks
#'
#' @importFrom logger log_debug
#' @importFrom logger log_info
#' @importFrom logger log_error
#' @importFrom stringr str_c
#' @importFrom stringr str_glue_data
#' @importFrom purrr map_lgl
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#'
#' @export

report_inbox_logs <- function(path_to_tasks = stringr::str_c(getOption("otteRagent_directory"), "tasks.csv"),
                              log_message = "Выполняю отправку логов изменени в почте"){

  logger::log_debug("📨  Запуск умения `report_inbox_logs`")

  # проверка параметров -----------------------------------------------------

  skills <- c("add_to_backlog")

  if(sum(skills |> purrr::map_lgl(exists)) == length(skills)){
    logger::log_debug("🦦  Все необходимые умения есть.")
  } else {
    logger::log_error("🦦  Один из следующих умений не установлен: {skills}")
    stop()
  }

  getOption("otteRagent_directory") |>
    stringr::str_c("logs/inbox_rules_logs.csv") ->
    inbox_rules_logs

  if(file.exists(inbox_rules_logs)){
    logger::log_debug("📨  файл с логами применения правил оброботки почты есть")
  } else {
    logger::log_error("📨  нет файла с логами применения правил оброботки почты")
    stop()
  }

  readr::read_csv(inbox_rules_logs,
                  show_col_types = FALSE,
                  progress = FALSE,
                  n_max = 0,
                  col_types = list(
                    from = "c",
                    to = "c",
                    subject = "c",
                    date = "T",
                    snippet = "c",
                    labels = "c",
                    thread_id = "c",
                    add_labels = "c",
                    remove_labels = "c",
                    rule_id = "i",
                    reported = "l")) |>
    colnames() ->
    inbox_rules_colnames

  expected_colnames <- c("rule_id", "from", "to", "subject", "date", "snippet", "labels", "thread_id", "add_labels", "remove_labels")

  absent_colnames <- expected_colnames[which(!(expected_colnames %in% inbox_rules_colnames))]

  if(length(absent_colnames) == 0){
    logger::log_debug("📨  в файле с логами применения правил оброботки почты верные колонки")
  } else {
    logger::log_error("📨  в файле с логами применения правил оброботки почты нет колонки {absent_colnames}")
    stop()
  }

  # начало работы функции ---------------------------------------------------

  logger::log_info("📨  {log_message}")

  readr::read_csv(inbox_rules_logs,
                  show_col_types = FALSE,
                  progress = FALSE,
                  col_types = list(
                    from = "c",
                    to = "c",
                    subject = "c",
                    date = "T",
                    snippet = "c",
                    labels = "c",
                    thread_id = "c",
                    add_labels = "c",
                    remove_labels = "c",
                    rule_id = "i",
                    reported = "l")) ->
    inbox_logs

  inbox_logs |>
    dplyr::filter(!reported) ->
    not_reported_inbox_logs

  if(nrow(not_reported_inbox_logs) > 0){

    not_reported_inbox_logs |>
      stringr::str_glue_data("
---

Added labels: {add_labels}

Removed labels: {remove_labels}

- {date}
- {from}
- {to}
- {subject}
- {snippet}
") |>
      stringr::str_c(collapse = "\n\n") ->
      message

    add_to_backlog(task = "Отправить письмо с логами изменений в почте",
                   skill = "sent_gmail_message",
                   schedule = "once",
                   immediate_execute = TRUE,
                   log_message = "Добавляю отправку письма с логами изменений в почте в список задач",
                   params = list(subject = "Логи изменений в почте",
                                 message = message),
                   path_to_tasks = path_to_tasks)

    inbox_logs |>
      dplyr::mutate(reported = TRUE) |>
      readr::write_csv(inbox_rules_logs, na = "")

  } else {
    logger::log_info("📨  Все логи уже зарепорчены.")
  }

  logger::log_debug("📨  Завершение запуска умения `report_inbox_logs`")
}
