#' Remind via `gmail`
#'
#' @param at time in the format `yyyy-mm-dd hh:mm`, other possible value is `now`.
#' @param tz time zone. By default gathered from the system.
#' @param message message for sending
#' @param subject subject of the mail
#' @param to where to send the notification
#' @param log_message message for adding to logs
#'
#' @importFrom logger log_debug
#' @importFrom logger log_error
#' @importFrom logger log_info
#' @importFrom purrr map_lgl
#' @importFrom lubridate ymd_hm
#' @importFrom lubridate now
#'
#' @export

remind_me <- function(at,
                      tz = Sys.timezone(),
                      message,
                      subject = "автоматическое напоминание",
                      to = getOption("otteRagent_preferred_out_mail"),
                      log_message = "Решаю, выслать ли автоматическое напоминание"){

  logger::log_debug("🦦  Запуск умения `remind_me`")

  # проверка параметров -----------------------------------------------------

  skills <- c("sent_gmail_message", "add_to_backlog")

  if(sum(skills |> purrr::map_lgl(exists)) == length(skills)){
    logger::log_debug("🔔  Все необходимые умения есть.")
  } else {
    logger::log_error("🔔  Один из следующих умений не установлен: {skills}")
    stop()
  }

  if(exists("at")){
    logger::log_debug("🔔  параметр `at` есть")
  } else {
    logger::log_error("🔔  не заполнен параметр `at`")
    stop()
  }

  if(lubridate::ymd_hm(at, quiet = TRUE) |> is.na() | at == "now"){
    logger::log_error("🔔  параметр `at` отличается от формата ymdhm; еще возможное значение --- `now`")
    stop()
  } else {
    logger::log_debug("🔔  параметр `at` парсится как дата")
  }

  if(exists("tz")){
    logger::log_debug("🔔  параметр `tz` есть")
  } else {
    logger::log_error("🔔  не заполнен параметр `tz`")
    stop()
  }

  if(exists("message")){
    logger::log_debug("🔔  параметр `message` есть")
  } else {
    logger::log_error("🔔  не заполнен параметр `message`")
    stop()
  }

  if(exists("subject")){
    logger::log_debug("🔔  параметр `subject` есть")
  } else {
    logger::log_error("🔔  не заполнен параметр `subject`")
    stop()
  }

  if(exists("to")){
    logger::log_debug("🔔  параметр `to` есть")
  } else {
    logger::log_error("🔔  не заполнен параметр `to`")
    stop()
  }

  if(exists("log_message")){
    logger::log_debug("🔔  параметр `log_message` есть")
  } else {
    logger::log_error("🔔  не заполнен параметр `log_message`")
    stop()
  }

  # вызов функции -----------------------------------------------------------

  logger::log_info("🔔  {log_message}")

  if(at == "now"){
    time_for_comparison <- lubridate::now(tz = tz)
  } else {
    time_for_comparison <- lubridate::ymd_hm(at, quiet = TRUE, tz = tz)
  }

  if(lubridate::now(tz = tz) <= time_for_comparison){
    add_to_backlog(task = "Прислать напоминание",
                   skill = "remind_me",
                   schedule = "once",
                   ignore = NA,
                   params = list(at = at,
                                 tz = tz,
                                 message = message,
                                 subject = subject,
                                 log_message = log_message))
  } else {
    sent_gmail_message(to = to,
                       subject = subject,
                       message = message,
                       log_message = log_message)
  }

  logger::log_debug("🦦  Завершение запуска умения `remind_me`")

}
