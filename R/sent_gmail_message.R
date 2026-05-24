#' Sent message via `gmailr`
#'
#' @param to value for the receiver's mail address
#' @param subject value for the subject of the mail
#' @param message value for the message of the mail formatted as markdown
#' @param log_message message for adding to logs
#'
#' @importFrom logger log_debug
#' @importFrom logger log_info
#' @importFrom stringr str_c
#' @importFrom litedown mark
#' @importFrom utils installed.packages
#' @importFrom gmailr gm_mime
#' @importFrom gmailr gm_to
#' @importFrom gmailr gm_subject
#' @importFrom gmailr gm_text_body
#' @importFrom gmailr gm_send_message
#' @importFrom gmailr gm_profile
#'
#' @export

sent_gmail_message <- function(to = getOption("otteRagent_preferred_out_mail"),
                               subject = "no subject",
                               message,
                               log_message = "Отправляю письмо на gmail"){

  logger::log_debug("🦦  Запуск умения `sent_gmail_message`")

  # проверка параметров -----------------------------------------------------

  logger::log_debug("📨  проверка параметров")

  if(exists("to")){
    logger::log_debug("📨  параметр `to` есть")
  } else {
    logger::log_error("📨  нет параметра `to`")
    stop()
  }

  if(!is.null(to)){
    logger::log_debug("📨  параметр `to` заполнен")
  } else {
    logger::log_error("📨  не заполнен параметр `to`")
    stop()
  }

  if(exists("subject")){
    logger::log_debug("📨  параметр `subject` есть")
  } else {
    logger::log_error("📨  нет параметра `subject`")
    stop()
  }

  if(exists("message")){
    logger::log_debug("📨  параметр `message` есть")
  } else {
    logger::log_error("📨  нет параметра `message`")
    stop()
  }

  if(exists("log_message")){
    logger::log_debug("📨  параметр `log_message` есть")
  } else {
    logger::log_error("📨  нет параметра `log_message`")
    stop()
  }

  if(is.null(gmailr::gm_profile())){
    logger::log_error("📨  Not logged in as any specific Google user.")
    stop()
  } else {
    logger::log_debug("📨  OAuth client set up")
  }

  # начало работы функции ---------------------------------------------------

  logger::log_info("🦦  {log_message}")

  stringr::str_c("Привет!\n\n", message, "\n\n🦦") |>
    litedown::mark() ->
    message_body

  gmailr::gm_mime() |>
    gmailr::gm_to(to) |>
    gmailr::gm_subject(subject) |>
    gmailr::gm_text_body(message_body,
                         content_type = "text/html",
                         charset = "utf-8",
                         encoding = "base64") |>
    gmailr::gm_send_message()

  logger::log_debug("🦦  Завершение запуска умения `sent_gmail_message`")
}
