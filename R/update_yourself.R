#' Update \code{otteRagent} directory from \code{git}
#'
#' @param otteRagent_directory path to the \code{otteRagent} directory
#' @param log_message message for adding to logs
#'
#' @importFrom gert git_pull
#' @importFrom logger log_debug
#' @importFrom logger log_error
#' @importFrom logger log_info
#'
#' @export

update_yourself <- function(otteRagent_directory = getOption("otteRagent_directory"),
                            log_message = "Обновляю себя"){

  logger::log_debug("🦦  Запуск умения `update_yourself`")

  # проверка параметров -----------------------------------------------------

  logger::log_debug("🦦  проверка параметров")

  if(exists("otteRagent_directory")){
    logger::log_debug("🦦  параметр `otteRagent_directory` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `otteRagent_directory`")
    stop()
  }

  if(exists("log_message")){
    logger::log_debug("🦦  параметр `log_message` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `log_message`")
    stop()
  }

  # начало работы функции ---------------------------------------------------

  logger::log_info("🦦  {log_message}")

  gert::git_pull("origin",
                 repo = otteRagent_directory)

  logger::log_debug("🦦  Завершение запуска умения `update_yourself`")
}
