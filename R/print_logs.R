#' Print logs
#'
#' @param log_message message for adding to logs
#'
#' @importFrom readr read_lines
#'
#' @export

print_logs <- function(log_message = "Печатаю логи:"){

  logger::log_debug("🦦  Запуск умения `print_logs`")

  # проверка параметров -----------------------------------------------------

  logger::log_debug("🦦  проверка параметров")

  if(file.exists(getOption("otteRagent_path_to_logs"))){
    logger::log_debug("🦦  файл логов есть")
  } else {
    logger::log_error("🦦  файла логов нет")
    stop()
  }

  # начало работы функции ---------------------------------------------------

  logger::log_info("🦦  {log_message}\n\n")

  getOption("otteRagent_path_to_logs") |>
    readr::read_lines() |>
    cat(sep = "\n")

  logger::log_debug("🦦  Завершение запуска умения `print_logs`")
}
