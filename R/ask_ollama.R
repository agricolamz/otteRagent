#' Post a messages to Ollama and send the result via `gmail`
#'
#' @param ollama_model Specifies an Ollama model to call.
#' @param ollama_message Text of the prompt.
#' @param log_message message for adding to logs
#' @param path_to_tasks path to tasks
#'
#' @importFrom logger log_debug
#' @importFrom logger log_error
#' @importFrom utils installed.packages
#' @importFrom ollamar test_connection
#' @importFrom ollamar model_avail
#' @importFrom purrr map_lgl
#' @importFrom stringr str_glue
#' @importFrom stringr str_c
#'
#' @export

ask_ollama <- function(ollama_model = "gemma4:26b",
                       ollama_message,
                       log_message = "Делаю запрос модели Ollama",
                       path_to_tasks = stringr::str_c(getOption("otteRagent_directory"), "tasks.csv")){

  logger::log_debug("🦦  Запуск умения `ask_ollama`")

  # проверка параметров -----------------------------------------------------

  skills <- c("sent_gmail_message")

  if(sum(skills |> purrr::map_lgl(exists)) == length(skills)){
    logger::log_debug("🦙  Все необходимые умения есть.")
  } else {
    logger::log_error("🦙  Один из следующих умений не установлен: {skills}")
    stop()
  }

  if(exists("ollama_model")){
    logger::log_debug("🦙  параметр `ollama_model` есть")
  } else {
    logger::log_error("🦙  не заполнен параметр `ollama_model`")
    stop()
  }

  if(exists("ollama_message")){
    logger::log_debug("🦙  параметр `ollama_message` есть")
  } else {
    logger::log_error("🦙  не заполнен параметр `ollama_message`")
    stop()
  }

  if(nchar(ollama_message) > 0){
    logger::log_debug("🦙  в параметре `ollama_message` есть текст")
  } else {
    logger::log_error("🦙  не заполнен параметр `ollama_message`")
    stop()
  }

  if(ollamar::test_connection(logical = TRUE)){
    logger::log_debug("🦙  Ollama запущена")
  } else {
    logger::log_error("🦙  Ollama не запущена")
    stop()
  }

  if(ollamar::model_avail(ollama_model)){
    logger::log_debug("🦙  Модель {ollama_model} есть в списке доступных моделей")
  } else {
    logger::log_error("🦙  Модели {ollama_model} нет в списке доступных моделей")
    stop()
  }

  # вызов функции -----------------------------------------------------------
  logger::log_info("🦙  {log_message}")

  ollama_message |>
    ollamar::generate(model = ollama_model) |>
    ollamar::resp_process("text") ->
    result

  # отправка результата на почту --------------------------------------------

  if(curl::has_internet()){
    sent_gmail_message(log_message = "Отправляю письмо с ответом Ollama",
                       subject = "Ответ модели Ollama",
                       message = stringr::str_glue("Вот ответ модели:\n\n{result}\n\n---\n\n### Промпт\n\n{ollama_message}"))
  } else {
    logger::log_warn("🦦  Нет интернет соединения, так что я не отправил ответа модели")
    add_to_backlog(task = "Отправить письмо с ответом модели",
                   skill = "sent_gmail_message",
                   schedule = "once",
                   immediate_execute = TRUE,
                   log_message = "Добавляю отправку письма с ответом модели в список задач",
                   params = list(subject = "Ответ модели Ollama",
                                 message = str_glue("Вот ответ модели:\n\n{result}\n\n---\n\n### Промпт\n\n{ollama_message}")),
                   path_to_tasks = path_to_tasks)
  }

  logger::log_debug("🦦  Завершение запуска умения `ask_ollama`")
}
