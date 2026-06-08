#' Post a messages to Ollama and send the result via `gmail`
#'
#' @param ollama_model Specifies an Ollama model to call.
#' @param ollama_message Text of the add_prompt.
#' @param ollama_attachment name of the pdf file that is located in the otteRagent directory in the \code{downloads} subfolder.
#' @param remove_from_attachment vector of integers that defines which pages should be excluded from the pdf file (e.g. references and long tables).
#' @param add_prompt logical. Indicates, whether add_prompt should be added to the email.
#' @param gmail_subject subject of the mail. Useful, when you have multiple calls to differentiate them from each other.
#' @param log_message message for adding to logs
#' @param path_to_tasks path to tasks
#'
#' @importFrom logger log_debug
#' @importFrom logger log_error
#' @importFrom utils installed.packages
#' @importFrom ollamar test_connection
#' @importFrom ollamar model_avail
#' @importFrom purrr map_lgl
#' @importFrom purrr pluck
#' @importFrom stringr str_glue
#' @importFrom stringr str_c
#' @importFrom pdftools pdf_info
#' @importFrom pdftools pdf_ocr_text
#'
#' @export

ask_ollama <- function(ollama_model = "gemma4:26b",
                       ollama_message,
                       ollama_attachment = NA,
                       remove_from_attachment = NULL,
                       add_prompt = TRUE,
                       gmail_subject = "Ответ модели Ollama",
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

  if(exists("gmail_subject")){
    logger::log_debug("🦙  параметр `gmail_subject` есть")
  } else {
    logger::log_error("🦙  не заполнен параметр `gmail_subject`")
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

  if(!is.na(ollama_attachment)){
    getOption("otteRagent_directory") |>
      stringr::str_c("downloads/", ollama_attachment) |>
      file.exists() ->
      ollama_attachment_file_exists

    if(ollama_attachment_file_exists){
      logger::log_debug("🦙  файл {ollama_attachment} существует")
    } else {
      logger::log_error("🦙  не нашел файла {ollama_attachment}")
      stop()
    }
  }

  # if(length(remove_from_attachment) > 0){
  #   if(!is.na(ollama_attachment)){
  #     if(is.integer(remove_from_attachment)){
  #       logger::log_debug("🦙  в аргументе `remove_from_attachment` вектор чисел")
  #     } else {
  #       logger::log_error("🦙  в аргументе `remove_from_attachment` должен быть вектор чисел")
  #       stop()
  #     }
  #   } else {
  #     logger::log_error("🦙  аргумент `remove_from_attachment` есть, а аргумент `ollama_attachment` не заполнен")
  #     stop()
  #   }
  # }

  if(ollamar::test_connection(logical = TRUE)){
    logger::log_debug("🦙  Ollama запущена")
  } else {
    logger::log_error("🦙  Ollama не запущена")
    stop()
  }

  ollamar::list_models() |>
    dplyr::pull(name) ->
    ollama_available_models

  if(ollama_model %in% ollama_available_models){
    logger::log_debug("🦙  Модель {ollama_model} есть в списке доступных моделей")
  } else {
    logger::log_error("🦙  Модели {ollama_model} нет в списке доступных моделей")
    stop()
  }

  if(exists("add_prompt")){
    if(is.logical(add_prompt)){
      logger::log_debug("🦙  параметр `add_prompt` существует и он логический")
    } else {
      logger::log_error("🦙  параметр `add_prompt` должен быть логическим")
      stop()
    }
  }


  # вызов функции -----------------------------------------------------------
  logger::log_info("🦙  {log_message}")

  if(!is.na(ollama_attachment)){

    logger::log_info("🦙  Начинаю обработку pdf-файла")

    stringr::str_c(getOption("otteRagent_directory"),
                   "downloads/",
                   ollama_attachment) |>
    pdftools::pdf_info() |>
    purrr::pluck("pages") |>
    seq_len() ->
    pages

    if(length(remove_from_attachment) > 0){
      if(is.character(remove_from_attachment)){
        parse(text = remove_from_attachment) |>
          eval() ->
          remove_from_attachment
      }
      pages <- pages[!(pages %in% remove_from_attachment)]
    }

    getOption("otteRagent_directory") |>
      stringr::str_c("downloads/", ollama_attachment) |>
      pdftools::pdf_ocr_text(pages = pages) |>
      stringr::str_c(collapse = "\n\n") ->
      ollama_message
  }

  ollama_message |>
    ollamar::generate(model = ollama_model) |>
    ollamar::resp_process("text") ->
    result

  if(add_prompt){
    mail_message <- stringr::str_glue("Вот ответ модели:\n\n{result}\n\n---\n\n### Промпт\n\n{ollama_message}")
  } else {
    mail_message <- stringr::str_glue("Вот ответ модели:\n\n{result}\n\n")
  }

  # отправка результата на почту --------------------------------------------

  if(curl::has_internet()){
    sent_gmail_message(log_message = "Отправляю письмо с ответом Ollama",
                       subject = gmail_subject,
                       message = mail_message)
  } else {
    logger::log_warn("🦦  Нет интернет соединения, так что я не отправил ответа модели")
    add_to_backlog(task = "Отправить письмо с ответом модели",
                   skill = "sent_gmail_message",
                   schedule = "once",
                   immediate_execute = TRUE,
                   log_message = "Добавляю отправку письма с ответом модели в список задач",
                   params = list(subject = gmail_subject,
                                 message = mail_message),
                   path_to_tasks = path_to_tasks)
  }

  logger::log_debug("🦦  Завершение запуска умения `ask_ollama`")
}
