#' Run Ollama task
#'
#' Post a messages to ollama, check whether they will fit the model's context length the and send the result via `gmail`.
#'
#' @param ollama_model Specifies an Ollama model to call.
#' @param ollama_message Text of the add_prompt.
#' @param ollama_system_prompt Text that passes as system prompt. Usefull when you have attachment
#' @param ollama_attachment name of the pdf file that is located in the otteRagent directory in the \code{downloads} subfolder.
#' @param remove_from_attachment vector of integers that defines which pages should be excluded from the pdf file (e.g. references and long tables).
#' @param add_prompt logical. Indicates, whether add_prompt should be added to the email.
#' @param result_delivery whether attach result as a \code{file} or as a raw \code{text}.
#' @param gmail_subject subject of the mail. Useful, when you have multiple calls to differentiate them from each other.
#' @param log_message message for adding to logs
#' @param path_to_tasks path to tasks
#'
#' @importFrom logger log_debug
#' @importFrom logger log_error
#' @importFrom logger log_info
#' @importFrom utils installed.packages
#' @importFrom ollamar test_connection
#' @importFrom ollamar model_avail
#' @importFrom purrr map_lgl
#' @importFrom readr write_lines
#' @importFrom purrr map_chr
#' @importFrom purrr pluck
#' @importFrom stringr str_glue
#' @importFrom stringr str_c
#' @importFrom stringr str_remove
#' @importFrom pdftools pdf_info
#' @importFrom pdftools pdf_ocr_text
#' @importFrom curl has_internet
#' @importFrom tokenizers count_words
#' @importFrom tokenizers tokenize_sentences
#'
#' @export

ask_ollama <- function(ollama_message,
                       ollama_model = "gemma4:26b",
                       ollama_system_prompt = "",
                       ollama_attachment = NA,
                       remove_from_attachment = NULL,
                       add_prompt = TRUE,
                       result_delivery = "text",
                       gmail_subject = "Ответ модели Ollama",
                       log_message = "Начинаю подготовку к запросу модели Ollama",
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

  if(exists("result_delivery")){
    if(result_delivery %in% c("text", "file")){
      logger::log_debug("🦙  параметр `result_delivery` существует")
    } else {
      logger::log_error("🦙  параметр `result_delivery` может быть либо `text`, либо `file`.")
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
      stringr::str_c(collapse = "\n\n") |>
      stringr::str_remove("(?<=\\S)-\\s{1,}") ->
      add_to_ollama_message
  } else {
    add_to_ollama_message <- ""
  }

  stringr::str_c(ollama_message, add_to_ollama_message) |>
    tokenizers::count_words() ->
    n_words

  context_length <- gather_context_length_of_the_ollama_model(ollama_model)

  # lets say that the 1 token correspond for every 3/4 of a word then
  # prompt is (n_words / 0.7) and in case of translation output will be (n_words / 0.7)
  # so the context_length should be bigger (n_words / 0.75) * 2

  n_parts <- ceiling(((n_words / 0.7) * 2)/context_length)

  sentences <- tokenizers::tokenize_sentences(add_to_ollama_message)[[1]]

  splits <- vector_splits(length(sentences), n_parts)

  logger::log_info("🦙  Делаю запрос")

  seq_along(splits) |>
    purrr::map_chr(function(i){
      ids <- c(0, splits)
      sentences[(ids[i]+1):ids[i+1]] |>
        stringr::str_c(collapse = " ") |>
        stringr::str_c(ollama_message, ... = _) |>
        ollamar::generate(model = ollama_model,
                          system = ollama_system_prompt) |>
        ollamar::resp_process("text")
    }) |>
    stringr::str_c(collapse = "\n\n") ->
    result

  print(nchar(result))

  if(result_delivery == "text"){
    if(add_prompt){
      mail_message <- stringr::str_glue("Вот ответ модели:\n\n{result}\n\n---\n\n### Промпт\n\n{ollama_message}")
    } else {
      mail_message <- stringr::str_glue("Вот ответ модели:\n\n{result}")
    }
  } else if(result_delivery == "file"){

    getOption("otteRagent_directory") |>
      stringr::str_c("uploads/", ollama_attachment, ".txt") ->
      file_name

    logger::log_info("🦙  Записываю результат в {file_name}")

    readr::write_lines(file = file_name, x = result)
  }

  # отправка результата на почту --------------------------------------------

  if(curl::has_internet()){
    if(result_delivery == "text"){
      sent_gmail_message(log_message = "Отправляю письмо с ответом Ollama",
                         subject = gmail_subject,
                         message = mail_message)
    } else if(result_delivery == "file"){
      sent_gmail_message(log_message = "Отправляю письмо с ответом Ollama",
                         subject = gmail_subject,
                         message = "Вот ответ модели.",
                         attach_file = file_name)
    }
  } else {
    logger::log_warn("🦦  Нет интернет соединения, так что я не отправил ответа модели")

    if(result_delivery == "text"){
      add_to_backlog(task = "Отправить письмо с ответом модели",
                     skill = "sent_gmail_message",
                     schedule = "once",
                     immediate_execute = TRUE,
                     log_message = "Добавляю отправку письма с ответом модели в список задач",
                     params = list(subject = gmail_subject,
                                   message = mail_message),
                     path_to_tasks = path_to_tasks)
    } else if(result_delivery == "file"){

      add_to_backlog(task = "Отправить письмо с ответом модели",
                     skill = "sent_gmail_message",
                     schedule = "once",
                     immediate_execute = TRUE,
                     log_message = "Добавляю отправку письма с ответом модели в список задач",
                     params = list(subject = gmail_subject,
                                   message = "Вот ответ модели.",
                                   attach_file = file_name),
                     path_to_tasks = path_to_tasks)
    }
  }

  logger::log_debug("🦦  Завершение запуска умения `ask_ollama`")
}
