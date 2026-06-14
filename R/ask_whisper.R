#' Run a whisper model on audio and send the result via `gmail`
#'
#' @param audio name of the audio file that is located in the otteRagent directory in the \code{downloads} subfolder.
#' @param output_name name of the output file.
#' @param model_name Specifies a whisper model to call.
#' @param model_path Path to the model.
#' @param language Two-symbol code of the language
#' @param sent_via_gmail logial, whether sent via gmail.
#' @param gmail_subject subject of the mail. Useful, when you have multiple calls to differentiate them from each other.
#' @param create_TextGrid logical, whether the TextGrid should be created and then sent.
#' @param log_message message for adding to logs
#' @param path_to_tasks path to tasks
#'
#' @importFrom logger log_debug
#' @importFrom logger log_error
#' @importFrom logger log_info
#' @importFrom stringr str_glue
#' @importFrom stringr str_c
#' @importFrom stringr str_squish
#' @importFrom av av_media_info
#' @importFrom av av_audio_convert
#' @importFrom audio.whisper whisper
#' @importFrom tools file_ext
#' @importFrom readr read_csv
#' @importFrom curl has_internet
#'
#' @export

ask_whisper <- function(audio,
                        output_name = "output",
                        model_path = getOption("WHISPER_MODEL_DIR"),
                        model_name = "large-v2-q8_0",
                        language = "ru",
                        sent_via_gmail = FALSE,
                        gmail_subject = "Ответ модели whisper",
                        create_TextGrid = FALSE,
                        log_message = "Делаю запрос модели whisper",
                        path_to_tasks = stringr::str_c(getOption("otteRagent_directory"), "tasks.csv")){

  logger::log_debug("🦦  Запуск умения `ask_whisper`")

  # проверка параметров -----------------------------------------------------

  getOption("otteRagent_directory") |>
    stringr::str_c("downloads/", audio) ->
    audio_file_path

  audio_file_path |>
    file.exists() ->
    audio_file_exists

  if(audio_file_exists){
    logger::log_debug("🗣  файл {audio} существует")
  } else {
    logger::log_error("🗣  не нашел файла {audio}")
    stop()
  }

  stringr::str_c(model_path, "ggml-", model_name, ".bin") |>
    file.exists() ->
    model_exists

  if(model_exists){
    logger::log_debug("🗣  модель {model_name} существует")
  } else {
    logger::log_error("🗣  не нашел модели {model_name}")
    stop()
  }

  if(exists("output_name")){
    logger::log_debug("🗣  параметр `output_name` есть")
  } else {
    logger::log_error("🗣  не заполнен параметр `output_name`")
    stop()
  }

  if(exists("model_path")){
    logger::log_debug("🗣  параметр `model_path` есть")
  } else {
    logger::log_error("🗣  не заполнен параметр `model_path`")
    stop()
  }

  if(exists("model_path")){
    logger::log_debug("🗣  параметр `model_name` есть")
  } else {
    logger::log_error("🗣  не заполнен параметр `model_name`")
    stop()
  }

  if(exists("language")){
    logger::log_debug("🗣  параметр `language` есть")
  } else {
    logger::log_error("🗣  не заполнен параметр `language`")
    stop()
  }

  if(exists("create_TextGrid")){
    logger::log_debug("🗣  параметр `create_TextGrid` есть")
  } else {
    logger::log_error("🗣  не заполнен параметр `create_TextGrid`")
    stop()
  }

  if(exists("sent_via_gmail")){
    logger::log_debug("🗣  параметр `sent_via_gmail` есть")
  } else {
    logger::log_error("🗣  не заполнен параметр `sent_via_gmail`")
    stop()
  }

  if(exists("gmail_subject")){
    logger::log_debug("🗣  параметр `gmail_subject` есть")
  } else {
    logger::log_error("🗣  не заполнен параметр `gmail_subject`")
    stop()
  }

  if(exists("log_message")){
    logger::log_debug("🗣  параметр `log_message` есть")
  } else {
    logger::log_error("🗣  не заполнен параметр `log_message`")
    stop()
  }

  # вызов функции -----------------------------------------------------------
  logger::log_info("🗣  {log_message}")

  media_info <- av::av_media_info(audio_file_path)

  bitrate <- media_info$audio$bitrate == "16000"
  extension <- tools::file_ext(audio_file_path) %in% c("WAV", "wav")

  if(sum(bitrate, extension) < 2){
    tmp <- tempdir()
    av::av_audio_convert(audio = audio_file_path,
                         output = stringr::str_c(tmp, "/", output_name, ".wav"),
                         format = "wav",
                         sample_rate = 16000,
                         verbose = FALSE)

    audio_file_path <- stringr::str_c(tmp, "/", output_name, ".wav")
  }

  on.exit(file.remove(audio_file_path))

  audio.whisper::whisper(x = model_name,
                         model_dir = model_path,
                         trace = FALSE) |>
    audio.whisper:::predict.whisper(newdata = audio_file_path,
                                    language = language,
                                    trace = FALSE) ->
    transcription_result

  transcription_result$data |>
    as.data.frame() |>
    mutate(text = stringr::str_squish(text)) |>
    readr::write_csv(file = stringr::str_glue("{output_name}.csv"),
                     na = "")
  print(transcription_result$timing)

  # отправка результата на почту --------------------------------------------

  if(sent_via_gmail){
    if(curl::has_internet()){
      sent_gmail_message(log_message = "Отправляю письмо с ответом whisper",
                         subject = gmail_subject,
                         message = str_glue("Результат транскрипции {audio}."),
                         attach_file = stringr::str_glue("{output_name}.csv"))
    } else {
      logger::log_warn("🦦  Нет интернет соединения, так что я не отправил ответа модели")
      add_to_backlog(task = "Отправить письмо с ответом модели",
                     skill = "sent_gmail_message",
                     schedule = "once",
                     immediate_execute = TRUE,
                     log_message = "Добавляю отправку письма с ответом модели в список задач",
                     params = list(subject = gmail_subject,
                                   message = str_glue("Результат транскрипции {audio}."),
                                   path_to_tasks = path_to_tasks,
                                   attach_file = stringr::str_glue("{output_name}.csv")))
    }
  }
  logger::log_debug("🦦  Завершение запуска умения `ask_whisper`")
}
