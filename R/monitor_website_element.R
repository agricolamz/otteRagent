#' Monitor changes in website
#'
#' @param url url
#' @param website_element css path, e. g. "h2 > a"
#' @param log_message message for adding to logs
#' @param path_to_tasks path to tasks
#'
#' @importFrom logger log_debug
#' @importFrom logger log_info
#' @importFrom logger log_error
#' @importFrom rvest read_html
#' @importFrom rvest html_elements
#' @importFrom rvest html_text
#' @importFrom rvest html_attr
#' @importFrom stringr str_c
#' @importFrom stringr str_glue_data
#' @importFrom purrr map_lgl
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom dplyr anti_join
#' @importFrom dplyr bind_rows
#' @importFrom curl has_internet
#'
#' @export

monitor_website_element <- function(url,
                                    website_element = "h2 > a",
                                    log_message = stringr::str_glue("Ищу изменения на вебсайте: {url}"),
                                    path_to_tasks = stringr::str_c(getOption("otteRagent_directory"), "tasks.csv")){

  logger::log_debug("🌐️  Запуск умения `monitor_website_element`")

  # проверка параметров -----------------------------------------------------

  skills <- c("add_to_backlog")

  if(sum(skills |> purrr::map_lgl(exists)) == length(skills)){
    logger::log_debug("🦦  Все необходимые умения есть.")
  } else {
    logger::log_error("🦦  Один из следующих умений не установлен: {skills}")
    stop()
  }

  if(exists("url")){
    logger::log_debug("🌐  параметр `url` есть")
  } else {
    logger::log_error("🌐  нет параметра `url`")
    stop()
  }

  if(exists("website_element")){
    logger::log_debug("🌐  параметр `website_element` есть")
  } else {
    logger::log_error("🌐  нет параметра `website_element`")
    stop()
  }

  if(exists("log_message")){
    logger::log_debug("🌐  параметр `log_message` есть")
  } else {
    logger::log_error("🌐  нет параметра `log_message`")
    stop()
  }

  if(exists("path_to_tasks")){
    logger::log_debug("🌐  параметр `path_to_tasks` есть")
  } else {
    logger::log_error("🌐  нет параметра `path_to_tasks`")
    stop()
  }

  # начало работы функции ---------------------------------------------------

  logger::log_info("🌐  {log_message}")

  getOption("otteRagent_directory") |>
    stringr::str_c("logs/website_monitoring_logs.csv") ->
    website_monitoring_logs_path

  if(file.exists(website_monitoring_logs_path)){
    website_monitoring_logs_path |>
      readr::read_csv(show_col_types = FALSE,
                      progress = FALSE,
                      col_types = list(
                        title = "c",
                        link = "c",
                        website_element = "c")) ->
      website_monitoring_logs
  } else {
    tibble(title = character(),
           link = character(),
           website_element = character()) ->
      website_monitoring_logs
  }

  if(curl::has_internet()){
    rvest::read_html(url) |>
      rvest::html_elements(website_element) ->
      h2

    tibble::tibble(title = h2 |> rvest::html_text() |> stringr::str_squish(),
                   link = h2 |> rvest::html_attr("href"),
                   website_element = website_element) ->
      website_monitoring_results

    website_monitoring_results |>
      dplyr::anti_join(website_monitoring_logs) ->
      detected_changes

    if(nrow(detected_changes) > 0){

      detected_changes |>
        stringr::str_glue_data("
---

- {title}
- {link}
") |>
        stringr::str_c(collapse = "\n\n") ->
        message

      add_to_backlog(task = "Отправить письмо с результатами мониторинга сайта",
                     skill = "sent_gmail_message",
                     schedule = "once",
                     immediate_execute = TRUE,
                     log_message = "Добавляю отправку письма с изменениями на странице в список задач",
                     params = list(subject = "Обнаружены изменения на сайте",
                                   message = message),
                     path_to_tasks = path_to_tasks)

      website_monitoring_logs |>
        dplyr::bind_rows(detected_changes) |>
        readr::write_csv(website_monitoring_logs_path, na = "")

    } else {
      logger::log_info("🌐  Изменений на сайте {url} не найдено.")
    }
  } else {
    logger::log_info("🌐  Нет интернет соединения")
  }

  logger::log_debug("🌐  Завершение запуска умения `monitor_website_element`")
}
