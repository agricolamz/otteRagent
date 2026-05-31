#' Get task from the inbox
#'
#' @param from mail address to get the tasks from
#' @param log_message message for adding to logs
#'
#' @importFrom logger log_debug
#' @importFrom logger log_error
#' @importFrom logger log_info
#' @importFrom dplyr filter
#' @importFrom stringr str_c
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom purrr map
#' @importFrom purrr list_rbind
#' @importFrom purrr map_lgl
#' @importFrom gmailr gm_threads
#' @importFrom gmailr gm_modify_thread
#' @importFrom gmailr gm_thread
#' @importFrom gmailr gm_id
#' @importFrom gmailr gm_body
#' @importFrom gmailr gm_from
#' @importFrom gmailr gm_to
#' @importFrom gmailr gm_subject
#' @importFrom tibble tibble
#' @importFrom yaml yaml.load

gather_tasks <- function(from = getOption("otteRagent_preferred_to_mail"),
                         log_message = "Собираю задания из gmail"){

  logger::log_debug("🦦  Запуск умения `gather_tasks`")

  # проверка параметров -----------------------------------------------------

  skills <- c("add_to_backlog")

  if(sum(skills |> purrr::map_lgl(exists)) == length(skills)){
    logger::log_debug("🦦  Все необходимые умения есть.")
  } else {
    logger::log_error("🦦  Один из следующих умений не установлен: {skills}")
    stop()
  }

  if(exists("from")){
    logger::log_debug("🦦  параметр `from` есть")
  } else {
    logger::log_error("🦦  нет параметра `from`")
    stop()
  }

  if(exists("log_message")){
    logger::log_debug("🦦  параметр `log_message` есть")
  } else {
    logger::log_error("🦦  нет параметра `log_message`")
    stop()
  }

  # вызов функции -----------------------------------------------------------
  logger::log_info("🦦  {log_message}")

  my_threads <- gmailr::gm_threads(search = "is:unread")
  prefered_in_mail_cleaned <-  stringr::str_c("<", stringr::str_remove(from, "\\+.*(?=@)"), ">")
  prefered_in_mail_escaped <- stringr::str_replace(from, "\\+", "\\\\\\+")

  seq(1, my_threads[[1]][2]$resultSizeEstimate) |>
    purrr::map(function(i){

      thread <- gmailr::gm_thread(gmailr::gm_id(my_threads)[[i]])

      tibble::tibble(thread_id = thread$id,
                     from = gmailr::gm_from(thread$messages[[1]]),
                     to = gmailr::gm_to(thread$messages[[1]]))
    }) |>
    purrr::list_rbind() |>
    dplyr::filter(stringr::str_detect(from, prefered_in_mail_cleaned),
                  stringr::str_detect(to, prefered_in_mail_escaped)) ->
    result

  result$thread_id |>
    purrr::map(function(thread_id){

      thread <- gmailr::gm_thread(thread_id)

      gmailr::gm_body(thread$messages[[1]]) |>
        unlist() |>
        yaml::yaml.load() ->
        task_params

      if(is.null(task_params$schedule)){
        task_params$schedule <- "once"
      }

      add_to_backlog(task = gmailr::gm_subject(thread$messages[[1]]),
                     skill = task_params$skill,
                     schedule = task_params$schedule,
                     params = task_params$params[[1]],
                     immediate_execute = TRUE)

      gm_modify_thread_fixed(id = thread_id,
                             remove_labels = "UNREAD")
    })

  logger::log_debug("🦦  Завершение запуска умения `gather_tasks`")
}
