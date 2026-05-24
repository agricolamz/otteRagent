#' Run the task
#'
#' @param task Task name
#' @param skill Task skill
#' @param params parameters for the skill
#' @param schedule If `once` --- removes task after complition
#' @param task_id Unique task identifier
#' @param path_to_tasks path to tasks
#' @param log_message message for adding to logs
#'
#' @importFrom logger log_debug
#' @importFrom logger log_info
#' @importFrom logger log_error
#' @importFrom logger log_warn
#' @importFrom readr read_csv
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom readr write_csv
#' @importFrom stringr str_glue
#' @importFrom yaml read_yaml
#' @importFrom curl has_internet
#'
#' @export

run_task <- function(task,
                     skill,
                     params,
                     schedule,
                     task_id,
                     path_to_tasks = getOption("otteRagent_path_to_tasks"),
                     log_message = stringr::str_glue("{task} начинается, запускаю умение {skill}")){

  logger::log_debug("🦦  Запуск умения `run_task`")

  # проверка параметров -----------------------------------------------------

  logger::log_debug("🦦  проверка параметров")

  if(exists("task")){
    logger::log_debug("🦦  параметр `task` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `task`")
    stop()
  }

  if(exists("skill")){
    logger::log_debug("🦦  параметр `skill` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `skill`")
    stop()
  }

  if(exists("params")){
    logger::log_debug("🦦  параметр `params` есть")
  } else {
    logger::log_error("🦦  нет параметра `params`")
    stop()
  }

  if(exists("log_message")){
    logger::log_debug("🦦  параметр `log_message` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `log_message`")
    stop()
  }

  if(exists("task_id")){
    logger::log_debug("🦦  параметр `task_id` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `task_id`")
    stop()
  }

  if(exists("schedule")){
    logger::log_debug("🦦  параметр `schedule` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `schedule`")
    stop()
  }

  if(exists("path_to_tasks")){
    logger::log_debug("🦦  параметр `path_to_tasks` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `path_to_tasks`")
    stop()
  }

  if(exists(skill)){
    logger::log_debug("🦦  Умение {skill} есть.")
  } else {
    logger::log_error("🦦  Нет умения {skill}.")
    logger::log_info("🦦  Меняю статус задачи {task} на `ignore`.")

    path_to_tasks |>
      readr::read_csv(show_col_types = FALSE,
                      progress = FALSE,
                      col_types = list(
                        id = "d",
                        task = "c",
                        skill = "c",
                        schedule = "c",
                        ignore = "c",
                        params = "c")) |>
      dplyr::mutate(ignore = dplyr::if_else(id == task_id, "ignore", ignore)) |>
      readr::write_csv(file = path_to_tasks, na = "")

    if(curl::has_internet()){
      sent_gmail_message(subject = "Нет умения для задачи",
                         message = str_glue("Я не нашел умения {skill} для задачи {task} и поменял ее статус на `ignore`."),
                         log_message = "Отправляю письмо на gmail с сообщением об ошибке")
    } else {
      logger::log_warn("🦦  Интернета нет, так что я не сообщил о проблеме")
      logger::log_info("🦦  Добавляю отправку письма с сообщением о проблеме в список задач")
      add_to_backlog(task = "Отправить письмо о проблеме",
                     skill = "sent_gmail_message",
                     schedule = "once",
                     log_message = "Отправляю письмо на gmail с сообщением об ошибке",
                     params = list(subject = "Нет умения для задачи",
                                   message = str_glue("Я не нашел умения {skill} для задачи {task} и поменял ее статус на `ignore`.")),
                     path_to_tasks = path_to_tasks)
    }
  }

  # начало работы функции ---------------------------------------------------

  logger::log_info("🦦  {log_message}")

  if(!is.list(params)){
    params |>
      yaml::read_yaml(text = _) ->
      params
  }

  do.call(what = skill, args = params)

  log_info("🦦  Задача {task} завершена.")

  if(schedule == "once"){
    logger::log_info("🗑   Удаляю задачу {task} из списка.")

    path_to_tasks |>
      readr::read_csv(show_col_types = FALSE,
                      progress = FALSE,
                      col_types = list(
                        id = "d",
                        task = "c",
                        skill = "c",
                        schedule = "c",
                        ignore = "c",
                        params = "c")) |>
      dplyr::filter(id != task_id) |>
      readr::write_csv(file = path_to_tasks, na = "")
  }

  logger::log_debug("🦦  Завершение запуска умения `run_task`")
}
