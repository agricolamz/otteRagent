#' Add task to the current task list
#'
#' @param task Task name
#' @param skill Task skill
#' @param schedule If `once` --- removes task after completion
#' @param ignore If `ignore` --- task will be kept unsolved in the task list.
#' @param params parameters for the skill
#' @param path_to_tasks path to tasks
#' @param immediate_execute logical. States whether the task should be executed.
#' @param log_message message for adding to logs
#'
#' @importFrom logger log_debug
#' @importFrom logger log_info
#' @importFrom logger log_error
#' @importFrom yaml as.yaml
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom dplyr bind_rows
#' @importFrom dplyr pull
#' @importFrom tibble tibble
#'
#' @export

add_to_backlog <- function(task = "новое задание",
                           skill,
                           schedule = "once",
                           ignore = NA,
                           params = list(),
                           path_to_tasks = getOption("otteRagent_path_to_tasks"),
                           immediate_execute = FALSE,
                           log_message = "Добавляю задание в список задач"){

  logger::log_debug("🦦  Запуск умения `add_to_backlog`")

  # проверка параметров -----------------------------------------------------

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

  if(exists("schedule")){
    logger::log_debug("🦦  параметр `schedule` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `schedule`")
    stop()
  }

  if(exists("ignore")){
    logger::log_debug("🦦  параметр `ignore` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `ignore`")
    stop()
  }

  if(exists("params")){
    logger::log_debug("🦦  параметр `params` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `params`")
    stop()
  }

  if(exists("log_message")){
    logger::log_debug("🦦  параметр `log_message` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `log_message`")
    stop()
  }

  if(exists("immediate_execute")){
    logger::log_debug("🦦  параметр `immediate_execute` есть")
  } else {
    logger::log_error("🦦  не заполнен параметр `immediate_execute`")
    stop()
  }

  if(file.exists(path_to_tasks)){
    logger::log_debug("🦦  файл с заданиями существует")
  } else {
    logger::log_error("🦦  нет файла с заданиями")
    stop()
  }

  readr::read_csv(path_to_tasks,
                  show_col_types = FALSE,
                  progress = FALSE,
                  col_types = list(
                    id = "d",
                    task = "c",
                    skill = "c",
                    schedule = "c",
                    ignore = "c",
                    params = "c")) |>
    colnames() ->
    task_colnames

  expected_colnames <- c("id", "task", "skill", "schedule", "ignore", "params")

  absent_colnames <- expected_colnames[which(!(expected_colnames %in% task_colnames))]

  if(length(absent_colnames) == 0){
    logger::log_debug("🦦  в файле с заданиями правильные колонки")
  } else {
    logger::log_error("🦦  в файле с заданиями нет колонки {absent_colnames}")
    stop()
  }

  # начало работы функции ---------------------------------------------------

  logger::log_info("🦦  {log_message}")

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
      dplyr::pull(id) |>
      as.double() |>
      sum() ->
      new_id

    new_id <- new_id + 1

    if(immediate_execute){
      run_task(task = task,
               skill = skill,
               schedule = "",
               params = params,
               task_id = new_id)
    }

    if ((isTRUE(immediate_execute) & schedule != "once") | isFALSE(immediate_execute)){
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
        dplyr::bind_rows(
          tibble::tibble(id = new_id,
                         task = task,
                         skill = skill,
                         schedule = schedule,
                         ignore = ignore,
                         params = params |> yaml::as.yaml())) |>
        readr::write_csv(file = path_to_tasks, na = "")
    }

  logger::log_debug("🦦  Завершение запуска умения `add_to_backlog`")
}
