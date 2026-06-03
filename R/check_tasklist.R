#' Check the task list and run all tasks
#'
#' @param path_to_tasks path to tasks
#' @param log_message message for adding to logs
#'
#' @importFrom logger log_debug
#' @importFrom logger log_info
#' @importFrom logger log_error
#' @importFrom readr read_csv
#' @importFrom readr write_csv
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr n
#' @importFrom dplyr if_else
#' @importFrom purrr walk
#'
#' @export

check_tasklist <- function(path_to_tasks = getOption("otteRagent_path_to_tasks"),
                           log_message = "Начало сессии. Читаю список задач"){

  logger::log_debug("📋️  Запуск умения `check_tasklist`")

  # проверка параметров -----------------------------------------------------

  logger::log_debug("📋️  проверка параметров")

  if(file.exists(path_to_tasks)){
    logger::log_debug("📋️  файл с задачами существует")
  } else {
    logger::log_error("📋️  нет файла с задачами")
    stop()
  }

  readr::read_csv(path_to_tasks,
                  show_col_types = FALSE,
                  progress = FALSE,
                  n_max = 0,
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
    logger::log_debug("📋️  в файле с задачами правильные колонки")
  } else {
    logger::log_error("📋️  в файле с задачами нет колонки {absent_colnames}")
    stop()
  }

  # начало работы функции ---------------------------------------------------

  logger::log_info("📋️ {log_message}")

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
    dplyr::filter(is.na(ignore)) |>
    dplyr::mutate(schedule = dplyr::if_else(is.na(schedule), "", schedule)) ->
    tasks

  if(sum(duplicated(tasks$id)) > 0) {
    log_info("📋️  Обнаружены повторяющиеся индексы, переиндексирую список задач")

    path_to_tasks |>
      readr::read_csv(show_col_types = FALSE,
                      progress = FALSE) |>
      dplyr::mutate(id = 1:n()) |>
      readr::write_csv(file = path_to_tasks, na = "")

    path_to_tasks |>
      dplyr::read_csv(show_col_types = FALSE,
                      progress = FALSE) |>
      dplyr::filter(is.na(ignore)) ->
      tasks
  }

  n_tasks <- nrow(tasks)

  log_info("📋️  Количество задач в файле: {n_tasks}")

  seq_along(tasks$id) |>
    purrr::walk(function(task_id){

      run_task(task = tasks$task[task_id],
               skill = tasks$skill[task_id],
               schedule = tasks$schedule[task_id],
               params = tasks$params[task_id],
               task_id = task_id,
               path_to_tasks = path_to_tasks)
    })

  log_info("📋  Задачи выполнены, переиндексирую список задач")

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
    nrow() ->
    n_tasks

  if(n_tasks > 0) {
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
      dplyr::mutate(id = 1:dplyr::n()) |>
      readr::write_csv(file = path_to_tasks, na = "")
  }

  log_info("🏁  Конец сессии.")
  logger::log_debug("📋️  Завершение запуска умения `check_tasklist`")
}
