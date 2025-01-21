#' Run PAM on ensemble data
#'
#' @export


cluster_solutions <- function(solutions, k = 0, metric = "manhattan") {
  if (inherits(solutions, ensemble)) {
    solutions.m <- matrify_solutions(solutions@solutions, scale = solutions@scale) |>
      dplyr::select(-class, -variableType) |>
      tibble::column_to_rownames("RC") |>
      as.matrix() |>
      t()

    if (k == 0) {
      solutions.m.pamk <- fpc::pamk(solutions.m, diss = F, krange = 2:10, usepam = T)
      k <- solutions.m.pamk$nc
      message(glue::glue("pamk found {k} clusters"))
    }

    solutions@clusters$pam <- cluster::pam(solutions.m, diss = F, k = k, metric = solutions@distance)
    solutions@clusters$k <- k
    return(solutions)
  } else if ("data.frame" %in% class(solutions)) {
    # solutions is the wide tibble
    # if k=0 then it will run pamk to determine k

    solutions.m <- solutions |>
      dplyr::select(-class, -variableType) |>
      tibble::column_to_rownames("RC") |>
      as.matrix() |>
      t()

    if (k == 0) {
      solutions.m.pamk <- fpc::pamk(solutions.m, diss = F, krange = 2:10, usepam = T)
      k <- solutions.m.pamk$nc
      message(glue::glue("pamk found {k} clusters"))
    }

    cluster::pam(solutions.m, diss = F, k = k, metric = metric)
  }
}
