#' Run PAM on ensemble data
#'
#' @export


cluster_solutions = function(data, k = 0, metric = "manhattan") {
  # data is the wide tibble
  # if k=0 then it will run pamk to determine k

  data.m = data %>%
    dplyr::select(-class, -variableType) %>%
    tibble::column_to_rownames("RC") %>%
    as.matrix() %>%
    t()

  if (k == 0) {
    data.m.pamk = fpc::pamk(data.m, diss=F, krange=2:10, usepam = T)
    k = data.m.pamk$nc
    message(glue::glue("pamk found {k} clusters"))
  }

  cluster::pam(data.m, diss=F, k = k, metric = metric)

}
