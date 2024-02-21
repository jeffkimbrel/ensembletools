#' Plot a histogram of the biomass
#'
#' @export



plot_biomass = function(solutions) {

  # solutions must be class ensemble or a dataframe
  if ("ensemble" %in%class(solutions)) {
    s = solutions@solutions
  } else if ("data.frame" %in% class(solutions)) {
    s = solutions
  } else {
    stop(glue::glue("solutions must be class <ensemble> or a <data.frame>, not <{class(solutions)[1]}>."), call. = FALSE)
  }

  # warning if scaled
  if (isTRUE(solutions@scale)) {
    message("Warning: data is scaled so biomass values are likely not useful")
  }




  bio = s |>
    dplyr::filter(type == "biomass")

  bio |>
    #dplyr::filter(other_values > median(other_values)) |>
    ggplot2::ggplot(ggplot2::aes(x = other_values)) +
    ggplot2::geom_histogram(binwidth = 0.1) +
    ggplot2::geom_vline(xintercept = median(bio$other_values),
                        linetype = "dashed",
                        color = "red")
  #ggplot2::geom_density()

}

