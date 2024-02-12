#' Plot a histogram of the biomass
#'
#' @export



plot_biomass = function(solutions) {
  bio = solutions |>
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

