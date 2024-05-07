#library(tidyverse)

json_path = "/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Biofuels_SFA/Computational/ProbAnnotation/k-medioids/SampleFBA.json"

e = ensemble(json_path, scale = T)

# for this dataset only, filter out the solutions
e@solutions = e@solutions |>
  dplyr::filter(model_no > 400 | model_no <= 200)

info(e)

# parse_rc("/Users/kimbrel1/Library/CloudStorage/OneDrive-LLNL/Documents/Biofuels_SFA/Computational/ProbAnnotation/k-medioids/SampleFBA.json",
#          type = "biomass")
#solutions |> dplyr::filter(other_values == 0) |> dplyr::count(type)

#
# e@solutions |> dplyr::count(class)
# e@solutions |> dplyr::count(variableType)


# biomass histogram
plot_biomass(e)




# Ordination
e.m = ordinate_solutions(e,
                        distance = "manhattan",
                        quiet = T)
e.e = ordinate_solutions(e,
                         distance = "euclidean",
                         quiet = T)
plot(e.m@ordination$nmds)
#plot(e.m)

plot(e.e@ordination$nmds)
#plot(e.e)

#e.m@ordination$nmds$distance
e.m@distance
e.e@distance

#e.m@ordination$nmds$stress
stress(e.m)
stress(e.e)


# e.m.df = nmds_to_df(e.m@ordination$nmds)
# e.e.df = nmds_to_df(e.e@ordination$nmds)


# Clustering

e.m = cluster_solutions(e.m, k = 4)
e.m@clusters$k
#e.m@clusters$pam
sil_widths(e.m)
medioids(e.m)
saveRDS(e.m, "/Users/kimbrel1/Github/ensembletools/e_m.rds")
e.m = readRDS("/Users/kimbrel1/Github/ensembletools/e_m.rds")
#plot(e.m)

e.e = cluster_solutions(e.e, k = 4)
e.e@clusters$k
sil_widths(e.e)
medioids(e.e)
saveRDS(e.e, "/Users/kimbrel1/Github/ensembletools/e_e.rds")
e.e = readRDS("/Users/kimbrel1/Github/ensembletools/e_e.rds")
#plot(e.e)

plot_clusters(e.e)


enframe(e.e@clusters$pam$clustering,
        name = "model",
        value = "PAM") |>
  left_join(nmds_to_df(e.e@ordination$nmds), by = "model") |>
  ggplot(aes(x = NMDS1, y = NMDS2, color = as.factor(PAM))) +
    ggplot2::theme_minimal() +
    geom_point()




# silhouette

sil_widths(e.m) |>
  dplyr::mutate(SIL = dplyr::case_when(
    sil_width >= 0.7 ~ "strong",
    sil_width >= 0.5 ~ "good",
    sil_width >= 0.25 ~ "weak",
    sil_width < 0.25 ~ "none"
  )) |>
  dplyr::mutate(SIL = forcats::fct_relevel(SIL, "strong", "good", "weak", "none")) |>
  ggplot2::ggplot(ggplot2::aes(y = reorder(model, sil_width), x = sil_width, fill = SIL)) +
  ggplot2::scale_fill_manual(values = c(
    "strong" = "blue",
    "good" = "darkgreen",
    "weak" = "orange",
    "none" = "red"
  )) +
  ggplot2::geom_area(ggplot2::aes(group = SIL),
                     orientation = "y") +
  ggplot2::facet_wrap(~cluster,
                      scales = "free_y",
                      ncol = 1) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 axis.text.y=ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_blank()) +
  ggplot2::labs(x = "Model", y = "Silhouette Width", fill = "Silhouette")





sil_widths(e.e) |>
  dplyr::mutate(SIL = dplyr::case_when(
    sil_width >= 0.7 ~ "strong",
    sil_width >= 0.5 ~ "good",
    sil_width >= 0.25 ~ "weak",
    sil_width < 0.25 ~ "none"
  )) |>
  dplyr::mutate(SIL = forcats::fct_relevel(SIL, "strong", "good", "weak", "none")) |>
  ggplot2::ggplot(ggplot2::aes(y = reorder(model, sil_width), x = sil_width, fill = SIL)) +
  ggplot2::scale_fill_manual(values = c(
    "strong" = "blue",
    "good" = "darkgreen",
    "weak" = "orange",
    "none" = "red"
  )) +
  ggplot2::geom_area(ggplot2::aes(group = SIL),
                     orientation = "y") +
  ggplot2::facet_wrap(~cluster,
                      scales = "free_y",
                      ncol = 2,
                      strip.position = "left") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom",
                 axis.text.y=ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_blank()) +
  ggplot2::labs(x = "Model", y = "Silhouette Width", fill = "Silhouette")

saveRDS(e.e, file = "e_e.rds")

