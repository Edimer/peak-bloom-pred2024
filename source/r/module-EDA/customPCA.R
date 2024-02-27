customPCA <- function(data, country_sel) {
  df_pca <-
    data |>
    filter(country == country_sel)
  
  value_q1 <-
    quantile(df_pca$bloom_doy, probs = 0.25, na.rm = TRUE)
  
  value_q2 <-
    quantile(df_pca$bloom_doy, probs = 0.50, na.rm = TRUE)
  
  value_q3 <-
    quantile(df_pca$bloom_doy, probs = 0.75, na.rm = TRUE)
  
  order_categ <-
    c("DOY < Q1",
      "Q1 <= DOY < Q2",
      "Q2 <= DOY < Q3",
      "DOY >= Q3")
  
  df_pca2 <-
    df_pca |>
    group_by(lat, long) |>
    mutate(median_doy = median(bloom_doy, na.rm = TRUE)) |>
    ungroup() |>
    distinct(lat, long, .keep_all = TRUE) |>
    select(-shrubs) |>
    mutate(
      doy_categ = case_when(
        median_doy < value_q1 ~ "DOY < Q1",
        median_doy >= value_q1 &
          median_doy < value_q2 ~ "Q1 <= DOY < Q2",
        median_doy >= value_q2 &
          median_doy < value_q3 ~ "Q2 <= DOY < Q3",
        median_doy >= value_q3 ~ "DOY >= Q3",
      ),
      doy_categ = factor(doy_categ, levels = order_categ)
    ) |>
    select(-c(location, country, bloom_date, bloom_doy, median_doy)) |>
    relocate(doy_categ, everything())
  
  res_pca <- PCA(X = df_pca2,
                 graph = FALSE,
                 quali.sup = 1)
  
  df_pca2$pc1 <- res_pca$ind$coord[, 1]
  df_pca2$pc2 <- res_pca$ind$coord[, 2]
  df_pca2$pc3 <- res_pca$ind$coord[, 3]
  
  eigen_pc1 <- res_pca$eig[, 2][1] |> round(digits = 1)
  eigen_pc2 <- res_pca$eig[, 2][2] |> round(digits = 1)
  eigen_pc3 <- res_pca$eig[, 2][3] |> round(digits = 1)
  
  g1 <-
    df_pca2 |>
    ggplot(aes(x = pc1, y = pc2, color = doy_categ)) +
    geom_point(size = 2.5,
               alpha = 0.75,
               shape = 18) +
    geom_hline(yintercept = 0,
               lty = 2,
               color = "firebrick3") +
    geom_vline(xintercept = 0,
               lty = 2,
               color = "firebrick3") +
    labs(
      x = glue("Component 1 ({eigen_pc1}%)"),
      y = glue("Component 2 ({eigen_pc2}%)")
    ) +
    scale_color_manual(values = c("dodgerblue3", "gray80", "gray80", "forestgreen")) +
    labs(color = "", title = "Component 1 vs Component 2")
  
  g2 <-
    df_pca2 |>
    select(-c(doy_categ)) |>
    correlate(method = "pearson") |>
    filter(term %in% c("pc1", "pc2", "pc3")) |>
    select(-c(pc1, pc2, pc3)) |>
    pivot_longer(-term) |>
    mutate(
      name = str_replace_all(
        name,
        "wildareas-v3-1993-human-footprint",
        "human-footprint93"
      ),
      name = str_replace_all(
        name,
        "wildareas-v3-2009-human-footprint",
        "human-footprint09"
      )
    ) |>
    ggplot(aes(
      x = reorder(name, value),
      y = term,
      fill = value
    )) +
    geom_tile() +
    scale_fill_gradient2(
      low = muted("red"),
      mid = "white",
      high = muted("dodgerblue2"),
      midpoint = 0
    ) +
    labs(x = "", y = "Component", fill = "Correlation") +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 7.5
      ),
      legend.key.size = unit(1, "cm"),
      legend.key.width = unit(2, "cm")
    )
  
  g3 <-
    df_pca2 |>
    ggplot(aes(x = pc1, y = pc3, color = doy_categ)) +
    geom_point(size = 2.5,
               alpha = 0.75,
               shape = 18) +
    geom_hline(yintercept = 0,
               lty = 2,
               color = "firebrick3") +
    geom_vline(xintercept = 0,
               lty = 2,
               color = "firebrick3") +
    labs(
      x = glue("Component 1 ({eigen_pc1}%)"),
      y = glue("Component 3 ({eigen_pc3}%)")
    ) +
    scale_color_manual(values = c("dodgerblue3", "gray80", "gray80", "forestgreen")) +
    labs(color = "", title = "Component 1 vs Component 3")
  
  return(list(
    res1 = g1,
    res2 = g2,
    res3 = g3
  ))
  
}
