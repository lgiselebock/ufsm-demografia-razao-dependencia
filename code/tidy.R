

options(scipen = 9999)

# PACOTES NECESSARIOS -----------------------------------------------------

library(ggplot2)


# IMPORT BASE -------------------------------------------------------------

razao_dependencia_tab <- readr::read_csv(
  here::here("data", "razao-dependencia.csv")
)

razao_dependencia

razao_dependencia <- c("RDT", "RDI", "RDJ")
x2000 <- c((9935100+50266122)/109597948, (9935100/109597948), (50266122/109597948))
x2010 <- c((14081477+45932294)/130742028, (14081477/130742028), (45932294/130742028))
x2020 <- c((20813349+44186100)/146756243, (20813349/146756243), (44186100/146756243))
x2030 <- c((30448865+42625016)/151794581, (30448865/151794581), (42625016/151794581))
x2040 <- c((40368048+38964325)/152587549, (40368048/152587549), (38964325/152587549))
x2050 <- c((50932665+35976487)/146024124, (50932665/146024124), (35976487/146024124))
x2060 <- c((58181930+33597781)/136506636, (58181930/136506636), (33597781/136506636))

razao_dependencia_calculada <- tibble::tibble(razao_dependencia,
                                              x2000,
                                              x2010,
                                              x2020,
                                              x2030,
                                              x2040,
                                              x2050,
                                              x2060) |>
  tidyr::pivot_longer(
    cols = x2000:x2060
  ) |>
  dplyr::mutate(
    year = dplyr::case_when(
      name == "x2000" ~ "2000",
      name == "x2010" ~ "2010",
      name == "x2020" ~ "2020",
      name == "x2030" ~ "2030",
      name == "x2040" ~ "2040",
      name == "x2050" ~ "2050",
      name == "x2060" ~ "2060"
    ),
    razao_dependencia = dplyr::case_when(
      razao_dependencia == "RDT" ~ "RD Total",
      razao_dependencia == "RDI" ~ "RD Idosos",
      razao_dependencia == "RDJ" ~ "RD Jovens"
    )
  )


palette <- c("#005c8b", "#e74645", "#006600")

razao_dependencia_calculada |>
  ggplot() +
  aes(x = year, y = value, group = razao_dependencia) +
  geom_line(aes(color = razao_dependencia), linewidth = 1.4) +
  scale_y_continuous(
    limits = c(0, 0.70),
    breaks = seq(0, 0.70, 0.10),
    labels = scales::percent
  ) +
  scale_color_manual(
    values = palette,
    guide = guide_legend(
      title = ""
    )
  ) +
  labs(
    x = "",
    y = "",
    caption = "Fonte: IBGE (2022)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)
  )



palette_alpha <- c("#b6c5d7", "#ffc5bd", "#b2cba9")


razao_dependencia_tab |>
  dplyr::rename(
    "populacao" = "id",
    "x2000" = "n_00",
    "x2010" = "n_10",
    "x2020" = "n_20",
    "x2030" = "n_30",
    "x2040" = "n_40",
    "x2050" = "n_50",
    "x2060" = "n_60"
  ) |>
  tidyr::pivot_longer(
    cols = x2000:x2060
  ) |>
  dplyr::mutate(
    year = dplyr::case_when(
      name == "x2000" ~ "2000",
      name == "x2010" ~ "2010",
      name == "x2020" ~ "2020",
      name == "x2030" ~ "2030",
      name == "x2040" ~ "2040",
      name == "x2050" ~ "2050",
      name == "x2060" ~ "2060"
    ),
    value = (value/1000000),
    populacao = dplyr::case_when(
      populacao == "jovem" ~ "criança",
      TRUE ~ populacao
    ),
    populacao = factor(populacao, levels = c("idoso", "adulto", "criança"))
  ) |>
  ggplot() +
  aes(
    x = year,
    y = value,
    group = populacao
  ) +
  geom_bar(aes(fill = populacao), stat = "identity") +
  geom_text(
    aes(
      label = round(value, 2)
    ),
    position = position_stack(vjust = 0.5)
  ) +
  scale_y_continuous(
    limits = c(0, 250),
    breaks = seq(0, 250, 50)
  ) +
  scale_fill_manual(
    values = palette_alpha,
    guide = guide_legend(
      title = ""
    )
  ) +
  labs(
    x = "",
    y = "",
    caption = "Fonte: IBGE (2022)",
    title = "Tamanho da população brasileira por faixa etária (em milhões)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 8),
    plot.caption = element_text(hjust = 0.5)
  )







