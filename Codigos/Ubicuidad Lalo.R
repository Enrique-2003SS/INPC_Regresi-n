datos = "outputs/Pivot_INPC_Op1.xlsx" |> 
  readxl::read_excel()


datos = datos |> 
  dplyr::mutate(
    dplyr::across(
      .cols = `002 Botanas elaboradas con cereales`:`221 Metro o transporte eléctrico`,
      .fns = ~ dplyr::if_else(
        condition = is.na(.x),
        true = 0,
        false = .x
      )
    )
  )

interes = c("Area Metropolitana de la Cd. de México", "Pachuca, Hgo.", "Tulancingo, Hgo.")



transponer = datos |>
  tidyr::pivot_longer(cols = -1, names_to = "Generico") |>
  tidyr::pivot_wider(names_from = 1)

ubicuidad = transponer |>
  dplyr::mutate(
    Ubicuidad = rowSums(dplyr::across(-1), na.rm = TRUE)
  ) |> 
  dplyr::select(Generico, Ubicuidad) |> 
  dplyr::arrange(Generico)



filtro = transponer |> 
  dplyr::select(Generico, interes)


ubicuidad_hidalgo = filtro |> 
  dplyr::mutate(
    `Ubicuidad Hidalgo` = `Area Metropolitana de la Cd. de México` + `Pachuca, Hgo.` + `Tulancingo, Hgo.`,
  ) |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    `Ubicuidad Hidalgo Ciudad` = paste(`Area Metropolitana de la Cd. de México`, `Pachuca, Hgo.`, `Tulancingo, Hgo.`, sep = ", ")
  ) |> 
  dplyr::select(Generico, `Ubicuidad Hidalgo`, `Ubicuidad Hidalgo Ciudad`) |> 
  dplyr::mutate(
    `Ubicuidad Hidalgo Ciudad` = dplyr::case_when(
      `Ubicuidad Hidalgo Ciudad` == "1, 0, 0" ~ "Area Metropolitana de la Cd. de México",
      `Ubicuidad Hidalgo Ciudad` == "0, 1, 0" ~ "Pachuca, Hgo.",
      `Ubicuidad Hidalgo Ciudad` == "0, 0, 1" ~ "Tulancingo, Hgo.",
      `Ubicuidad Hidalgo Ciudad` == "0, 0, 0" ~ "Ninguno",
      `Ubicuidad Hidalgo Ciudad` == "1, 1, 1" ~ "Area Metropolitana de la Cd. de México; Pachuca, Hgo.; Tulancingo, Hgo.",
      `Ubicuidad Hidalgo Ciudad` == "1, 0, 1" ~ "Area Metropolitana de la Cd. de México; Tulancingo, Hgo.",
      `Ubicuidad Hidalgo Ciudad` == "1, 1, 0" ~ "Area Metropolitana de la Cd. de México; Pachuca, Hgo.",
      `Ubicuidad Hidalgo Ciudad` == "0, 1, 1" ~ "Pachuca, Hgo.; Tulancingo, Hgo.",
    )
  )



ubicuidad = ubicuidad |> 
  dplyr::left_join(
    y = ubicuidad_hidalgo,
    by = "Generico"
  )

