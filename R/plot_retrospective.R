plot_retrospective <- function(
  dat,
  end_years=NULL,
  quantity="spawning_biomass",
  show_end_year_points=TRUE,
  group="model",
  ...
){

  # plot_biomass, plot_spawning_biomass, plot_fishing_mortality
  # plot_recruitment_deviations, plot_recruitment, plot_spr
  quantity <- match.arg(quantity, c("biomass", "spawning_biomass", "fishing_mortality", "recruitment", "spr"))

  func_name <- paste0("plot_",quantity)
  if(!(func_name %in% ls(getNamespace("stockplotr")))){
    cli::cli_abort("`quantity` not valid. Should correspond to one of the available stockplotr plotting functions.")
  }

  if(is.null(end_years)) end_years <- rep(0, length(unique(dat$model)))

  if(is.null(names(end_years))){
    cli::cli_alert("`end_years` provided without names. names automatically applied.")
    names(end_years) <- dat$model |> unique()
  }

  retro_dat <- dat |>
    # dplyr::mutate(model=as.character(model)) |>
    dplyr::filter(!is.na(year), !is.na(estimate)) |>
    dplyr::mutate(term_year=max(year)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
        max_yr = term_year+end_years[which(model==names(end_years))]
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(year < max_yr-1) |>
    dplyr::select(-term_year, -max_yr)

  
  func_pars <- list(
    dat = retro_dat, 
    group=group,
    ...
  )

  plt <- do.call(func_name, func_pars)

  if(show_end_year_points){
    # get data from the "geom_line" elements and identfiy terminal year estimate
    term_dat <- plt$layers[[2]]$data |> 
      dplyr::slice_max(order_by=year, n=1, by=model)

    # This is kinda hacky, but would require changing "plot_recruitment"
    # to conform to the "normal" plot data format
    ycol <- switch(quantity,
      "recruitment"="predicted_recruitment",
      "estimate"
    )

    plt <- plt+
      ggplot2::geom_point(
        data=term_dat, 
        ggplot2::aes(
            x=.data[["year"]], 
            y=.data[[ycol]], 
            color={
              if (length(unique(.data[["model"]])) > 1) {
                interaction(model, group_var)
              } else {
                group_var
              }
            }
        ),
        size=3.5
      )
  }

  plt <- plt +
    ggplot2::scale_color_discrete(labels=unique(retro_dat$model))+
    ggplot2::scale_fill_discrete(labels=unique(retro_dat$model))+
    ggplot2::guides(
      color=ggplot2::guide_legend("Peel"), 
      fill=ggplot2::guide_legend("Peel")
    )
  
  plt

}
