gg_lumigram <- function(dataset,
                       Variable.colname = MEDI,
                       Datetime.colname = Datetime,
                       unit = "1 hour",
                       doubleplot = c("no", "same", "next"),
                       date.title = "Date",
                       date.breaks = 1,
                       date.labels = "%d/%m",
                       # date.height = 1.5*3600,
                       time.title = "Local time (HH:MM)",
                       time.breaks = hms::hms(hours = seq(0, 48, by = 6)),
                       time.labels = "%H:%M",
                       fill.title = "Illuminance\n(lx, mel EDI)",
                       ...
){
  
  doubleplot <- match.arg(doubleplot)
  
  limits <- c(0,24*3600)
  
  
  
  stopifnot(
    "The given dataset is not a dataframe" = is.data.frame(dataset),
    "The given column for X is not in the Dataset. If you did not specify X, you are working with data not originating from LightLogR. Please specify an appropriate Datetime column" = 
      colname.defused({{ Datetime.colname }}) %in% names(dataset),
    "The given column for X is not a Datetime" =
      lubridate::is.POSIXct(dataset[[colname.defused({{ Datetime.colname }})]]),
    "The X axis label must be a string" = is.character(time.title),
    "The Y axis label must be a string" = is.character(date.title)
  )
  
  dat <- 
    dataset |> 
    dplyr::mutate(max_date = lubridate::date(max({{Datetime.colname}})), .before = 1) |> 
    aggregate_Datetime(unit = unit,
                       max_date = dplyr::first(max_date) + 1) |> 
    dplyr::mutate(date = lubridate::date({{ Datetime.colname }})) |>
    dplyr::filter(date < max_date) |>
    add_Time_col({{ Datetime.colname }})
  
  dat <- 
    dat |> 
    dplyr::group_by(date, .add = TRUE) |> 
    dplyr::mutate(
                  .metric = {{ Variable.colname }} / max({{ Variable.colname }}),
                  .metric = .metric*0.95
                  )
  
  dat2 <- NULL
  
  if(doubleplot != "no"){
    dat2 <- 
      dat |> dplyr::mutate(
        Time = Time + 24*3600)
    
    if(doubleplot == "next") {
      dat2 <- 
        dat2 |> dplyr::mutate(
          date = date -1) |> 
        dplyr::filter(date > min(date))
    }
    
    limits <- c(0,48*3600)
  }
  
  dat <- 
    rbind(dat,dat2)
  
  plot <- 
    dat |> 
    ggplot2::ggplot(ggplot2::aes(x = Time, y = (date))) +
    ggplot2::geom_raster(ggplot2::aes(fill = {{ Variable.colname }}),
                         hjust = 1,...) +
    ggplot2::scale_x_time(breaks = time.breaks, 
                          labels = scales::label_time(format = time.labels),
                          expand = c(0,0),
                          limits = limits) +
    ggplot2::facet_wrap(dplyr::group_vars(dat), strip.position = "left", scales = "free_y") + 
    ggplot2::scale_y_continuous(
      transform = c("date", "reverse2"),
      breaks = \(x) seq(from = x[1]-0.5, to = x[2]-0.5, by = -date.breaks),
      labels = scales::label_date(format = date.labels)
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title = fill.title,
      label.position = "left",
      label.hjust = 1, 
    )) +
    ggplot2::labs(y = date.title, x = time.title) +
    cowplot::theme_cowplot()+
    ggplot2::theme(
      plot.title.position = "plot",
      panel.grid.major.y = ggplot2::element_line("grey95"),
      panel.grid.major.x = 
        ggplot2::element_line(colour = "grey", linewidth = 0.25),
      strip.text.y = ggplot2::element_text(face = "bold",),
      strip.placement = "outside",
      plot.margin = ggplot2::margin(10, 20, 10, 10, "pt")
    ) + 
    ggplot2::theme(
      # panel.background = ggplot2::element_rect(fill = NA),
      # panel.ontop = TRUE,
      panel.grid.major.y = ggplot2::element_blank(),
      # panel.grid.major.x = ggplot2::element_line(colour = "black", linewidth = 0.5, linetype = "dashed")
    )
  
  plot
}
