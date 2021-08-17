#' Isotopic variation on two y-columns.
#' @name zoo_isotops
#'
#' @description Compare two isotopic variations (eg O, Sr) on a two y-axis plots
#'
#' @param dataDir directory of the dataset
#' @param dataFile name of the dataset
#' @param img.title title of the output plot
#' @param iso.names names of the isotopes.By default columns c("O", "Sr")
#' @param iso.colors color of the isotopes. By default c("turquoise", "turquoise4")
#' @param iso.by.y intervals on y-axis. By default c(5, .0002)
#' @param x.by interval on x-axis. By default = 5
#' @param stat.ci show statistics: confidence interval. Default value is TRUE
#' @param img.layout layout for the output. By default c("grid", "align")
#' @param img.dim dimensions for the output, Width and length. By default list(grid = c(18, 16), align = c(13, 21))
#' @param img.units By default "cm"
#' @param col.ERJ x-axis. By default column 'ERJ'
#' @param col.ind group column. By default column 'ind' (individus)
#'   be removed?
#' @return ggplot graphs, in grid layout or aligned vertically.
#'
#' @examples
#' zoo_isotops()
#' 
#' zoo_isotops(img.layout = "align", 
#'             img.format = ".png",
#'             iso.by = c(1, .0002),
#'             x.by = 5)
#' 
#' zoo_isotops(img.layout = "grid",
#'             iso.colors = c("red", "blue"),
#'             stat.ci = FALSE,
#'             img.format = ".png")

library(ggplot2)
library(tibble)
library(ggpubr)
library(grid)
library(gridExtra)



graphic_df <- function(df, 
                       img.title. = img.title,
                       iso.left. = iso.left,
                       iso.right. = iso.right,
                       y.left.color. = y.left.color,
                       y.right.color. = y.right.color,
                       y.left.by. = y.left.by,
                       y.right.by. = y.right.by,
                       x.by. = x.by,
                       stat.ci. = stat.ci,
                       min.left. = min.left,
                       min.right. = min.right,
                       max.left. = max.left,
                       max.right. = max.right,
                       xlim.ERJ. = xlim.ERJ,
                       a. = a, b. = b,
                       flag. = flag){
  # df <- df.select
  # df <- read.table(paste0(dataDir, dataFile), header = TRUE, sep = "\t")
  # iso.left.name <- iso.left
  # iso.right.name <- iso.right
  # min.left.value <- min.left
  # min.right.value <- min.right
  # max.left.value <- max.left
  # max.right.value <- max.right
  df.tb <- tibble(
    ind = df$ind,
    ERJ = df$ERJ,
    left = df[ , iso.left.],
    left.std = runif(length(df[ , iso.left.]), 0, 1),
    right = df[ , iso.right.],
    right.std = runif(length(df[ , iso.right.]), 0, 1)
  )
  if(flag. == "align"){g.title <- img.title.}
  if(flag. == "grid"){g.title <- df.tb$ind}
  img.out <- ggplot(df.tb, aes(ERJ, left)) +
    ggtitle(g.title) +
    # stats
    {if(stat.ci.)geom_ribbon(aes(ymin = left - left.std,
                                 ymax = left + left.std),  
                             fill = y.left.color., alpha = .3)} +
    {if(stat.ci.)geom_ribbon(aes(ymin = a. + right*b. - right.std,
                                 ymax = a. + right*b. + right.std),  
                             fill = y.right.color., alpha = .3)} +
    # y-left
    geom_line(color = y.left.color., size = 0.2) +
    geom_point(col = y.left.color., size = 1) +
    # y-right
    geom_line(aes(y = a. + right*b.),
              color = y.right.color.,
              size = 0.2) +
    geom_point(aes(y = a. + right*b.),
               color = y.right.color.,
               size = 1) +
    scale_y_continuous(name = iso.left.,
                       limits = c(min.left., max.left.),
                       breaks = seq(min.left., max.left., by = y.left.by.),
                       sec.axis = sec_axis(~ (. - a.)/b.,
                                           name = iso.right.,
                                           breaks = seq(min.right., max.right., by = y.right.by.),
                                           labels = scales::number_format(accuracy = 0.0001,
                                                                          decimal.mark = '.'))) +
    scale_x_reverse(breaks = seq(ceiling(min(df.tb$ERJ))-1,
                                 ceiling(max(df.tb$ERJ)),
                                 by = x.by.)) +
    expand_limits(x = c(ceiling(xlim.ERJ.[1])-1, xlim.ERJ.[2])) +
    theme_bw() +
    theme(
      title = element_text(size = 8),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6),
      # left y
      axis.title.y.left = element_text(color = y.left.color., size = 7),
      axis.text.y.left = element_text(color = y.left.color., size = 6),
      # right y
      axis.title.y.right = element_text(color = y.right.color., size = 7),
      axis.text.y.right = element_text(color = y.right.color., size = 6)
    )
  # print("DONE")
  return(img.out)
}

zoo_isotops <- function(dataDir = paste0(getwd(), "/extdata/"),
                        dataFile = "C_O_Sr_etno3.txt",
                        img.title = "Sheep and goat",
                        iso.names = c("O", "Sr"),
                        iso.colors = c("turquoise", "turquoise4"),
                        iso.by.y = c(5, .0002),
                        x.by = 5,
                        stat.ci = TRUE,
                        img.format = ".pdf",
                        img.layout = c("grid", "align"),
                        img.dim = list(grid = c(18, 16), align = c(13, 21)),
                        img.units = "cm",
                        # TODO: include these mandatory columns
                        col.ERJ = "ERJ",
                        col.ind = "ind"){
  iso.left <- iso.names[1]
  iso.right <- iso.names[2]
  y.left.color <- iso.colors[1]
  y.right.color <- iso.colors[2]
  y.left.by <- iso.by.y[1]
  y.right.by <- iso.by.y[2]
  img.width.grid <- img.dim$grid[1]
  img.height.grid <- img.dim$grid[2]
  img.width.align <- img.dim$align[1]
  img.height.align <- img.dim$align[2]
  outFile <- gsub("\\..*", "", dataFile)
  df.iso <- read.table(paste0(dataDir, dataFile), 
                       header = TRUE, 
                       sep = "\t")
  # min/max values for axes
  max.left <- max(df.iso[ , iso.left], na.rm = T) ; min.left <- min(df.iso[ , iso.left], na.rm = T)
  max.right <- max(df.iso[ , iso.right], na.rm = T) ; min.right <- min(df.iso[ , iso.right], na.rm = T)
  
  xlim.ERJ <- c(min(df.iso[ , col.ERJ]), max(df.iso[ , col.ERJ])) 
  ylim.left <- c(min(df.iso[ , iso.left]), max(df.iso[ , iso.left])) 
  ylim.right <- c(min(df.iso[ , iso.right]), max(df.iso[ , iso.right]))
  
  # prepare double axis
  # scale the right y data
  b <- diff(ylim.left) / diff(ylim.right)
  a <- ylim.left[1] - b*ylim.right[1]
  
  if(any(img.layout == "grid")){
    flag <- "grid"
    ll.g <- list()
    for(ind in unique(df.iso[ , col.ind])){
      # ind <- "OVAR1"
      df.select <- df.iso[df.iso[ , col.ind] == ind, ]
      img.out <- graphic_df(df.select, 
                            img.title,
                            iso.left, iso.right, 
                            y.left.color, y.right.color,
                            y.left.by, y.right.by,
                            x.by,
                            stat.ci,
                            min.left, min.right,
                            max.left, max.right,
                            xlim.ERJ,
                            a, b,
                            flag)
      ll.g[[length(ll.g) + 1]] <- img.out
    }
    # g.grid <- do.call("grid.arrange", c(ll.g, ncol = 3))
    # # add title
    # g.grid <- g.grid(top = textGrob("Daily QC: Blue",
    #                                 gp = gpar(fontsize=20,font=3)))
    g.grid <- grid.arrange(grobs = ll.g, 
                           ncol = 3, 
                           top = textGrob(img.title,
                                          gp = gpar(fontsize = 12)))
    g.out <- paste0(file.path(tempdir()), "\\", outFile, "_grid_", img.format)
    ggsave(g.out, g.grid,
           width = img.width.grid, 
           height = img.height.grid,
           units = img.units)
    # ggsave(g.out, g.grid,
    #        width = img.width.grid, 
    #        height = img.height.grid,
    #        units = img.units)
    cat(print(paste0("image '", g.out, "' has been saved !")))
  }
  if(any(img.layout == "align")){
    # TODO: ind -> df.iso[ , col.ind]
    flag <- "align"
    img.out <- graphic_df(df.iso, 
                          img.title,
                          iso.left, iso.right, 
                          y.left.color, y.right.color,
                          y.left.by, y.right.by,
                          x.by,
                          stat.ci,
                          min.left, min.right,
                          max.left, max.right,
                          xlim.ERJ,
                          a, b,
                          flag)
    img.out <- img.out + facet_grid(ind ~ .)
    g.out <- paste0(file.path(tempdir()), "\\", outFile, "_align_", img.format)
    ggsave(g.out, img.out,
           width = img.width.align, 
           height = img.height.align,
           units = img.units)
    cat(print(paste0("image '", g.out, "' has been saved !")))
  }
}