

#maxSr=0.709999
#minSr=0.707976

#maxO= 3.83
#minO=-8.24

############################################################################### Gràfics quadrats en serie
###############################################################################
library(ggplot2)
library(tibble)
library(ggpubr)

df <- read.table("~/Desktop/UTILITATS /R/R_Sr&O/Resultats_SR_O_C_Etno3.txt", header=TRUE,sep="\t")
maxO <- max(df$O, na.rm = T) ; minO <- min(df$O, na.rm = T)
maxSr <- max(df$Sr, na.rm = T) ; minSr <- min(df$Sr, na.rm = T)
xlim.ERJ <- c(min(df$ERJ), max(df$ERJ)) 
ylim.O <- c(min(df$O), max(df$O)) 
ylim.Sr <- c(min(df$Sr), max(df$Sr))
b <- diff(ylim.O)/diff(ylim.Sr)
a <- ylim.O[1] - b*ylim.Sr[1]
llg.ethn <- list()
for(ind in unique(df$ind)){
  df.select <- df[df$ind == ind, ]
  ethno.df <- tibble(
    ind = df.select$ind,
    ERJ = df.select$ERJ,
    Sr = df.select$Sr,
    O = df.select$O
  )
  g.ethno <- ggplot(ethno.df, aes(ERJ, O)) +
    ggtitle(paste(ind)) +
    geom_line(color = "turquoise", size=0.2) +
    geom_point(col="turquoise", size=1) +
    geom_line(aes(y = a + Sr*b), color = "turquoise4", size=0.2) +
    geom_point(aes(y = a + Sr*b), color = "turquoise4", size=1) +
    scale_y_continuous("O", 
                       limits = c(minO,maxO),
                       sec.axis = sec_axis(~ (. - a)/b, name = "Sr", 
                                           breaks = seq(minSr, maxSr, by = .0002),
                                           labels = scales::number_format(accuracy = 0.0001,
                                                                          decimal.mark = '.'))) +
    scale_x_reverse(breaks = seq(ceiling(min(ethno.df$ERJ))-1,
                                 ceiling(max(ethno.df$ERJ)),
                                 by = 5)) +
    expand_limits(x = c(ceiling(xlim.ERJ[1])-1, xlim.ERJ[2])) +
    theme_bw() +
    theme(title = element_text(size = 8),
      axis.title.y.left=element_text(color="turquoise", size = 7),
      axis.text.y.left=element_text(color="turquoise", size = 6),
      axis.title.y.right=element_text(color="turquoise4", size = 7),
      axis.text.y.right=element_text(color="turquoise4", size = 6),
      axis.text.x = element_text(size = 6),
      axis.text.y = element_text(size = 6)
    )
  llg.ethn[[length(llg.ethn) + 1]] <- g.ethno
}
# llg.ethn[[3]]

# Save
library(grid)
library(gridExtra)
g.out <- "~/Desktop/UTILITATS /R/R_Sr&O/all_SR_O4.pdf"
g.ethno.arranged <- do.call("grid.arrange", c(llg.ethn, ncol=3))
ggsave(g.out, g.ethno.arranged,
       width = 18, height =16,
       units = "cm")
# shell.exec(g.out)

###############################################################################Gràfics allargats en serie O/Sr
############################################################################### 


library(ggplot2)
library(tibble)

df <- read.table("~/Desktop/UTILITATS /R/R_Sr&O/Resultats_SR_O_C_Etno3.txt", header=TRUE,sep="\t")
maxO <- max(df$O, na.rm = T) ; minO <- min(df$O, na.rm = T)
maxSr <- max(df$Sr, na.rm = T) ; minSr <- min(df$Sr, na.rm = T)
# ovar <- "CAHI1"
# df <- df[df$ind == ovar, ]
# View(df)

ethno.df <- tibble(
  ind = df$ind,
  ERJ = df$ERJ,
  Sr = df$Sr,
  O = df$O
)

ylim.O <- c(min(ethno.df$O), max(ethno.df$O)) 
ylim.Sr <- c(min(ethno.df$Sr), max(ethno.df$Sr))

b <- diff(ylim.O)/diff(ylim.Sr)
a <- ylim.O[1] - b*ylim.Sr[1]

g.ethno <- ggplot(ethno.df, aes(ERJ, O)) +
  ggtitle(paste("Sheep and goat")) +
  facet_grid(ind ~ .) +
  geom_line(color = "turquoise") +
  geom_point(col="turquoise") +
  geom_line(aes(y = a + Sr*b), color = "turquoise4") +
  geom_point(aes(y = a + Sr*b), color = "turquoise4") +
  scale_y_continuous("O", limits = c(minO, maxO), 
                     sec.axis = sec_axis(~ (. - a)/b, name = "Sr", 
                                         breaks = seq(minSr, maxSr, by = .0002),
                                         labels = scales::number_format(accuracy = 0.000001,
                                                                        decimal.mark = '.'))) +
  scale_x_reverse(breaks = seq(ceiling(min(ethno.df$ERJ))-1,
                               ceiling(max(ethno.df$ERJ)),
                               by = 2)) +
# limits = c(minSr, maxSr)
  theme_bw() +
  theme(title = element_text(size = 8),
        axis.title.y.left=element_text(color="turquoise", size = 7),
        axis.text.y.left=element_text(color="turquoise", size = 6),
        axis.title.y.right=element_text(color="turquoise4", size = 7),
        axis.text.y.right=element_text(color="turquoise4", size = 6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)
  )
g.ethno
#llg.ethn[[length(llg.ethn) + 1]] <- g.ethno


# Save

g.out <- "~/Desktop/UTILITATS /R/R_Sr&O/all_SR_Prova.pdf"
ggsave(g.out, g.ethno,
       width = 13, height = 21,
       units = "cm")

# shell.exec(g.out)

