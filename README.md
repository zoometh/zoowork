# ***zoowork*** R package <br> Methods for Archaeozoology <img src="https://github.com/zoometh/thomashuet.github.io/blob/main/img/prj_zoowork.png" align="right" width="120"/>
> Thomas Huet, Ariadna Nieto-Espinet

Install the package from GitHub into R:

```
devtools::install_github("zoometh/zoowork")
```

## functions


### zoo_isotops()

<img src="www/align.png" align="center" width="500"/>

Plot the isotope dataset in a grid layout without confidence intervals

```
zoo_isotops(img.layout = "grid",
            iso.colors = c("red", "blue"),
            stat.ci = FALSE,
            img.format = ".png")
```

Align the different plot with confidence intervals and defaults colors

```
zoo_isotops(img.layout = "align",
            img.title = "O and Sr isotopic variations"
)
```

### zoo_nisp_hc()

Hierarchical clustering on NISP distributions (coming soon)
