# ***zoowork*** functions <br> Methods for Zooarchaeology <img src="https://github.com/zoometh/thomashuet.github.io/blob/main/img/prj_zoowork.png" align="right" width="120"/>
> Thomas Huet, Ariadna Nieto-Espinet


## functions


Plot the isotope dataset in grid without confidence intervals

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