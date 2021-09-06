
# ggHoriPlot: build horizon plots in ggplot2 <img src='man/images/sticker_ggHoriPlot.png' align="right" height="180" >

<!-- badges: start -->

[![R-CMD-check](https://github.com/rivasiker/ggHoriPlot/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/rivasiker/ggHoriPlot/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/rivasiker/ggHoriPlot/branch/master/graph/badge.svg?token=8V5E63YVM2)](https://codecov.io/gh/rivasiker/ggHoriPlot)
<!-- badges: end -->

This package allows building horizon plots in ggplot2. You can learn
more about the package in `vignette('ggHoriPlot')`

## Installation

If you install devtools in your R environment with
`install.packages("devtools")`, the package can be installed with the
following command:

    devtools::install_github("rivasiker/ggHoriPlot")

## Basic example

``` r
library(tidyverse)
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
#> ✓ tibble  3.1.4     ✓ dplyr   1.0.5
#> ✓ tidyr   1.1.3     ✓ stringr 1.4.0
#> ✓ readr   2.0.1     ✓ forcats 0.5.1
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(ggHoriPlot) 
library(ggthemes)

utils::data(climate_CPH)

cutpoints <- climate_CPH  %>% 
  mutate(
    outlier = between(
      AvgTemperature, 
      quantile(AvgTemperature, 0.25, na.rm=T)-1.5*IQR(AvgTemperature, na.rm=T),
      quantile(AvgTemperature, 0.75, na.rm=T)+1.5*IQR(AvgTemperature, na.rm=T))) %>% 
  filter(outlier)

ori = sum(range(cutpoints$AvgTemperature))/2
sca <- seq(range(cutpoints$AvgTemperature)[1], range(cutpoints$AvgTemperature)[2], length.out = 7)[-4]

climate_CPH %>% ggplot() +
  geom_horizon(aes(date_mine, 
                   AvgTemperature,
                   fill = ..Cutpoints..), 
               origin = ori, horizonscale = sca) +
  scale_fill_hcl(palette = 'RdBu', reverse = T) +
  facet_grid(Year~.) +
  theme_few() +
  theme(
    panel.spacing.y=unit(0, "lines"),
    strip.text.y = element_text(size = 7, angle = 0, hjust = 0),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.border = element_blank()
    ) +
  scale_x_date(expand=c(0,0), date_breaks = "1 month", date_labels = "%b") +
  xlab('Date')
```

![](man/figures/CPH_climate-1.png)<!-- -->
