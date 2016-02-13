##' ----------------------------------------------------------------------------
##' @title Reshape and produce some simple plot of greenland fieldwork 
##'   sites NDVI 
##' @author Damien G
##' @date 10/02/2016
##' 
##' @description
##'   In this script we will play a bit whith the NDVI data extracted from GEE via
##'   the script https://code.earthengine.google.com/5c32d33e066ef89cec6cfac697483de6
##' @log
##' 
##' @licence GPL-2
##' ----------------------------------------------------------------------------

##' start of script ------------------------------------------------------------
rm(list = ls())
setwd("~/GREENLAND/workdir/")

##' laod libraries -------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

output.dir <- "graph_ndvi_trends"
dir.create(output.dir, showWarnings = FALSE, recursive = TRUE)

##' load sites NDVI extractions ------------------------------------------------
mcd43a4.ndvi.file <- "../gee_extactions/ndvi_extraction_2016-02-10/documents-export-2016-02-11/fieldwork_sites_mcd43a4_ndvi_extractions.csv"
mod13q1.ndvi.file <- "../gee_extactions/ndvi_extraction_2016-02-10/documents-export-2016-02-11/fieldwork_sites_mod13q1_ndvi_extractions.csv"
landsat7.ndvi.file <- "../gee_extactions/ndvi_extraction_2016-02-10/documents-export-2016-02-11/fieldwork_sites_landsat7_ndvi_extractions.csv"
gimms3g.ndvi.file <- "../gee_extactions/ndvi_extraction_2016-02-10/documents-export-2016-02-11/fieldwork_sites_gimms3g_ndvi_extractions.csv"

mcd43a4.ndvi <- read.csv(mcd43a4.ndvi.file, header = TRUE, stringsAsFactors = FALSE)
mod13q1.ndvi <- read.csv(mod13q1.ndvi.file, header = TRUE, stringsAsFactors = FALSE)
landsat7.ndvi <- read.csv(landsat7.ndvi.file, header = TRUE, stringsAsFactors = FALSE)
gimms3g.ndvi <- read.csv(gimms3g.ndvi.file, header = TRUE, stringsAsFactors = FALSE)

sites.ndvi <- bind_rows(mcd43a4.ndvi, mod13q1.ndvi, landsat7.ndvi, gimms3g.ndvi)
rm(mcd43a4.ndvi, mod13q1.ndvi, landsat7.ndvi, gimms3g.ndvi)

##' reshape the table ----------------------------------------------------------
sites.ndvi <- sites.ndvi %>% select(-.geo) %>% filter(!is.na(source)) %>% 
  mutate(stdDev_buff_000000m = 0)

##' put the value of the smaller resolution if the next one is empty
res <- c('000000m', '000500m', '001000m', '008000m', '025000m')
stat <- c('mean', 'min', 'max', 'stdDev')
for(i in 1:(length(res)-1)){
  cat("\n>", i)
  res1 <- res[i]
  res2 <- res[i+1]
  for(stat_ in stat){
    sites.ndvi[[paste0(stat_, "_buff_", res2)]][is.na(sites.ndvi[[paste0(stat_, "_buff_", res2)]])] <- sites.ndvi[[paste0(stat_, "_buff_", res1)]][is.na(sites.ndvi[[paste0(stat_, "_buff_", res2)]])]
  }
}


##' produce the graph of max of max ndvi by year fct resolution ---------------
## extract max ndvi
dat_ <- sites.ndvi %>% select(site, year, source, max_buff_000000m, max_buff_000500m, max_buff_008000m) %>%
  gather(max.ndvi.buff, max.ndvi.val, max_buff_000000m, max_buff_000500m, max_buff_008000m) %>%
  mutate(buff.size = sub("^max_buff_[0]{1,5}", "", max.ndvi.buff)) %>% select(-max.ndvi.buff)


## extract stdDev ndvi
dat__ <- sites.ndvi %>% select(site, year, source, stdDev_buff_000000m, stdDev_buff_000500m, stdDev_buff_008000m) %>%
  gather(stdDev.ndvi.buff, stdDev.ndvi.val, stdDev_buff_000000m, stdDev_buff_000500m, stdDev_buff_008000m) %>%
  mutate(buff.size = sub("^stdDev_buff_[0]{1,5}", "", stdDev.ndvi.buff)) %>% select(-stdDev.ndvi.buff)

## join table
dat_ <- dat_ %>% full_join(dat__)
rm(dat__)

## keep only the max of max ndvi over buffer by year, site and source
dat_ <- dat_ %>% group_by(site, year, source, buff.size) %>% 
  filter(max.ndvi.val == max(max.ndvi.val, na.rm = TRUE)) %>%
  distinct

dat_$meta.site <- sub("_.*$", "", dat_$site)
dat_$meta.subsite <- sub("^.*_", "", dat_$site)

## fine tuning of graph
dat__ <- dat_ %>% filter(year >= 2000, year <= 2015)
dat__$stdDev.ndvi.val[is.na(dat__$stdDev.ndvi.val)] <- 0

png(file.path(output.dir,"trends_in_max_ndvi_meta.png"), width = 900, height = 900)
gg <- ggplot(data = dat__, aes(x = year, y = max.ndvi.val, ymin = max.ndvi.val - stdDev.ndvi.val,
                               ymax = max.ndvi.val + stdDev.ndvi.val, col = source, lty = meta.subsite)) +
  geom_point() + geom_smooth(method = 'lm', se = FALSE) + ylab("yearly max buffered averaged ndvi") + xlab('') +
  facet_grid(meta.site ~ buff.size )
print(gg)
dev.off()

## produce a graph by resolution, by sites groups => 3*11 => 33 graphs
output.dir.bis <- file.path(output.dir, "trends_in_max_ndvi_site_by_site")
dir.create(output.dir.bis, showWarnings = FALSE, recursive = TRUE)
for(meta.site_ in unique(dat__$meta.site)){
  for(buff.size_ in c('0m', '500m', '8000m')){
    dat___ <- dat__ %>% filter(meta.site == meta.site_, buff.size == buff.size_)
    png(file.path(output.dir.bis, paste0("trends_in_max_ndvi_",meta.site_, "_", buff.size_,".png")))
    gg <- ggplot(data = dat___, aes(x = year, y = max.ndvi.val, ymin = max.ndvi.val - stdDev.ndvi.val,
                                    ymax = max.ndvi.val + stdDev.ndvi.val, col = source, lty = meta.subsite)) +
      geom_point() + geom_smooth(method = 'lm', se = FALSE) + ylab("yearly max buffered averaged ndvi") + xlab('')+
      coord_cartesian(ylim = c(0.1, 1))
    print(gg)
    dev.off()
  }
}

##' end of script --------------------------------------------------------------
