#!/usr/bin/Rscript

library(data.table)
library(lubridate)
library(ggplot2)
library(svglite)
library(scales)

web_fonts <- "https://fonts.cdnfonts.com/css/liberation-sans"


sacct <- fread("sacct", sep="|")
samples <- fread("samples", header=FALSE,
                 col.names = c("work.dir", "sample"))
filesizes <- fread("filesizes", header=FALSE,
                   col.names = c("bytes", "sample_end"))


jobs <- sacct[grepl("^concat-", JobName) & State== "COMPLETED",
              .(JobName,
                start=Start,
                partition=Partition,
                elapsed=hms(Elapsed),
                work.dir=sub("^.*/([^/]+)$", "\\1", WorkDir)
                )]

filesizes[, sample:=sub("_[12]$", "", sample_end) ]
filesizes[, end:=sub("^.*_([12])$", "\\1", sample_end) ]
sizes <- filesizes[, .(size.GB = sum(bytes)/1024^3), by=sample]


data <- samples[jobs, on=.(work.dir)]
data <- sizes[data, on=.(sample)]


p.size <- ggplot(sizes) +
    aes(sample, size.GB) +
    geom_point() +
    theme_bw()

p.time.vs.sample <- ggplot(data) +
    aes(sample, elapsed) +
    geom_violin(draw_quantiles=.5) +
    geom_point(aes(color=partition)) +
    scale_colour_brewer(type="qual") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=30, hjust=1))

p.time.vs.partition <- ggplot(data) +
    aes(partition, elapsed) +
    geom_violin(draw_quantiles=.5) +
    geom_point(aes(colour=sample)) +
    scale_colour_brewer(type="qual") +
    theme_bw()

p.time.vs.size <- ggplot(data) +
    aes(size.GB, elapsed, colour=partition, fill=partition) +
    geom_point() +
    scale_colour_brewer(type="qual") +
    scale_fill_brewer(type="qual") +
    theme_bw()

p.time.vs.start <- ggplot(data) +
    facet_grid(sample ~ .) +
    aes(start, elapsed, color=partition) +
    geom_point() +
    scale_colour_brewer(type="qual") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=30, hjust=1))



svglite("plots/size.svg", 4, 3, web_fonts=web_fonts)
print(p.size)
z <- dev.off()

svglite("plots/time-sample.svg", 4, 3, web_fonts=web_fonts)
print(p.time.vs.sample)
z <- dev.off()

svglite("plots/time-partition.svg", 4, 3, web_fonts=web_fonts)
print(p.time.vs.partition)
z <- dev.off()

svglite("plots/time-size.svg", 4, 3, web_fonts=web_fonts)
print(p.time.vs.size)
z <- dev.off()

svglite("plots/time-start.svg", 4, 3, web_fonts=web_fonts)
print(p.time.vs.start)
z <- dev.off()


##end
