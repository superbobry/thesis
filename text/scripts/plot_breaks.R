library(ggplot2)
library(plyr)
library(rjson)
library(stringr)


fancy_scientific <- function(l) {
    ## turn in to character string in scientific notation
    l <- format(l, scientific = TRUE)
    ## quote the part before the exponent to keep all the digits
    l <- gsub("^(.*)e", "'\\1'e", l)
    ## turn the 'e+' into plotmath format
    l <- gsub("e\\+", "%*%10^", l)
    ## return this as an expression
    parse(text = l)
}


main <- function(csv_path, json_path, cutoff = 1000, chr = "1", strand = "+") {
    f <- file(json_path)
    breaks_map <- fromJSON(file = f)
    close(f)

    df <- read.csv(csv_path, sep = "\t")
    df <- df[df$chr == chr & df$strand == strand, ]

    ## Note(lebedev): we need a cutoff to drop bins which won't be
    ## noticable on a bar plot.
    cutoff <- df$count[df$distance == 61]
    max_count_distance = max(df[df$count > cutoff, "distance"])
    breaks <- breaks_map[[sprintf("(%s, %s)", chr, strand)]]
    p <- (ggplot(with(df, df[count > cutoff, ]),
                 aes(x = factor(distance, levels = min(distance):max(distance)),
                     y = count))
          + geom_bar(stat = "identity", position = "dodge", alpha = 0, colour = "black")
          + geom_vline(xintercept = breaks[breaks <= max_count_distance],
                       linetype = "dotted")
          + xlab(expression(d))
          + ylab(expression(d)) + ylab(expression(paste("#{", d[t] == d, "}")))
          + scale_y_continuous(labels = fancy_scientific)
          + theme_minimal())

    png_name <- sprintf("%s.png", str_replace(basename(json_path), ".json", ""))
    png_path <- file.path(dirname(json_path), png_name)
    ggsave(png_path, height = 4, width = 12)
}


if (!interactive()) {
    args <- commandArgs(TRUE)
    if (length(args) < 1 || length(args) %% 2 > 0) {
        write("Usage: [executable] [path/to/distances.csv path/to/distances.json]+",
              stderr())
        q(statu = 1)
    }

    do.call(main, as.list(args))
}
