#!/usr/bin/env Rscript

require(ggplot2)
require(reshape2)
require(scales)
require(stringr)
require(plyr)


## Main
## ----

alias_labeller <- function(mapping) {
    function(variable, values) {
        sapply(as.character(values), function(v) {
            ifelse(is.null(mapping[[v]]), v, mapping[[v]])
        })
    }
}

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


main <- function(path, base_model) {
    data <- read.csv(path)
    data <- data[data$model %in% c("BD", "BM"), ]
    data <- data[data$chr %in% c("1"), ]
    ## data <- data[data$model %in% c("BHMM", "BMM"), ]
    ## data <- data[data$model %in% c("BHMM", "BHSMM-64", "BHSMM-32"), ]
    data$BIC <- NULL

    ## Order chromosome lexicographically.
    chr_nums  <- as.character(levels(factor(data$chr)))
    chr_names <- str_join("chr", chr_nums)
    data$chr  <- factor(
        str_join("chr", data$chr),
        levels = chr_names[order(chr_nums)])

    ## Order models lexicographically.
    chunks <- as.data.frame(str_split_fixed(levels(data$model), "-", 2))
    models <- levels(data$model)[with(
        chunks, order(sapply(as.character(V1), str_length),
                      as.character(V1),
                      as.character(V2),
                      decreasing = TRUE))]

    data$model <- factor(as.character(data$model), levels = models)
    data.m     <- melt(data, id.vars = c("model", "chr"))
    data.m     <- ddply(data.m, c("variable", "chr"), function(df) {
        df$value  <- abs(as.numeric(df$value))
        base_value <- df[df$model == base_model, "value"]
        df$weight <- sprintf(
            "%s%.2f%%",
            ifelse(df$value / base_value * 100 - 100 >= 0, "+", ""),
            (df$value / base_value * 100 - 100))
        df
    })

    data.m$mask <- data.m$model == base_model  # Base mask.
    ggplot(data.m, aes(x = model, y = value)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0, color = "black") +
        geom_hline(aes(yintercept = value), linetype = "dotted",
                   data = data.m[data.m$mask, ]) +
        scale_fill_brewer(palette = "Set1") +
        labs(fill = "Model") + xlab("") + ylab("") +
        scale_y_continuous(labels = fancy_scientific) +
        coord_flip() +
        facet_grid(. ~ variable,
                   labeller = alias_labeller(list(
                       "log_likelihood" = "Модуль логарифма правдоподобия")),
                   scales = "free_y", space = "free_y") +
        geom_text(aes(label = weight, hjust = 1.5),
                  size = 3, color = "black",
                  position = position_dodge(width = 1)) + theme_minimal() +
    ggsave(str_replace(path, ".csv", ".png"), width = 8, height = 4)
}


if (!interactive()) {
  args <- commandArgs(TRUE)
  if (length(args) != 2) {
    write("Usage: [executable] path/to/csv BASE_MODEL", stderr())
    q(status = 1)
  }

  options(error = function() traceback())
  do.call(main, as.list(args))
}
