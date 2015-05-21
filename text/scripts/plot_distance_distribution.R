library(ggplot2)
library(scales)


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


plot_distance_distribution <- function(path, chr = "1", strand = "+", cutoff = 1000) {
    df <- read.csv(path, sep="\t")
    df <- df[df$chr == chr & df$strand == strand & df$count > cutoff, ]
    ggplot(df, aes(x = factor(distance, levels = min(distance):max(distance)),
                   y = count)) +
        geom_bar(stat = "identity", position = "dodge", alpha = 0, colour = "black") +
        xlab(expression(d)) + ylab(expression(paste("#{", d[t] == d, "}"))) +
        scale_y_continuous(labels = fancy_scientific) +
        theme_minimal()
}


if (!interactive()) {
    p <- plot_distance_distribution("data/GSE30202_mm9_mES-V6.5.csv")
    ggsave("images/distance_distribution.png", p, width = 12, height = 4)
}
