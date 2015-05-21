library(rjson)
library(ggplot2)
library(stringr)
library(reshape2)
library(tools)


plot_models <- function(...) {
    go <- function(path) {
        json <- fromJSON(file = path)
        df   <- t(do.call(rbind, json$event_probabilities))
        rownames(df) <- c("A", "T", "C", "G")
        colnames(df) <- c("UNMETHYLATED", "LOW", "FULL")
        df <- melt(df, value.name = "p", varnames = c("nucleotide", "state"))
        df$chr <- sapply(str_split_fixed(file_path_sans_ext(path), "_", 2)[2],
                         function(chr) sprintf("chr%s", chr))
        levels(df$nucleotide) <- c("A", "T", "C", "G")
        df
    }

    df <- do.call(rbind, lapply(list(...), go))
    ggplot(df, aes(x = nucleotide, y = p)) +
        geom_bar(stat = "identity", alpha = 0, colour = "black") +
        facet_grid(. ~ state) +
        xlab("") + ylab(expression(p)) + theme_minimal()
    ggsave("../images/multinomial.png", width = 8, height = 6)
}
