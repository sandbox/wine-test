library(ggplot2)
library(reshape)
## creating the word cloud visualization
library(wordcloud)
library(RColorBrewer)

non.word.columns <- 1:3

treatment.ranks <- function(row, f) {
  as.integer(sub("X", "", colnames(sort(f[row, -(non.word.columns)]))))
}
word.ranks <- function(berry, earthy) {
  berry.order <- data.frame(berry=1:length(berry),word=berry)
  berry.order <- berry.order[with(berry.order, order(word)), ]
  earthy.order <- data.frame(earthy=1:length(earthy),word=earthy)
  earthy.order <- earthy.order[with(earthy.order, order(word)), ]

  data.frame(berry=berry.order$berry,earthy=earthy.order$earthy,word=words[earthy.order$word,1],color=words[earthy.order$word,3])
}
rank.chart <- function(ranks, title="Blah", filename="word-rank.pdf") {
  graph.df <- melt(ranks)
  ggplot(graph.df, aes(factor(variable), value, group = word, colour = color, label = word)) +
    geom_line() +
      geom_text(data = subset(graph.df, variable == "earthy"), aes(x = factor(variable)), size = 3.5, hjust = 0) +
        geom_text(data = subset(graph.df, variable == "berry"), aes(x = factor(variable)), size = 3.5, hjust = 1.0) +
          theme_bw() +
            opts(title=title, legend.position = "none", panel.border = theme_blank(), axis.ticks = theme_blank()) +
              scale_colour_identity() + 
                scale_x_discrete(breaks = levels(graph.df$variable), labels = c("berry", "earthy")) +
                  scale_y_continuous(breaks = NA, trans = "reverse") + xlab(NULL) + ylab(NULL)
  ggsave(filename)
}
output.cloud <- function(wine, treatment, d, filename="wordcloud.png") {
  crit <- d$wine == wine & d$treatment == treatment
  pal <- brewer.pal(8,"Dark2")
  png(filename, width=1280,height=800)
  wordcloud(d$variable[crit],d$value[crit],
            scale=c(8,.2),
            min.freq=1,
            max.words=Inf,
            random.order=FALSE,
            ordered.colors=TRUE,
            rot.per=.0,
            colors=as.character(words$color))
  dev.off()
}

experiment.three <- read.table("actual/survey_3.csv", header = TRUE, sep=",")
words            <- read.table("actual/descriptors.csv", header = TRUE, sep=",")
raw.words        <- read.table("actual/survey_3.csv", header = TRUE, sep=",")[,-3]

exp.melt <- melt(experiment.three, id.vars = c("treatment", "wine"), fun=sum)
word.freq.table <- cast(wine + treatment ~ variable | value, data=exp.melt, fun.aggregate=sum)$`1`

wine.D.earthy.ranks <- treatment.ranks(1, word.freq.table)
wine.D.berry.ranks  <- treatment.ranks(4, word.freq.table)
wine.E.earthy.ranks <- treatment.ranks(2, word.freq.table)
wine.E.berry.ranks  <- treatment.ranks(3, word.freq.table)

by.treatment <- rowsum(experiment.three, experiment.three[,1])[,-(non.word.columns)]
by.wine <- rowsum(experiment.three, experiment.three[,2])[,-(non.word.columns)]


wine.D.ranks <- word.ranks(wine.D.berry.ranks, wine.D.earthy.ranks)
wine.E.ranks <- word.ranks(wine.E.berry.ranks, wine.E.earthy.ranks)

colnames(raw.words) <- c(colnames(raw.words)[1:2],
                         as.character(words[as.integer(sub("X","",colnames(raw.words)[-(1:2)])),1]))
raw.words.melt <- melt(raw.words, id.vars = c("treatment", "wine"), fun=sum)
raw.words.melt.cast <- cast(wine + treatment ~ variable | value, data=raw.words.melt, fun.aggregate=sum)$`1`
word.freq.df <- melt(raw.words.melt.cast)

# charting the rank charts
rank.chart(wine.D.ranks, "Wine D Word Usage Rank Given Association", "actual/word-rank-wine-d.pdf")
rank.chart(wine.E.ranks, "Wine E Word Usage Rank Given Association", "actual/word-rank-wine-e.pdf")

#wine, treatment
output.cloud(1, 1, word.freq.df, "actual/Wine D as Earthy.png")
output.cloud(1, 2, word.freq.df, "actual/Wine E as Earthy.png")

output.cloud(2, 1, word.freq.df, "actual/Wine E as Berry.png")
output.cloud(2, 2, word.freq.df, "actual/Wine D as Berry.png")

# basic barcharts
raw.words.melt.cast

# overall total word choice
graph.freq.df <- melt(apply(raw.words.melt.cast, 2, sum))
graph.freq.df$variable <- factor(rownames(graph.freq.df))
graph.freq.df$variable <- factor(graph.freq.df$variable, levels=as.character(graph.freq.df[with(graph.freq.df, order(value)), ]$variable), ordered=TRUE)
colors <- as.character(words[order(graph.freq.df$variable),]$color)
ggplot(graph.freq.df, aes(variable, value, fill=variable, colour=variable)) +
  geom_bar(stat="identity") +
  theme_bw() +
  scale_colour_manual(values=colors) +
  scale_fill_manual(values=colors) +
  xlab("Descriptors") +
  ylab("Frequency of Usage") +
  opts(legend.position = "none", axis.title.x=theme_text(vjust=0))
ggsave("actual/Word Frequency.pdf", width=18,height=12)

apply(raw.words.melt.cast, 2, sum)

# preference count visualizations and analysis
