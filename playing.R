library(ggbio)

data("CRC", package = "biovizBase")
head(hg19sub)

autoplot(hg19sub, layout = "circle", fill = "gray70")

gr.crc1 <- crc.gr[crc.gr$individual == "CRC-1"]

p <- ggbio()
p <- p + circle(mut.gr, geom = "rect", color = "steelblue")
p <- p + circle(hg19sub, geom = "ideo", fill = "gray70")
p <- p + circle(hg19sub, geom = "scale", size = 2)
p <- p + circle(hg19sub, geom = "text", aes(label = seqnames), vjust = 0, size = 3)
p <- p + circle(gr.crc1, geom = "point", aes(y = score, size = tumreads), color = "red", grid = TRUE, radius = 30)
p <- p + scale_size(range = c(1, 2.5))
p <- p + circle(gr.crc1, geom = "link", linked.to = "to.gr", aes(color = rearrangements), radius = 23)
p

hg19sub
gr.crc1
mut.gr

library(GenomicRanges)

genes.gr <- GRanges(
  seqnames = Rle(rep(c("chr1", "chr2", "chr3"), each = 10)),
  ranges = IRanges(round(sort(runif(30, 1, 1000)))),
  seqlengths = c(chr1 = 1000, chr2 = 1000, chr3 = 1000)
)

edges.gr <- genes.gr[sample(1:30, 30, replace = FALSE)]

genes.gr$to.gr <- edges.gr

ggbio() +
  circle(genes.gr, geom = "rect", color = "steelblue") +
  circle(genes.gr, geom = "ideo", fill = "gray70") +
  circle(genes.gr, geom = "link", linked.to = "to.gr", radius = 30)
