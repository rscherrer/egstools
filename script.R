







#disassortment <- mrep(c(1,-1), sapply(d, nrow))
#d <- do.call(rbind, d)
#d$disassortment <- as.factor(disassortment)

d2 <- d %>%
  filter(disassortment == 1) %>%
  group_by(id) %>%
  summarize(RI = RI[n()])

length(which(d2$RI < -0.2))
hist(d2$RI)

d <- egstools::collect(
  dir = "/media/raphael/bigass/simulations_epistatic_large",
  files = files
)

d2 <- d %>%
  group_by(id) %>%
  summarize(RI = RI[n()])
folders <- d2$id[which(d2$RI > 0.5)]

folders <- list.files("/media/raphael/bigass/simulations_disassortative_longrun", full.names = TRUE)
folders <- folders[grep("sim_", folders)]

lapply(seq_len(33), function(i) {

  folder <- folders[i]

  file.copy(
    paste0(folder, "/architecture.txt"),
    paste0("/media/raphael/bigass/architectures_assortative/architecture_", i, ".txt")
  )

})

# Phase plane
p <- plot_phase(
  d,
  xname = "EI",
  yname = "RI",
  #tname = "t",
  labs = c("Ecological divergence", "Reproductive isolation"),
  #colvar = "disassortment",
  #collab = "Disassortative"
  splitvar = "ecosel",
  splitvar2 = "hsymmetry"
)

p

ggsave("RSI.pdf", p)

# Heatmap
h <- plot_heatmap(
  d,
  labs = c("Habitat symmetry", "Ecological selection"),
  xname = "hsymmetry",
  yname = "ecosel",
  zname = "EI",
  tname = "t",
  summary = "value",
  aggregate = "number",
  threshold = 0.9,
  collab = "Ecological divergence",
  colors = c("black", "lightgreen"),
  splitvar = "epistasis"
)

h
ggsave("heatmap_EI_lowdispersal.pdf", h)

############

############

# Speciation cube
#cube <- plot_cube(d, labs, phi = 30, theta = 300)
