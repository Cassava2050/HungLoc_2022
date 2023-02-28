# Libraries ---------------------------------------------------------------

library(corrplot)

# Load the file -----------------------------------------------------------

folder_output = here::here("output//")


list_file <- list.files(folder_output)
list_file
sel_file <- list_file[str_detect(list_file, "_master_results_") &
                        str_detect(list_file, trial_interest)]

sel_file[1]
blupDF_kp <- read_excel(
  paste(folder_output,
        sel_file[1],
        sep = ""
  ),
  sheet = paste0("BLUPs_gxe")
)


# Remove row names --------------------------------------------------------

blupDF_value <- blupDF_kp %>%
  select(-accession_name)


# Compute the correlation -------------------------------------------------

M <- cor(as.matrix(blupDF_value), use = "complete.obs")
testRes <- cor.mtest(as.matrix(blupDF_value), conf.level = 0.95)


# Save the file -----------------------------------------------------------

pdf(paste("cor_traits_", trial_interest, "_",
          Sys.Date(), ".pdf",
          sep = ""
), width = 10, height = 10)

corrplot(M,
         p.mat = testRes$p,
         method = "color",
         sig.level = c(0.001, 0.01, 0.05),
         insig = "label_sig",
         pch.cex = 0.9,
         # addCoef.col = 'black',
         type = "upper",
         diag = FALSE,
         col = colorRampPalette(c("red", "white", "blue"))(40),
         tl.cex = 1.5,
         tl.col = "black",
         addgrid.col = "black"
)$corrPos -> p1
text(p1$x, p1$y, pos = 3, round(p1$corr, 3), col = "black", cex = 1)

dev.off()
