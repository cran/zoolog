## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(zoolog)
str(reference, max.level = 1)

## ----echo=FALSE---------------------------------------------------------------
options(knitr.kable.NA = "")
knitr::kable(referenceSets)

## -----------------------------------------------------------------------------
library(zoolog)
dataFile <- system.file("extdata", "dataValenzuelaLamas2008.csv.gz", 
                        package = "zoolog")
data = read.csv2(dataFile,
                 quote = "\"", header = TRUE, na.strings = "",
                 fileEncoding = "UTF-8",
                 stringsAsFactors = TRUE)
knitr::kable(head(data)[, -c(6:20,32:64)])

## -----------------------------------------------------------------------------
dataWithLog <- LogRatios(data)
knitr::kable(head(dataWithLog)[, -c(6:20,32:64)])

## -----------------------------------------------------------------------------
AScases <- InCategory(dataWithLog$Os, "astragalus", zoologThesaurus$element)
knitr::kable(head(dataWithLog[AScases, -c(6:20,32:64)]))

## -----------------------------------------------------------------------------
GLandGLl <- list(c("GL", "GLl"))
dataWithLog <- LogRatios(data, mergedMeasures = GLandGLl)
knitr::kable(head(dataWithLog[AScases, -c(6:20,32:64)]))

## -----------------------------------------------------------------------------
caprineCategory <- list(ovar = c("sheep", "capra", "oc"))
dataWithLog=LogRatios(data, joinCategories = caprineCategory, mergedMeasures = GLandGLl)
knitr::kable(head(dataWithLog)[, -c(6:20,32:64)])

## -----------------------------------------------------------------------------
dataWithLogPruned=RemoveNACases(dataWithLog)
knitr::kable(head(dataWithLogPruned[, -c(6:20,32:64)]))

## ---- eval = FALSE------------------------------------------------------------
#  write.csv2(dataWithLogPruned, "myDataWithLogValues.csv",
#             quote=FALSE, row.names=FALSE, na="",
#             fileEncoding="UTF-8")

## -----------------------------------------------------------------------------
dataWithSummary <- CondenseLogs(dataWithLogPruned)
knitr::kable(head(dataWithSummary)[, -c(6:20,32:64,72:86)])

## -----------------------------------------------------------------------------
dataStandardized <- StandardizeDataSet(dataWithSummary)
knitr::kable(head(dataStandardized)[, -c(6:20,32:64,72:86)])

## -----------------------------------------------------------------------------
dataOC <- subset(dataWithSummary, InCategory(Especie, 
                                             c("sheep", "capra", "oc"),
                                             zoologThesaurus$taxon))
knitr::kable(head(dataOC)[, -c(6:20,32:64)])

## -----------------------------------------------------------------------------
dataOCStandardized <- StandardizeDataSet(dataOC)
knitr::kable(head(dataOCStandardized)[, -c(6:20,32:64)])

## -----------------------------------------------------------------------------
dataOCWithWidth <- RemoveNACases(dataOCStandardized, measureNames = "Width")
dataOCWithLength <- RemoveNACases(dataOCStandardized, measureNames = "Length")

## ---- echo = FALSE------------------------------------------------------------
library(ggplot2)

## ---- fig.asp = 0.6, fig.width = 6, fig.align="center"------------------------
ggplot(dataOCStandardized, aes(x = Site, y = Width)) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(width = 0.2, height = 0, alpha = 1/2, color = 4, na.rm = TRUE) +
  theme_bw() +
  ggtitle("Caprine widths") +
  ylab("Width log-ratio") +
  coord_flip()

## ---- fig.asp = 0.6, fig.width = 6, fig.align="center"------------------------
ggplot(dataOCStandardized, aes(x = Site, y = Length)) +
  geom_boxplot(outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(width = 0.2, height = 0, alpha = 1/2, color = 4, na.rm = TRUE) +
  theme_bw() +
  ggtitle("Caprine lengths") +
  ylab("Length log-ratio") +
  coord_flip()

## ---- fig.asp = 0.7, fig.width = 6, fig.align="center"------------------------
ggplot(dataOCStandardized, aes(Width)) +
  geom_histogram(bins = 30, na.rm = TRUE) +
  ggtitle("Caprine widths") +
  xlab("Width log-ratio") +
  facet_grid(Site ~.) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 10) ) +
  scale_y_continuous(breaks = c(0, 10, 20, 30))

## -----------------------------------------------------------------------------
levels0 <- levels(dataOCStandardized$Taxon)
levels0

## -----------------------------------------------------------------------------
dataOCStandardized$Taxon <- factor(dataOCStandardized$Taxon, 
                                   levels = levels0[c(2,3,1)])
levels(dataOCStandardized$Taxon)

## ---- message = FALSE, fig.asp = 0.6, fig.width = 6, fig.align="center"-------
Ocolour <- c("#A2A475", "#D8B70A", "#81A88D")
ggplot(dataOCStandardized, aes(x=Site, y=Width)) + 
  geom_boxplot(aes(fill=Taxon), 
               notch = TRUE, alpha = 0, lwd = 0.377, outlier.alpha = 0,
               width = 0.5, na.rm = TRUE,
               position = position_dodge(0.75),
               show.legend = FALSE) + 
  geom_point(aes(colour = Taxon, shape = Taxon), 
             alpha = 0.7, size = 0.8, 
             position = position_jitterdodge(jitter.width = 0.3),
             na.rm = TRUE) +
  scale_colour_manual(values=Ocolour) +
  scale_shape_manual(values=c(15, 18, 16)) +
  theme_bw(base_size = 8) +
  ylab("LSI value") +
  ggtitle("Sheep/goat LSI width values") 

## ---- warning = FALSE, fig.asp = 0.6, fig.width = 6, fig.align="center"-------
TaxonSiteWidthHist <- ggplot(dataOCStandardized, aes(Width, fill = Taxon)) + 
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") + 
  ggtitle("Sheep/goat Widths") + facet_grid(Site ~ Taxon)
TaxonSiteWidthHist

## ---- warning = FALSE, fig.asp = 0.6, fig.width = 6, fig.align="center"-------
TaxonSiteWidthHist <- ggplot(dataOCStandardized, aes(Width, fill = Taxon)) + 
  geom_histogram(bins = 30, alpha = 0.5, position = "identity") + 
  ggtitle("Sheep/goat Widths") + facet_grid(~Site)
TaxonSiteWidthHist

## -----------------------------------------------------------------------------
t.test(Length ~ Site, dataOCStandardized, 
       subset = Site %in% c("OLD", "ALP"))

## -----------------------------------------------------------------------------
t.test(Width ~ Site, dataOCStandardized, 
       subset = Site %in% c("OLD", "ALP"))

## ---- message = FALSE---------------------------------------------------------
library(stats)
pairwise.t.test(dataOCStandardized$Width, dataOCStandardized$Site, 
                pool.sd = FALSE)

