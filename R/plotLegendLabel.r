# plotLegendLabel <- function(quantity=expression(paste(theta)),
#                            unit=expression(paste("[", degree, "]")),
#                            vjust.quantity=1.5, vjust.unit=2.5, ...) {
#     trellis.focus("legend", side="right", clipp.off=TRUE, highlight=FALSE)
#     grid.text(quantity, 0.2, 0, hjust=0.5, vjust=vjust.quantity, 
#               gp = gpar(...))
#     grid.text(unit, 0.2, 0, hjust=0.5, vjust=vjust.unit, gp = gpar(...))
#     trellis.unfocus()
# }
# 
# plotLegendLabelHeight <- function(quantity='height', unit='[m]', 
#                                  vjust.unit=3, ...) {
#     plotLegendLabel(quantity, unit, vjust.unit=vjust.unit, ...)
# }