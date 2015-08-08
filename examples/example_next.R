smMatrix = matrix_structural( c(
  c("Perceived Visual Complexity", "Aproach/Avoidance"),
  c("Arousal","Aproach/Avoidance")
))

# smMatrix <- matrix(c("Perceived Visual Complexity", "Aproach/Avoidance",
#                      "Arousal","Aproach/Avoidance"),nrow=2,ncol=2,byrow =TRUE,
#                    dimnames = list(1:2,c("source","target")))


mmMatrix <- measurement_matrix(c(
        c("PVC", "F", c("VX.0", "VX.1", "VX.2", "VX.3", "VX.4")),
        c("ARO", "F", c("VX.0", "Aro1", "Aro2", "Aro3", "Aro4")),
        c("AA", "R", c("AA.0", "AA.1", "AA.2", "AA.3"))
))

# mmMatrix <- matrix(c("Perceived Visual Complexity","VX.0","F",
#                      "Perceived Visual Complexity","VX.1","F",
#                      "Perceived Visual Complexity","VX.2","F",
#                      "Perceived Visual Complexity","VX.3","F",
#                      "Perceived Visual Complexity","VX.4","F",
#                      "Arousal","Aro1","F",
#                      "Arousal","Aro2","F",
#                      "Arousal","Aro3","F",
#                      "Arousal","Aro4","F",
#                      "Aproach/Avoidance","AA.0","R",
#                      "Aproach/Avoidance","AA.1","R",
#                      "Aproach/Avoidance","AA.2","R",
#                      "Aproach/Avoidance","AA.3","R"),nrow=13,ncol=3,byrow =TRUE,
#                    dimnames = list(1:13,c("latent","measurement","type")))

latent_measures <- function(mmMatrix, latent_name) {
  mmMatrix[mmMatrix[,"latent"]==latent_name, "measurement"]
}

`%/=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 / e2))