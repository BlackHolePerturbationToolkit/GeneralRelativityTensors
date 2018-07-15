{
  "More Information" -> {
      "CovariantD returns an expression that is a sum and product of Tensors.",
      "Call MergeTensors on the output to form one Tensor."
  },

  "Basic Examples" -> {
    "gRN = ToMetric[\"ReissnerNordstrom\"]",
    "dgRN = CovariantD[gRN, -\[Gamma]]",
    "MergeTensors[dgRN] // TensorValues // Simplify",
    "uS = FourVelocityVector[\"SchwarzschildGeneric\"]",
    "covDuS = CovariantD[uS, uS]",
    "vals = TensorValues@MergeTensors[covDuS, \"ActWith\" -> Simplify]",
    "vals /. Thread[D[Through[Coordinates[uS][\[Tau]]], \[Tau]] -> TensorValues[uS]] // Simplify"
    },

    "See Also" ->
    {"BianchiIdentities","TensorValues","FourVelocityVector"}

}
