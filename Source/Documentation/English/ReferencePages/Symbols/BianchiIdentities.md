{
  "More Information" -> {
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "bian0 = BianchiIdentities[gS]",
    "bian0M = MergeTensors[bian0,ActWith->Simplify]",
    "TensorValues[bian0M]",
    "bian2 = BianchiIdentities[gS,2]",
    "bian2M = MergeTensors[bian2,ActWith->Simplify]",
    "TensorValues[bian2M]"
    },

    "See Also" ->
    {"CovariantD"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Built in common Tensors"
    }

}
