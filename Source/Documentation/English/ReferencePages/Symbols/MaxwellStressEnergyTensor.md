{
  "More Information" -> {
  },

  "Basic Examples" -> {
    "ARN = MaxwellPotential[\"ReissnerNordstrom\"]",
    "FRN = FieldStrengthTensor[ARN]",
    "TRN = MaxwellStressEnergyTensor[FRN,ActWith->Simplify]",
    "TensorValues[TRN]"
    },

    "See Also" ->
    {"MaxwellPotential", "FieldStrengthTensor"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Built in common Tensors",
      "Examples - Wave equations"
    }

}
