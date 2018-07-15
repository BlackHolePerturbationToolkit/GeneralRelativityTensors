{
  "More Information" -> {
      "It is generally simpler to call MergeTensors than to call ContractIndices (which MergeTensors does internally)."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "gS[\[Alpha], -\[Alpha]]",
    "gST = ContractIndices[gS[\[Alpha], -\[Alpha]]]",
    "gST // TensorValues // Simplify"
    },

    "See Also" ->
    {"MergeTensors","MultiplyTensors","MultiplyTensorScalar","AddTensors"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Manipulating and differentiating Tensors"
    }

}
