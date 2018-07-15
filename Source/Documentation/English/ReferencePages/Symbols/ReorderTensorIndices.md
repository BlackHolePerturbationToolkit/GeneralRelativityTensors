{
  "More Information" -> {
      "It is generally simpler to call MergeTensors on an expression than to call \
ReorderTensorIndices on individual terms."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "rieS = RiemannTensor[gS, \"ActWith\" -> Simplify]",
    "rieSSwap = ReorderTensorIndices[rieS[-\[Alpha], -\[Beta], -\[Gamma], -\[Delta]], {3, 4, 1, 2}]",
    "TensorValues[rieSSwap] - TensorValues@rieS[-\[Alpha], -\[Beta], -\[Gamma], -\[Delta]] // Simplify",
    "MergeTensors[rieS[-\[Alpha], -\[Beta], -\[Gamma], -\[Delta]] -
   rieS[-\[Gamma], -\[Delta], -\[Alpha], -\[Beta]], \"ActWith\" -> Simplify] // TensorValues"
    },

    "See Also" ->
    {"MergeTensors","ContractIndices","ShiftIndices"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Manipulating and differentiating Tensors"
    }

}
