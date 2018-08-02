{
  "More Information" -> {
      "Indices will fail with an error message and abort if not all the terms in an expression \
have the same indices.",
    "Use IndicesTraced on an expression to get a unique list."
  },

  "Basic Examples" -> {
    "gS = ToMetric[\"Schwarzschild\"]",
    "Indices[gS]",
    "ricS = RicciTensor[gS]",
    "Indices[gS[\[Alpha],\[Beta]]+ricS[\[Alpha],\[Beta]]]"
    },

    "See Also" ->
    {"PossibleIndices","IndicesTraced","Rank","IndexPositions","Coordinates","SpacetimeDimensions"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors"
    }

}
