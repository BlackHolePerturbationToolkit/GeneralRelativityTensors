{
  "More Information" -> {
      "DownValues of Tensors call ShiftIndices if the values are from the list of PossibleIndices.",
      "It is usually easier to raise and lower indices with DownValues.",
      "In order to act with a function on the values using the Option \"ActWith\", \
call ShiftIndices explicitly, not with DownValues."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "ShiftIndices[gK, {\[Alpha], \[Beta]}]",
    "gK[\[Alpha], \[Beta]]",
    "gK[\[Alpha], -\[Beta]] // TensorValues",
    "ShiftIndices[gK, {\[Alpha], -\[Beta]}, \"ActWith\" -> Simplify] // TensorValues"
    },

    "See Also" ->
    {"Component","TensorValues"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Manipulating and differentiating Tensors"
    }

}
