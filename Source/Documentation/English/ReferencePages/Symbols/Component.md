{
  "More Information" -> {
      "DownValues of Tensors call Component if the values are from the list of Coordinates.",
      "It is usually easier to raise and lower indices with DownValues.",
      "In order to act with a function on the values using the Option ActWith, \
call Component explicitly, not with DownValues."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "Component[gK, {t, t}]",
    "gK[t, t]",
    "gK[t, -t]",
    "Component[gK, {t, -t}, \"ActWith\" -> Simplify]"
    },

    "See Also" ->
    {"ShiftIndices","TensorValues"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Manipulating and differentiating Tensors"
    }

}
