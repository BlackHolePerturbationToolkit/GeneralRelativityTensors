{
  "More Information" -> {
      "Within the List of PossibleIndices, it is easier to change the Indices of a tensor using DownValues.",
      "When changing the Indices to Symbols outside the List of PossibleIndices, use IgnoreWarnings->True."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "Coordinates[gK]",
    "SetIndices[gK,{-\[Gamma],-\[Delta]}]",
    "gK[-\[Gamma],-\[Delta]]",
    "gKLatin = SetPossibleIndices[SetIndices[gK,{-a,-b},IgnoreWarnings->True],{a,b,c,d,e,f,g,h,i,j}]",
    "Indices[gKLatin]",
    "PossibleIndices[gKLatin]"
    },

    "See Also" ->
    {"SetTensorKeyValue","Indices","PossibleIndices","SetPossibleIndices"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
