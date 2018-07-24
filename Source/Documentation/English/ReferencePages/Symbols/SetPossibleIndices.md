{
  "More Information" -> {
      "When changing the PossibleIndices to a List of Symbols that does not include Indices, use IgnoreWarnings->True."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "Coordinates[gK]",
    "gKLatin = SetIndices[SetPossibleIndices[gK,{a,b,c,d,e,f,g,h,i,j},IgnoreWarnings->True],{-a,-b}]",
    "Indices[gKLatin]",
    "PossibleIndices[gKLatin]"
    },

    "See Also" ->
    {"SetTensorKeyValue","Indices","PossibleIndices","SetIndices"},

    "Tutorials" -> {
      "Introduction to GeneralRelativityTensors",
      "Caching Tensor values",
      "Low level Tensor operations"
    }

}
