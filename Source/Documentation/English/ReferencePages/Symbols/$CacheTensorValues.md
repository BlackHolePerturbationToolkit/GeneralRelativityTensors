{
  "More Information" -> {
      "Simply re-setting $CacheTensorValues to False does not remove already cached values.",
      "Instead, call ClearCachedTensorValues."

  },

  "Basic Examples" -> {
    "$CacheTensorValues = True",
    "gK = ToMetric[\"Kerr\"]",
    "TensorValues[gK]",
    "CachedTensorValues[gK]",
    "$CacheTensorValues = False",
    "CachedTensorValues[gK]",
    "ClearCachedTensorValues[gK]",
    "CachedTensorValues[gK]"
    },

    "See Also" ->
    {"CachedTensorValues","ClearCachedTensorValues"}

}
