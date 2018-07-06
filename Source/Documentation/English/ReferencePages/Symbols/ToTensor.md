{
  "More Information" -> {
      "The name n can be a two element List. The first element is the actual \"TensorName\" (used for internal purposes) \
while the second is the \"TensorDisplayName\" which is used for formatted output in a notebook."
  },

  "Basic Examples" -> {
    "gK = ToMetric[\"Kerr\"]",
    "t1 = ToTensor[\"t1\", gK, {g[t], h[r], k[r], s[\[Phi]]}]",
    "TensorValues[t1] // TableForm",
    "t2 = ToTensor[\"t2\", gK, {g[t], h[r], k[r], s[\[Phi]]}, {-\[Alpha]}]"
    },

    "See Also" ->
    {"ToMetric","ToCurve","ToTensorOnCurve","TensorValues"}

}
