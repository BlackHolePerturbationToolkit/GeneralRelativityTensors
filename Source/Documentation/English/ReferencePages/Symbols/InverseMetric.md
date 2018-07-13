{
  "More Information" -> {
   "Usually it is easier to raise the indices of a metric with ShiftIndices (or DownValues), \
which calls InverseMetric internally."
  },

  "Basic Examples" -> {
      "gS = ToMetric[\"Schwarzschild\"]",
      "gSI = InverseMetric[gS]",
      "gSI // TensorValues",
      "gS[\[Alpha],\[Beta]] // TensorValues"
    },

    "See Also" ->
    {"ToMetric","Metric","MetricQ"}

}
