{
  "More Information" -> {
  },

  "Basic Examples" -> {
      "uS = FourVelocityVector[\"SchwarzschildGeneric\"]",
      "TensorValues[uS]",
      "OnCurveQ[uS]",
      "uSq = MergeTensors[uS[\[Alpha]]uS[-\[Alpha]],ActWith->Simplify]",
      "TensorValues[uSq]"
    },

    "See Also" ->
    {"Curve","OnCurveQ","ToCurve","ToTensorOnCurve","ToTensorFieldOnCurve"},

    "Tutorials" -> {
        "Introduction to GeneralRelativityTensors",
        "Introduction to Tensor Curves"
    }


}
