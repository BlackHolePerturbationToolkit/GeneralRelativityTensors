gK = ToMetric["Kerr"];
gRN = ToMetric["ReissnerNordstrom"];
rieRN = RiemannTensor[gRN];
ricTRN = RicciTensor[gRN];
ricSRN = RicciScalar[gRN];
uS = FourVelocityVector["SchwarzschildGeneric"];

VerificationTest[
	Simplify[TensorValues@ShiftIndices[gK,{\[Alpha], \[Beta]}]-TensorValues@InverseMetric[gK]],
	{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}},
	TestID->"ShiftIndices1"	
]

VerificationTest[
	Simplify[TensorValues[gK[\[Alpha], -\[Beta]]]],
	{{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}},
	TestID->"ShiftIndices2"	
]

VerificationTest[
	Simplify[Component[gK,{-t,-t}]-(-((a^2 - 2 M r + r^2 - a^2 Sin[\[Theta]]^2)/(
 r^2 + a^2 Cos[\[Theta]]^2)))],
	0,
	TestID->"Component1"	
]

VerificationTest[
	Simplify[gK[t,t] - (-((a^4 + 2 r^4 + a^2 r (2 M + 3 r) + 
  a^2 (a^2 + r (-2 M + r)) Cos[2 \[Theta]])/((a^2 + 
    r (-2 M + r)) (a^2 + 2 r^2 + a^2 Cos[2 \[Theta]]))))],
	0,
	TestID->"Component2"	
]

VerificationTest[
	Simplify[TensorValues@ContractIndices[gK[\[Alpha], -\[Alpha]]]],
	4,
	TestID->"ContractIndices"	
]

VerificationTest[
	Simplify[TensorValues@MultiplyTensors[uS[\[Alpha]], uS[\[Beta]]]
	-
	{{\[ScriptCapitalE]^2/(1 - (2 M)/
    r[\[Tau]])^2, (\[ScriptCapitalE] r[\[Tau]] \
Sqrt[\[ScriptCapitalE]^2 - ((-2 M + r[\[Tau]]) (\[ScriptCapitalL]^2 + 
       r[\[Tau]]^2))/r[\[Tau]]^3])/(-2 M + r[\[Tau]]), 
  0, -((\[ScriptCapitalE] \[ScriptCapitalL])/(
   2 M r[\[Tau]] - 
    r[\[Tau]]^2))}, {(\[ScriptCapitalE] r[\[Tau]] Sqrt[\
\[ScriptCapitalE]^2 - ((-2 M + r[\[Tau]]) (\[ScriptCapitalL]^2 + 
       r[\[Tau]]^2))/r[\[Tau]]^3])/(-2 M + 
   r[\[Tau]]), \[ScriptCapitalE]^2 - ((-2 M + 
      r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/r[\[Tau]]^3, 
  0, (\[ScriptCapitalL] Sqrt[\[ScriptCapitalE]^2 - ((-2 M + 
       r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/r[\[Tau]]^3])/
  r[\[Tau]]^2}, {0, 0, 0, 
  0}, {-((\[ScriptCapitalE] \[ScriptCapitalL])/(
   2 M r[\[Tau]] - 
    r[\[Tau]]^2)), (\[ScriptCapitalL] Sqrt[\[ScriptCapitalE]^2 - ((-2 \
M + r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/r[\[Tau]]^3])/
  r[\[Tau]]^2, 0, \[ScriptCapitalL]^2/r[\[Tau]]^4}}],
	{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}},
	TestID->"MultiplyTensors"	
]

VerificationTest[
	Simplify[TensorValues@AddTensors[gK, gK]-TensorValues@MultiplyTensorScalar[2, gK]],
	{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}},
	TestID->"MultiplyTensorScalar-AddTensors"	
]

VerificationTest[
	Simplify[2 TensorValues@uS - TensorValues@MultiplyTensorScalar[2, uS]],
	{0, 0, 0, 0},
	TestID->"MultiplyTensorScalar"	
]

VerificationTest[
	Simplify[TensorValues@MergeTensors[ricTRN[-\[Alpha], -\[Beta]] - gRN[-\[Alpha], -\[Beta]] ricSRN/2]
	-
	{{(Q^2 (Q^2 + r (-2 M + r)))/r^6, 0, 0, 
  0}, {0, -(Q^2/(r^2 (Q^2 + r (-2 M + r)))), 0, 0}, {0, 0, Q^2/r^2, 
  0}, {0, 0, 0, (Q^2 Sin[\[Theta]]^2)/r^2}}],
	{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}},
	TestID->"MergeTensors1"	
]

VerificationTest[
	Simplify[TensorValues@MergeTensors[uS[\[Alpha]] uS[\[Beta]]]
	-
	{{\[ScriptCapitalE]^2/(1 - (2 M)/
    r[\[Tau]])^2, (\[ScriptCapitalE] r[\[Tau]] \
Sqrt[\[ScriptCapitalE]^2 - ((-2 M + r[\[Tau]]) (\[ScriptCapitalL]^2 + 
       r[\[Tau]]^2))/r[\[Tau]]^3])/(-2 M + r[\[Tau]]), 
  0, -((\[ScriptCapitalE] \[ScriptCapitalL])/(
   2 M r[\[Tau]] - 
    r[\[Tau]]^2))}, {(\[ScriptCapitalE] r[\[Tau]] Sqrt[\
\[ScriptCapitalE]^2 - ((-2 M + r[\[Tau]]) (\[ScriptCapitalL]^2 + 
       r[\[Tau]]^2))/r[\[Tau]]^3])/(-2 M + 
   r[\[Tau]]), \[ScriptCapitalE]^2 - ((-2 M + 
      r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/r[\[Tau]]^3, 
  0, (\[ScriptCapitalL] Sqrt[\[ScriptCapitalE]^2 - ((-2 M + 
       r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/r[\[Tau]]^3])/
  r[\[Tau]]^2}, {0, 0, 0, 
  0}, {-((\[ScriptCapitalE] \[ScriptCapitalL])/(
   2 M r[\[Tau]] - 
    r[\[Tau]]^2)), (\[ScriptCapitalL] Sqrt[\[ScriptCapitalE]^2 - ((-2 \
M + r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/r[\[Tau]]^3])/
  r[\[Tau]]^2, 0, \[ScriptCapitalL]^2/r[\[Tau]]^4}}],
	{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}},
	TestID->"MergeTensors2"	
]

VerificationTest[
	Simplify[TensorValues@
   ReorderTensorIndices[
    rieRN[-\[Alpha], -\[Beta], -\[Gamma], -\[Delta]], {2, 1, 3, 4}] + 
  TensorValues@rieRN[-\[Alpha], -\[Beta], -\[Gamma], -\[Delta]]],
	{{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0,
     0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0,
     0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
    0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
    0, 0, 0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}},
	TestID->"ReorderTensorIndices"	
]
