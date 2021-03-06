gK = ToMetric["Kerr"];
gRN = ToMetric["ReissnerNordstrom"];
rieRN = RiemannTensor[gRN];
ricTRN = RicciTensor[gRN];
ricSRN = RicciScalar[gRN];
uS = FourVelocityVector["SchwarzschildGeneric"];

VerificationTest[
	Simplify[TensorValues[D[gRN[\[Alpha], \[Beta]], -\[Alpha]]]-{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{(
   2 r (-Q^2 + M r))/(Q^2 + r (-2 M + r))^2, 0, 0, 0}, {0, (
   2 (-Q^2 + M r))/r^3, 0, 0}, {0, 0, -(2/r^3), 0}, {0, 0, 
   0, -((2 Csc[\[Theta]]^2)/r^3)}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
   0, 0, 0}, {0, 0, 
   0, -((2 Cot[\[Theta]] Csc[\[Theta]]^2)/r^2)}}, {{0, 0, 0, 0}, {0, 
   0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}],
	{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 
   0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 
   0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
   0}, {0, 0, 0, 0}, {0, 0, 0, 0}}},
	TestID->"Tensor-D1"	
]

VerificationTest[
	Simplify[TensorValues[D[uS, \[Tau]]]
	-
	{-((2 M \[ScriptCapitalE] Derivative[1][r][\[Tau]])/(-2 M + 
    r[\[Tau]])^2), -(((3 M \[ScriptCapitalL]^2 - \[ScriptCapitalL]^2 \
r[\[Tau]] + M r[\[Tau]]^2) Derivative[1][r][\[Tau]])/(
  r[\[Tau]]^4 Sqrt[\[ScriptCapitalE]^2 - ((-2 M + 
       r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/
    r[\[Tau]]^3])), 0, -((
  2 \[ScriptCapitalL] Derivative[1][r][\[Tau]])/r[\[Tau]]^3)}],
	{0, 0, 0, 0},
	TestID->"Tensor-D2"	
]

VerificationTest[
	Simplify[TensorValues@MergeTensors@CovariantD[gRN,-\[Gamma]]],
	{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 
   0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 
   0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
   0}, {0, 0, 0, 0}, {0, 0, 0, 0}}},
	TestID->"Tensor-CovariantD1"	
]

VerificationTest[
	Simplify[TensorValues@MergeTensors@CovariantD[uS,uS]-
	{(2 M \[ScriptCapitalE] (Sqrt[\[ScriptCapitalE]^2 - ((-2 M + 
        r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/r[\[Tau]]^3] -
     Derivative[1][r][\[Tau]]))/(-2 M + 
   r[\[Tau]])^2, ((3 M \[ScriptCapitalL]^2 - \[ScriptCapitalL]^2 r[\
\[Tau]] + 
    M r[\[Tau]]^2) (Sqrt[\[ScriptCapitalE]^2 - ((-2 M + 
        r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/r[\[Tau]]^3] -
     Derivative[1][r][\[Tau]]))/(
 r[\[Tau]]^4 Sqrt[\[ScriptCapitalE]^2 - ((-2 M + 
      r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/
   r[\[Tau]]^3]), 0, (
 2 \[ScriptCapitalL] (Sqrt[\[ScriptCapitalE]^2 - ((-2 M + 
        r[\[Tau]]) (\[ScriptCapitalL]^2 + r[\[Tau]]^2))/r[\[Tau]]^3] -
     Derivative[1][r][\[Tau]]))/r[\[Tau]]^3}],
	{0, 0, 0, 0},
	TestID->"Tensor-CovariantD2"	
]

VerificationTest[
	Simplify[TensorValues[MergeTensors[BianchiIdentities[gRN,0]]]],
	{{{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 
     0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 
     0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0,
      0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}, {{{0, 0, 0, 0}, {0, 0, 
     0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
     0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0,
      0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}, {0, 0, 0, 0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0,
      0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}}, {{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}}, {{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}}, {{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}}, {{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
     0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}}},
	TestID->"Bianchi0"	
]

VerificationTest[
	Simplify[TensorValues[MergeTensors[BianchiIdentities[gRN,1]]]],
	{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 
   0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 
   0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
   0}, {0, 0, 0, 0}, {0, 0, 0, 0}}},
	TestID->"Bianchi1"
]

VerificationTest[
	Simplify[TensorValues[MergeTensors[BianchiIdentities[gRN,2]]]],
	{0, 0, 0, 0},
	TestID->"Bianchi2"
]
