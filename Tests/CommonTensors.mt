gRN = ToMetric["ReissnerNordstrom"];
c1 = ToCurve["x1", gRN, {t[\[Chi]], (p M)/(1 + e Cos[\[Chi]]), \[Pi]/2, \[Phi][\[Chi]]}, \[Chi]];
gRNC = ToTensorOnCurve[gRN, c1];

ricSRN = RicciScalar[gRN];
ricSRNC = RicciScalar[gRNC];
einRN = EinsteinTensor[gRN];
einRNC = EinsteinTensor[gRNC];
weylRN = WeylTensor[gRN];
weylRNC = WeylTensor[gRNC];

Test[
	Simplify@TensorValues@ChristoffelSymbol[gRN],
	{{{0, (-Q^2 + M r)/(r (Q^2 + r (-2 M + r))), 0, 0}, {(-Q^2 + M r)/(
   r (Q^2 + r (-2 M + r))), 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
   0}}, {{((-Q^2 + M r) (Q^2 + r (-2 M + r)))/r^5, 0, 0, 0}, {0, (
   Q^2 - M r)/(Q^2 r - 2 M r^2 + r^3), 0, 0}, {0, 0, 
   2 M - (Q^2 + r^2)/r, 0}, {0, 0, 
   0, -(((Q^2 + r (-2 M + r)) Sin[\[Theta]]^2)/r)}}, {{0, 0, 0, 
   0}, {0, 0, 1/r, 0}, {0, 1/r, 0, 0}, {0, 0, 
   0, -Cos[\[Theta]] Sin[\[Theta]]}}, {{0, 0, 0, 0}, {0, 0, 0, 1/
   r}, {0, 0, 0, Cot[\[Theta]]}, {0, 1/r, Cot[\[Theta]], 0}}},
	TestID->"ChristoffelSymbol1"	
];

Test[
	Simplify@TensorValues@ChristoffelSymbol[gRNC],
	{{{0, -(((1 + e Cos[\[Chi]])^2 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M p (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
       e^2 Q^2 Cos[\[Chi]]^2))), 0, 
   0}, {-(((1 + e Cos[\[Chi]])^2 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M p (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
       e^2 Q^2 Cos[\[Chi]]^2))), 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
   0}}, {{-(((1 + e Cos[\[Chi]])^2 (-M^2 p + Q^2 + 
       e Q^2 Cos[\[Chi]]) (M^2 (-2 + p) p + Q^2 - 
       2 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[\[Chi]]^2))/(
    M^5 p^5)), 0, 0, 
   0}, {0, ((1 + e Cos[\[Chi]])^2 (-M^2 p + Q^2 + 
      e Q^2 Cos[\[Chi]]))/(
   M p (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
      e^2 Q^2 Cos[\[Chi]]^2)), 0, 0}, {0, 
   0, -(((1 + e Cos[\[Chi]]) (Q^2 + (
       M^2 p (-2 + p - 2 e Cos[\[Chi]]))/(1 + e Cos[\[Chi]])^2))/(
    M p)), 0}, {0, 0, 
   0, -(((1 + e Cos[\[Chi]]) (Q^2 + (
       M^2 p (-2 + p - 2 e Cos[\[Chi]]))/(1 + e Cos[\[Chi]])^2))/(
    M p))}}, {{0, 0, 0, 0}, {0, 0, (1 + e Cos[\[Chi]])/(M p), 
   0}, {0, (1 + e Cos[\[Chi]])/(M p), 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0,
    0}, {0, 0, 0, (1 + e Cos[\[Chi]])/(M p)}, {0, 0, 0, 0}, {0, (
   1 + e Cos[\[Chi]])/(M p), 0, 0}}},
	TestID->"ChristoffelSymbol2"	
];

Test[
	Simplify@TensorValues@RiemannTensor[gRN],
	{{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, (-3 Q^2 + 2 M r)/(r^2 (Q^2 + r (-2 M + r))), 0, 0}, {(
    3 Q^2 - 2 M r)/(r^2 (Q^2 + r (-2 M + r))), 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 0, 0}}, {{0, 0, (Q^2 - M r)/r^2, 0}, {0, 0, 0, 
    0}, {(-Q^2 + M r)/r^2, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 
    0, ((Q^2 - M r) Sin[\[Theta]]^2)/r^2}, {0, 0, 0, 0}, {0, 0, 0, 
    0}, {((-Q^2 + M r) Sin[\[Theta]]^2)/r^2, 0, 0, 
    0}}}, {{{0, (-3 Q^4 + Q^2 (8 M - 3 r) r + 2 M r^2 (-2 M + r))/r^6,
     0, 0}, {(3 Q^4 + 2 M (2 M - r) r^2 + Q^2 r (-8 M + 3 r))/r^6, 0, 
    0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, (
    Q^2 - M r)/r^2, 0}, {0, (-Q^2 + M r)/r^2, 0, 0}, {0, 0, 0, 
    0}}, {{0, 0, 0, 0}, {0, 0, 0, ((Q^2 - M r) Sin[\[Theta]]^2)/
    r^2}, {0, 0, 0, 0}, {0, ((-Q^2 + M r) Sin[\[Theta]]^2)/r^2, 0, 
    0}}}, {{{0, 0, ((Q^2 - M r) (Q^2 + r (-2 M + r)))/r^6, 0}, {0, 0, 
    0, 0}, {((-Q^2 + M r) (Q^2 + r (-2 M + r)))/r^6, 0, 0, 0}, {0, 0, 
    0, 0}}, {{0, 0, 0, 0}, {0, 0, (-Q^2 + M r)/(
    r^2 (Q^2 + r (-2 M + r))), 0}, {0, (Q^2 - M r)/(
    r^2 (Q^2 + r (-2 M + r))), 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 
    0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 
    0, 0, 0}, {0, 0, 0, ((-Q^2 + 2 M r) Sin[\[Theta]]^2)/r^2}, {0, 
    0, ((Q^2 - 2 M r) Sin[\[Theta]]^2)/r^2, 0}}}, {{{0, 0, 
    0, ((Q^2 - M r) (Q^2 + r (-2 M + r)))/r^6}, {0, 0, 0, 0}, {0, 0, 
    0, 0}, {((-Q^2 + M r) (Q^2 + r (-2 M + r)))/r^6, 0, 0, 0}}, {{0, 
    0, 0, 0}, {0, 0, 0, (-Q^2 + M r)/(r^2 (Q^2 + r (-2 M + r)))}, {0, 
    0, 0, 0}, {0, (Q^2 - M r)/(r^2 (Q^2 + r (-2 M + r))), 0, 0}}, {{0,
     0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, (Q^2 - 2 M r)/r^2}, {0, 
    0, -((Q^2 - 2 M r)/r^2), 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
    0, 0}, {0, 0, 0, 0}}}},
	TestID->"RiemannTensor1"	
];

Test[
	Simplify@TensorValues@RiemannTensor[gRNC],
	{{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, ((1 + e Cos[\[Chi]])^3 (2 M^2 p - 3 Q^2 - 
       3 e Q^2 Cos[\[Chi]]))/(
    M^2 p^2 (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
       e^2 Q^2 Cos[\[Chi]]^2)), 0, 
    0}, {((1 + e Cos[\[Chi]])^3 (-2 M^2 p + 3 Q^2 + 
       3 e Q^2 Cos[\[Chi]]))/(
    M^2 p^2 (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
       e^2 Q^2 Cos[\[Chi]]^2)), 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, 
    0, ((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^2 p^2), 0}, {0, 0, 0, 
    0}, {-(((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     M^2 p^2)), 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 
    0, ((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^2 p^2)}, {0, 0, 0, 0}, {0, 0, 0, 
    0}, {-(((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     M^2 p^2)), 0, 0, 
    0}}}, {{{0, -(((1 + e Cos[\[Chi]])^3 (-2 M^4 (-2 + p) p^2 + 
        M^2 p (-8 + 3 p) Q^2 + 3 Q^4 + 
        e (4 M^4 p^2 + M^2 p (-16 + 3 p) Q^2 + 9 Q^4) Cos[\[Chi]] + 
        e^2 Q^2 (-8 M^2 p + 9 Q^2) Cos[\[Chi]]^2 + 
        3 e^3 Q^4 Cos[\[Chi]]^3))/(M^6 p^6)), 0, 
    0}, {((1 + e Cos[\[Chi]])^3 (-2 M^4 (-2 + p) p^2 + 
       M^2 p (-8 + 3 p) Q^2 + 3 Q^4 + 
       e (4 M^4 p^2 + M^2 p (-16 + 3 p) Q^2 + 9 Q^4) Cos[\[Chi]] + 
       e^2 Q^2 (-8 M^2 p + 9 Q^2) Cos[\[Chi]]^2 + 
       3 e^3 Q^4 Cos[\[Chi]]^3))/(M^6 p^6), 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
    0, 0, 0}}, {{0, 0, 0, 0}, {0, 
    0, ((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^2 p^2), 
    0}, {0, -(((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + 
        e Q^2 Cos[\[Chi]]))/(M^2 p^2)), 0, 0}, {0, 0, 0, 0}}, {{0, 0, 
    0, 0}, {0, 0, 
    0, ((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^2 p^2)}, {0, 0, 0, 
    0}, {0, -(((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + 
        e Q^2 Cos[\[Chi]]))/(M^2 p^2)), 0, 0}}}, {{{0, 
    0, ((1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + 
       e Q^2 Cos[\[Chi]]) (M^2 (-2 + p) p + Q^2 - 
       2 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[\[Chi]]^2))/(
    M^6 p^6), 0}, {0, 0, 0, 
    0}, {-(((1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + 
        e Q^2 Cos[\[Chi]]) (M^2 (-2 + p) p + Q^2 - 
        2 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[\[Chi]]^2))/(
     M^6 p^6)), 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 
    0, -(((1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     M^2 p^2 (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
        e^2 Q^2 Cos[\[Chi]]^2))), 
    0}, {0, ((1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + 
       e Q^2 Cos[\[Chi]]))/(
    M^2 p^2 (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
       e^2 Q^2 Cos[\[Chi]]^2)), 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 
    0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 
    0, 0, 0}, {0, 0, 
    0, -(((1 + e Cos[\[Chi]]) (-2 M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     M^2 p^2))}, {0, 
    0, ((1 + e Cos[\[Chi]]) (-2 M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^2 p^2), 0}}}, {{{0, 0, 
    0, ((1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + 
       e Q^2 Cos[\[Chi]]) (M^2 (-2 + p) p + Q^2 - 
       2 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[\[Chi]]^2))/(
    M^6 p^6)}, {0, 0, 0, 0}, {0, 0, 0, 
    0}, {-(((1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + 
        e Q^2 Cos[\[Chi]]) (M^2 (-2 + p) p + Q^2 - 
        2 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[\[Chi]]^2))/(
     M^6 p^6)), 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 
    0, -(((1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     M^2 p^2 (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
        e^2 Q^2 Cos[\[Chi]]^2)))}, {0, 0, 0, 
    0}, {0, ((1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + 
       e Q^2 Cos[\[Chi]]))/(
    M^2 p^2 (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
       e^2 Q^2 Cos[\[Chi]]^2)), 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 
    0, ((1 + e Cos[\[Chi]]) (-2 M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^2 p^2)}, {0, 
    0, -(((1 + e Cos[\[Chi]]) (-2 M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     M^2 p^2)), 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0,
     0, 0}}}},
	TestID->"RiemannTensor2"	
];

Test[
	Simplify@TensorValues@RicciTensor[gRN],
	{{(Q^4 + Q^2 r (-2 M + r))/r^6, 0, 0, 
  0}, {0, -(Q^2/(r^2 (Q^2 + r (-2 M + r)))), 0, 0}, {0, 0, Q^2/r^2, 
  0}, {0, 0, 0, (Q^2 Sin[\[Theta]]^2)/r^2}},
	TestID->"RicciTensor1"	
];

Test[
	Simplify@TensorValues@RicciTensor[gRNC],
	{{(Q^2 (1 + e Cos[\[Chi]])^4 (M^2 (-2 + p) p + Q^2 - 
     2 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[\[Chi]]^2))/(
  M^6 p^6), 0, 0, 
  0}, {0, -((Q^2 (1 + e Cos[\[Chi]])^4)/(
   M^2 p^2 (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
      e^2 Q^2 Cos[\[Chi]]^2))), 0, 0}, {0, 
  0, (Q + e Q Cos[\[Chi]])^2/(M^2 p^2), 0}, {0, 0, 
  0, (Q + e Q Cos[\[Chi]])^2/(M^2 p^2)}},
	TestID->"RicciTensor2"	
];

Test[
	Simplify@TensorValues@RicciScalar[gRN],
	0,
	TestID->"RicciScalar1"	
];

Test[
	Simplify@TensorValues@RicciScalar[gRNC],
	0,
	TestID->"RicciScalar2"	
];

Test[
	Simplify@TensorValues@EinsteinTensor[gRN],
	{{(Q^2 (Q^2 + r (-2 M + r)))/r^6, 0, 0, 
  0}, {0, -(Q^2/(r^2 (Q^2 + r (-2 M + r)))), 0, 0}, {0, 0, Q^2/r^2, 
  0}, {0, 0, 0, (Q^2 Sin[\[Theta]]^2)/r^2}},
	TestID->"EinsteinTensor1"	
];

Test[
	Simplify@TensorValues@EinsteinTensor[gRNC],
	{{(Q^2 (1 + e Cos[\[Chi]])^4 (M^2 (-2 + p) p + Q^2 - 
     2 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[\[Chi]]^2))/(
  M^6 p^6), 0, 0, 
  0}, {0, -((Q^2 (1 + e Cos[\[Chi]])^4)/(
   M^2 p^2 (M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
      e^2 Q^2 Cos[\[Chi]]^2))), 0, 0}, {0, 
  0, (Q + e Q Cos[\[Chi]])^2/(M^2 p^2), 0}, {0, 0, 
  0, (Q + e Q Cos[\[Chi]])^2/(M^2 p^2)}},
	TestID->"EinsteinTensor2"	
];

Test[
	Simplify@TensorValues@WeylTensor[gRN],
	{{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, (
    2 (Q^2 - M r))/r^4, 0, 0}, {(2 (-Q^2 + M r))/r^4, 0, 0, 0}, {0, 0,
     0, 0}, {0, 0, 0, 0}}, {{0, 
    0, -((Q^4 + M (2 M - r) r^2 + Q^2 r (-3 M + r))/r^4), 0}, {0, 0, 
    0, 0}, {(Q^4 + M (2 M - r) r^2 + Q^2 r (-3 M + r))/r^4, 0, 0, 
    0}, {0, 0, 0, 0}}, {{0, 0, 
    0, ((-Q^2 + M r) (Q^2 + r (-2 M + r)) Sin[\[Theta]]^2)/r^4}, {0, 
    0, 0, 0}, {0, 0, 0, 
    0}, {((Q^2 - M r) (Q^2 + r (-2 M + r)) Sin[\[Theta]]^2)/r^4, 0, 0,
     0}}}, {{{0, (2 (-Q^2 + M r))/r^4, 0, 0}, {(2 (Q^2 - M r))/r^4, 0,
     0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, (
    Q^2 - M r)/(Q^2 - 2 M r + r^2), 0}, {0, (-Q^2 + M r)/(
    Q^2 + r (-2 M + r)), 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 
    0, ((Q^2 - M r) Sin[\[Theta]]^2)/(Q^2 + r (-2 M + r))}, {0, 0, 0, 
    0}, {0, -(((Q^2 - M r) Sin[\[Theta]]^2)/(Q^2 + r (-2 M + r))), 0, 
    0}}}, {{{0, 0, (Q^4 + M (2 M - r) r^2 + Q^2 r (-3 M + r))/r^4, 
    0}, {0, 0, 0, 
    0}, {-((Q^4 + M (2 M - r) r^2 + Q^2 r (-3 M + r))/r^4), 0, 0, 
    0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, (-Q^2 + M r)/(
    Q^2 + r (-2 M + r)), 0}, {0, (Q^2 - M r)/(Q^2 - 2 M r + r^2), 0, 
    0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
    0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
    0, -2 (Q^2 - M r) Sin[\[Theta]]^2}, {0, 0, 
    2 (Q^2 - M r) Sin[\[Theta]]^2, 0}}}, {{{0, 0, 
    0, ((Q^2 - M r) (Q^2 + r (-2 M + r)) Sin[\[Theta]]^2)/r^4}, {0, 0,
     0, 0}, {0, 0, 0, 
    0}, {((-Q^2 + M r) (Q^2 + r (-2 M + r)) Sin[\[Theta]]^2)/r^4, 0, 
    0, 0}}, {{0, 0, 0, 0}, {0, 0, 
    0, -(((Q^2 - M r) Sin[\[Theta]]^2)/(Q^2 + r (-2 M + r)))}, {0, 0, 
    0, 0}, {0, ((Q^2 - M r) Sin[\[Theta]]^2)/(Q^2 + r (-2 M + r)), 0, 
    0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 
    2 (Q^2 - M r) Sin[\[Theta]]^2}, {0, 
    0, -2 (Q^2 - M r) Sin[\[Theta]]^2, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}},
	TestID->"WeylTensor1"	
];

Test[
	Simplify@TensorValues@WeylTensor[gRNC],
	{{{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, (
    2 (1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^4 p^4), 0, 
    0}, {-((2 (1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + 
        e Q^2 Cos[\[Chi]]))/(M^4 p^4)), 0, 0, 0}, {0, 0, 0, 0}, {0, 0,
     0, 0}}, {{0, 
    0, -(((1 + e Cos[\[Chi]]) (-M^4 (-2 + p) p^2 + 
        M^2 (-3 + p) p Q^2 + Q^4 + 
        e (2 M^4 p^2 + M^2 (-6 + p) p Q^2 + 3 Q^4) Cos[\[Chi]] + 
        3 e^2 Q^2 (-M^2 p + Q^2) Cos[\[Chi]]^2 + 
        e^3 Q^4 Cos[\[Chi]]^3))/(M^4 p^4)), 0}, {0, 0, 0, 
    0}, {((1 + e Cos[\[Chi]]) (-M^4 (-2 + p) p^2 + 
       M^2 (-3 + p) p Q^2 + Q^4 + 
       e (2 M^4 p^2 + M^2 (-6 + p) p Q^2 + 3 Q^4) Cos[\[Chi]] + 
       3 e^2 Q^2 (-M^2 p + Q^2) Cos[\[Chi]]^2 + 
       e^3 Q^4 Cos[\[Chi]]^3))/(M^4 p^4), 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, 0, 
    0, -(((1 + e Cos[\[Chi]]) (-M^4 (-2 + p) p^2 + 
        M^2 (-3 + p) p Q^2 + Q^4 + 
        e (2 M^4 p^2 + M^2 (-6 + p) p Q^2 + 3 Q^4) Cos[\[Chi]] + 
        3 e^2 Q^2 (-M^2 p + Q^2) Cos[\[Chi]]^2 + 
        e^3 Q^4 Cos[\[Chi]]^3))/(M^4 p^4))}, {0, 0, 0, 0}, {0, 0, 0, 
    0}, {((1 + e Cos[\[Chi]]) (-M^4 (-2 + p) p^2 + 
       M^2 (-3 + p) p Q^2 + Q^4 + 
       e (2 M^4 p^2 + M^2 (-6 + p) p Q^2 + 3 Q^4) Cos[\[Chi]] + 
       3 e^2 Q^2 (-M^2 p + Q^2) Cos[\[Chi]]^2 + 
       e^3 Q^4 Cos[\[Chi]]^3))/(M^4 p^4), 0, 0, 
    0}}}, {{{0, -((
     2 (1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     M^4 p^4)), 0, 0}, {(
    2 (1 + e Cos[\[Chi]])^3 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^4 p^4), 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 
    0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 
    0, (2 (1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + 
       e Q^2 Cos[\[Chi]]))/(-4 M^2 p + 2 M^2 p^2 + 2 Q^2 + e^2 Q^2 - 
     4 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[2 \[Chi]]), 
    0}, {0, -((
     2 (1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + 
        e Q^2 Cos[\[Chi]]))/(-4 M^2 p + 2 M^2 p^2 + 2 Q^2 + e^2 Q^2 - 
      4 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[2 \[Chi]])), 0, 
    0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, (
    2 (1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + 
       e Q^2 Cos[\[Chi]]))/(-4 M^2 p + 2 M^2 p^2 + 2 Q^2 + e^2 Q^2 - 
     4 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[2 \[Chi]])}, {0, 0, 
    0, 0}, {0, -((
     2 (1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + 
        e Q^2 Cos[\[Chi]]))/(-4 M^2 p + 2 M^2 p^2 + 2 Q^2 + e^2 Q^2 - 
      4 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[2 \[Chi]])), 0, 
    0}}}, {{{0, 
    0, ((1 + e Cos[\[Chi]]) (-M^4 (-2 + p) p^2 + M^2 (-3 + p) p Q^2 + 
       Q^4 + e (2 M^4 p^2 + M^2 (-6 + p) p Q^2 + 3 Q^4) Cos[\[Chi]] + 
       3 e^2 Q^2 (-M^2 p + Q^2) Cos[\[Chi]]^2 + 
       e^3 Q^4 Cos[\[Chi]]^3))/(M^4 p^4), 0}, {0, 0, 0, 
    0}, {-(((1 + e Cos[\[Chi]]) (-M^4 (-2 + p) p^2 + 
        M^2 (-3 + p) p Q^2 + Q^4 + 
        e (2 M^4 p^2 + M^2 (-6 + p) p Q^2 + 3 Q^4) Cos[\[Chi]] + 
        3 e^2 Q^2 (-M^2 p + Q^2) Cos[\[Chi]]^2 + 
        e^3 Q^4 Cos[\[Chi]]^3))/(M^4 p^4)), 0, 0, 0}, {0, 0, 0, 
    0}}, {{0, 0, 0, 0}, {0, 
    0, -(((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
      e^2 Q^2 Cos[\[Chi]]^2)), 
    0}, {0, ((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
     e^2 Q^2 Cos[\[Chi]]^2), 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 
    0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 
    0}, {0, 0, 
    0, -((2 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     1 + e Cos[\[Chi]]))}, {0, 0, (
    2 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(1 + e Cos[\[Chi]]), 
    0}}}, {{{0, 0, 
    0, ((1 + e Cos[\[Chi]]) (-M^4 (-2 + p) p^2 + M^2 (-3 + p) p Q^2 + 
       Q^4 + e (2 M^4 p^2 + M^2 (-6 + p) p Q^2 + 3 Q^4) Cos[\[Chi]] + 
       3 e^2 Q^2 (-M^2 p + Q^2) Cos[\[Chi]]^2 + 
       e^3 Q^4 Cos[\[Chi]]^3))/(M^4 p^4)}, {0, 0, 0, 0}, {0, 0, 0, 
    0}, {-(((1 + e Cos[\[Chi]]) (-M^4 (-2 + p) p^2 + 
        M^2 (-3 + p) p Q^2 + Q^4 + 
        e (2 M^4 p^2 + M^2 (-6 + p) p Q^2 + 3 Q^4) Cos[\[Chi]] + 
        3 e^2 Q^2 (-M^2 p + Q^2) Cos[\[Chi]]^2 + 
        e^3 Q^4 Cos[\[Chi]]^3))/(M^4 p^4)), 0, 0, 0}}, {{0, 0, 0, 
    0}, {0, 0, 
    0, -(((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
     M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
      e^2 Q^2 Cos[\[Chi]]^2))}, {0, 0, 0, 
    0}, {0, ((1 + e Cos[\[Chi]]) (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    M^2 (-2 + p) p + Q^2 - 2 e (M^2 p - Q^2) Cos[\[Chi]] + 
     e^2 Q^2 Cos[\[Chi]]^2), 0, 0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 
    0, 0, (2 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(
    1 + e Cos[\[Chi]])}, {0, 
    0, -((2 (-M^2 p + Q^2 + e Q^2 Cos[\[Chi]]))/(1 + e Cos[\[Chi]])), 
    0}}, {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}}},
	TestID->"WeylTensor2"	
];

ARN = MaxwellPotential["ReissnerNordstrom"];
ARNC = ToTensorOnCurve[ARN,c1];
FRN = FieldStrengthTensor[ARN];
FRNC = FieldStrengthTensor[ARNC];
seRN = MaxwellStressEnergyTensor[FRN];
seRNC = MaxwellStressEnergyTensor[FRNC];

Test[
	Simplify@TensorValues@ARN,
	{Q/r, 0, 0, 0},
	TestID->"MaxwellPotential1"	
];

Test[
	Simplify@TensorValues@ARNC,
	{(Q + e Q Cos[\[Chi]])/(M p), 0, 0, 0},
	TestID->"MaxwellPotential2"	
];

Test[
	Simplify@TensorValues@FRN,
	{{0, -(Q/r^2), 0, 0}, {Q/r^2, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}},
	TestID->"FieldStrengthTensor1"	
];

Test[
	Simplify@TensorValues@FRNC,
	{{0, -((Q (1 + e Cos[\[Chi]])^2)/(M^2 p^2)), 0, 0}, {(
  Q (1 + e Cos[\[Chi]])^2)/(M^2 p^2), 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 
  0, 0}},
	TestID->"FieldStrengthTensor2"	
];

Test[
	Simplify@TensorValues@seRN,
	{{Q^2/(8 \[Pi] r^2 (Q^2 + r (-2 M + r))), 0, 0, 
  0}, {0, -((Q^2 (Q^2 + r (-2 M + r)))/(8 \[Pi] r^6)), 0, 0}, {0, 0, 
  Q^2/(8 \[Pi] r^6), 0}, {0, 0, 0, (Q^2 Csc[\[Theta]]^2)/(
  8 \[Pi] r^6)}},
	TestID->"MaxwellStressEnergyTensor1"	
];

Test[
	Simplify@TensorValues@seRNC,
	{{(Q^2 (1 + e Cos[\[Chi]])^4)/(
  8 M^2 p^2 \[Pi] (M^2 (-2 + p) p + Q^2 - 
     2 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[\[Chi]]^2)), 0, 0, 
  0}, {0, -((
   Q^2 (1 + e Cos[\[Chi]])^4 (M^2 (-2 + p) p + Q^2 - 
      2 e (M^2 p - Q^2) Cos[\[Chi]] + e^2 Q^2 Cos[\[Chi]]^2))/(
   8 M^6 p^6 \[Pi])), 0, 0}, {0, 0, (Q^2 (1 + e Cos[\[Chi]])^6)/(
  8 M^6 p^6 \[Pi]), 0}, {0, 0, 0, (Q^2 (1 + e Cos[\[Chi]])^6)/(
  8 M^6 p^6 \[Pi])}},
	TestID->"MaxwellStressEnergyTensor2"	
];

Test[
	Simplify@TensorValues@MergeTensors[einRN[-\[Alpha], -\[Beta]] - 8 \[Pi] seRN[-\[Alpha], -\[Beta]]],
	{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}},
	TestID->"EinsteinEquations1"	
];

Test[
	Simplify@TensorValues@MergeTensors[einRNC[-\[Alpha], -\[Beta]] - 8 \[Pi] seRNC[-\[Alpha], -\[Beta]]],
	{{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}},
	TestID->"EinsteinEquations2"	
];

Test[
	Simplify@TensorValues@KretschmannScalar[gRN],
	(8 (7 Q^4 - 12 M Q^2 r + 6 M^2 r^2))/r^8,
	TestID->"KretschmannScalar1"	
];

Test[
	Simplify@TensorValues@KretschmannScalar[gRNC],
	(8 (1 + e Cos[\[Chi]])^6 (6 M^4 p^2 - 12 M^2 p Q^2 + 7 Q^4 + 
   2 e Q^2 (-6 M^2 p + 7 Q^2) Cos[\[Chi]] + 
   7 e^2 Q^4 Cos[\[Chi]]^2))/(M^8 p^8),
	TestID->"KretschmannScalar2"	
];
