(* ::Package:: *)

BeginPackage["Tensors`TensorDerivatives`",{"Tensors`TensorDefinitions`"}];


TensorDerivatives`Private`str[a_String]:=ToString[Style[a,Italic],TraditionalForm]
TensorDerivatives`Private`str[args___]:=StringRiffle[TensorDerivatives`Private`str/@{args},","]


ChristoffelSymbol::usage="ChristoffelSymbol["<>TensorDerivatives`Private`str["m"]<>"] returns the Christoffel \
symbol computed from the metric Tensor "<>TensorDerivatives`Private`str["m"]<>".";
CovariantD::usage="CovariantD["<>TensorDerivatives`Private`str["t","ind"]<>"] returns the covariant derivative of tensor \
"<>TensorDerivatives`Private`str["t"]<>" with respect to the index "<>TensorDerivatives`Private`str["ind"]<>" as a sum and product of tensors.
CovariantD["<>TensorDerivatives`Private`str["t","param"]<>"] returns the covariant derivative of tensor \
"<>TensorDerivatives`Private`str["t"]<>" which parametrized values on a curve with respect to the curve \
parameter "<>TensorDerivatives`Private`str["param"]<>".";


Begin["`Private`"];


Options[ChristoffelSymbol]={"SimplifyFunction"->Identity};
Tensor/:ChristoffelSymbol[t_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{n,g,ig,xx,vals,posInds,gT,name,simpFn},
	simpFn=OptionValue["SimplifyFunction"];
	gT=Metric[t];
	xx=Coordinates[gT];
	posInds=PossibleIndices[gT];
	n=Dimensions[gT];
	g=RawTensorValues[gT];
	ig=RawTensorValues@InverseMetric[gT];
	name="ChristoffelSymbol"<>TensorName[t];

	vals=
		If[RawTensorValues[name,{"Up","Down","Down"}]===Undefined,
			simpFn@Table[(1/2)Sum[ig[[i,s]](-D[g[[j,k]],xx[[s]]]+D[g[[j,s]],xx[[k]]]+D[g[[s,k]],xx[[j]]]),{s,1,n}],{i,1,n},{j,1,n},{k,1,n}],
			RawTensorValues[name,{"Up","Down","Down"}]
		];

	ToTensor[Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","IsMetric","Indices"}],
			Association["Metric"->gT,
						"IsMetric"->False,
						"Values"->vals,
						"DisplayName"->"\[CapitalGamma]",
						"Name"->name,
						"Indices"->{posInds[[1]],-posInds[[2]],-posInds[[3]]}]]]
]


Clear[validateDerivativeIndices]
validateDerivativeIndices[inds1_List,inds2_List]:=
Module[{indsUp,repeatedInds,inds,toCov},

	toCov[expr_]:=expr/.-sym_Symbol:>sym;
	inds=Join[inds1,inds2];
	indsUp=toCov[inds];
	repeatedInds=Cases[inds,#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp]);

	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];
		Abort[]
	];

	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],toCov[#[[1]]],##&[]]&/@repeatedInds];
		Abort[]
	];
]


Tensor/:D[t1_Tensor,-a_Symbol] :=
Module[{vals,inds,repeatedInds,tvs,dims,itrs,indsLocal,local,indsFinal,coords},

	inds[1]={-a};
	inds[2]=Indices[t1];
	validateDerivativeIndices[inds[1],inds[2]];
	
	local[sym_]:=If[MatchQ[sym,-_Symbol],Symbol["cov"<>ToString[-sym]],Symbol["con"<>ToString[sym]]];
	indsLocal[1]=local/@inds[1];
	indsLocal[2]=local/@inds[2];
	indsLocal["Tot"]=Join[indsLocal[1],indsLocal[2]];
	indsFinal=indsLocal["Tot"]/.(local[#]->#&/@Join[inds[1],inds[2]]);

	tvs=RawTensorValues[t1];
	dims=Dimensions[t1];
	coords=Coordinates[t1];
	itrs={#,1,dims}&/@indsLocal["Tot"];
	vals=Table[D[tvs[[Sequence@@indsLocal[2]]],coords[[Sequence@@indsLocal[1]]]],Evaluate[Sequence@@itrs]];

	ToTensor[{"(PartialD"<>TensorName[t1]<>")-Auto","(\[PartialD]"<>TensorDisplayName[t1]<>")"},
			indsFinal,
			"Values"->vals,
			"Metric"->Metric[t1],
			"Coordinates"->Coordinates[t1],
			"Abstract"->False,
			"PossibleIndices"->PossibleIndices[t1],
			"Dimensions"->dims,
			"CurveParameter"->CurveParameter@t1,
			"ParametrizedValues"->ParametrizedValuesQ@t1,
			"Curve"->Curve@t1,
			"IsCurve"->CurveQ@t1]
] /;MemberQ[PossibleIndices[t1],a];

Tensor/:D[t1_Tensor,a_]:=
Module[{posInds},
	posInds=Complement[PossibleIndices[t1],Join[{a},Indices[t1]]/.{-ind_:>ind}];
	Metric[t1][a,posInds[[1]]]D[t1,-posInds[[1]]]
] /;MemberQ[PossibleIndices[t1],a];


Tensor/:D[t1_Tensor,a_Symbol]:=SetTensorName[ActOnTensorValues[t1,D[#,a]&],{"(PartialD"<>TensorName[t1]<>")-Auto","(\[PartialD]"<>TensorDisplayName[t1]<>")"}]


Clear[chrTerm]
chrTerm[t_Tensor,tensorInd_,derivInd_,avoidInds_:{}]:=
Module[{inds,dummy,chr,chrDummy,newInds,tNew,tensorIndUp},
	inds=Indices[t];
	tensorIndUp=tensorInd/.-sym_Symbol:>sym;
	dummy=First[Complement[PossibleIndices[t],Join[{tensorInd,derivInd},inds,avoidInds]/.-sym_Symbol:>sym]];
	chr=ChristoffelSymbol[Metric[t]];
	chrDummy=If[MatchQ[tensorInd,-_Symbol],chr[dummy,tensorInd,derivInd],chr[tensorInd,-dummy,derivInd]];

	newInds=inds/.tensorIndUp->dummy;
	tNew=ToTensor[Join[KeyDrop[(Association@@t),{"Indices","Metric","IsMetric"}],
						Association["Indices"->newInds,
									"Metric"->Metric[t],
									"IsMetric"->False]]];
	If[tensorIndUp===tensorInd,1,-1]tNew chrDummy
]


Clear[CovariantD]
Tensor/: CovariantD[t1_Tensor,-a_Symbol,avoidInds_:{}] :=
D[t1,-a]+Sum[chrTerm[t1,i,-a,avoidInds],{i,Indices[t1]}]/; MemberQ[PossibleIndices[t1],a]
Tensor/: CovariantD[t1_Tensor,a_Symbol,avoidInds_:{}] :=
Module[{posInds},
	posInds=Complement[PossibleIndices[t1],Join[{a},Indices[t1],avoidInds]/.{-ind_:>ind}];
	Metric[t1][a,posInds[[1]]]CovariantD[t1,-posInds[[1]],{a}]
]/; MemberQ[PossibleIndices[t1],a] ;


Tensor/:CovariantD[t1_Tensor,param_Symbol,avoidInds_:{}]:=
Module[{chr,chrC,inds,x1},
	inds=PossibleIndices[t1];
	x1=Curve[t1];
	chr=ChristoffelSymbol[Metric[x1]];

	D[t1[inds[[1]]],param]+chr[inds[[1]],-inds[[2]],-inds[[3]]]t1[inds[[2]]]t1[inds[[3]]]
]/;OnCurveQ[t1]&&Not@MemberQ[PossibleIndices[t1],param]&&CurveParameter[t1]===param


End[];

EndPackage[];
