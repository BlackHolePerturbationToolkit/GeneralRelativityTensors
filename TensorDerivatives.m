(* ::Package:: *)

BeginPackage["Tensors`TensorDerivatives`",{"Tensors`TensorDefinitions`"}];


ChristoffelSymbol::usage="ChristoffelSymbol[m] returns the Christoffel symbol computed from the metric Tensor m.";
CovariantD::usage="CovariantD[t,ind] returns the covariant derivative of tensor \
t with respect to the index ind as a sum and product of tensors.
CovariantD[t,param] returns the covariant derivative of tensor t with respect to the curve \
parameter param.";


Begin["`Private`"];


Options[ChristoffelSymbol]={"ActWith"->Identity};
DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"] = {"ActWith"->"Function which is applied to the elements of ChristoffelSymbol after they are calculated."};


Tensor/:ChristoffelSymbol[t_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{n,g,ig,xx,vals,gT,name,simpFn,a,b,c},
	simpFn=OptionValue["ActWith"];
	gT=Metric[t];
	xx=Coordinates[gT];
	{a,b,c}=Take[PossibleIndices[gT],3];
	n=Dimensions[gT];
	g=RawTensorValues[gT];
	ig=RawTensorValues@InverseMetric[gT];
	name="ChristoffelSymbol"<>TensorName[t];

	vals=
		If[RawTensorValues[name,{"Up","Down","Down"}]===Undefined,
			Map[simpFn,Table[(1/2)Sum[ig[[i,s]](-D[g[[j,k]],xx[[s]]]+D[g[[j,s]],xx[[k]]]+D[g[[s,k]],xx[[j]]]),{s,1,n}],{i,1,n},{j,1,n},{k,1,n}],{3}],
			RawTensorValues[name,{"Up","Down","Down"}]
		];

	ToTensor[Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","IsMetric","Indices"}],
			Association["Metric"->gT,
						"IsMetric"->False,
						"Values"->vals,
						"DisplayName"->"\[CapitalGamma]",
						"Name"->name,
						"Indices"->{a,-b,-c}]]]
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

	ToTensor[Join[KeyDrop[Association@@t1,{"DisplayName","Metric","Name","IsMetric","Indices","Values"}],
					Association["IsMetric"->False,
								"Metric"->Metric[t1],
								"Values"->vals,
								"DisplayName"->"(\[PartialD]"<>TensorDisplayName[t1]<>")",
								"Name"->"(PartialD"<>TensorName[t1]<>")-Auto",
								"Indices"->indsFinal]]]
								
] /;MemberQ[PossibleIndices[t1],a];

Tensor/:D[t1_Tensor,a_]:=
Module[{posInds},
	posInds=Complement[PossibleIndices[t1],Join[{a},Indices[t1]]/.{-ind_:>ind}];
	Metric[t1][a,posInds[[1]]]D[t1,-posInds[[1]]]
] /;MemberQ[PossibleIndices[t1],a];


Tensor/:D[t1_Tensor,a_Symbol]:=SetTensorName[ActOnTensorValues[t1,D[#,a]&],{"(PartialD"<>TensorName[t1]<>")-Auto","(\[PartialD]"<>TensorDisplayName[t1]<>")"}]


Tensor/:D[t1_Tensor?CurveQ,param_Symbol] :=
Module[{vals},

	vals=RawTensorValues[t1];

	ToTensor[Join[KeyDrop[Association@@t1,{"DisplayName","Name","Values","IsCurve","Curve"}],
					Association["IsCurve"->False,
								"Curve"->Curve[t1],
								"Values"->(D[#,param]&/@vals),
								"DisplayName"->"(d"<>TensorDisplayName[t1]<>"/d"<>ToString[param]<>")",
								"Name"->"(d"<>TensorName[t1]<>"/d"<>ToString[param]<>")-Auto"]]]
								
] /;(CurveParameter[t1]===param);


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


(*Tensor/:CovariantD[t1_Tensor,param_Symbol,avoidInds_:{}]:=
Module[{chr,chrC,a,b,c,x1},
	{a,b,c}=Take[PossibleIndices[t1],3];
	x1=Curve[t1];
	chr=ToTensorOnCurve[ChristoffelSymbol[Metric[x1]],x1];

	D[t1[a],param]+chr[a,-b,-c]t1[b]D[x1[c],param]
]/;OnCurveQ[t1]&&Not@MemberQ[PossibleIndices[t1],param]&&CurveParameter[t1]===param*)


Tensor/:CovariantD[t1_Tensor?OnCurveQ,u_Tensor?OnCurveQ,avoidInds_:{}]:=
Module[{chr,chrC,a,b,c,x1,x2,param},
	x1=Curve[t1];
	x2=Curve[u];
	If[TensorName[x1]=!=TensorName[x2],
		Print["Cannot take covariant derivative along 4-velocity from different curves."]; 
		Abort[]
	];
	If[Not[Rank[u]==={1,0}],
		Print["Four velocity that we differentiate along must be rank {1,0}."];
		Abort[]
	];
	If[Not[Total@Rank[t1]===1],
		Print["For now, covariant differentiation on Curves is only possible for vectors."];
		Abort[]
	];

	param=CurveParameter[x2];
	
	{a,b,c}=Select[PossibleIndices[t1],Not[MemberQ[{avoidInds}/.(-nn_Symbol:>nn),#]]&,3];
	
	chr=ToTensorOnCurve[ChristoffelSymbol[Metric[x1]],x1];

	D[t1[a],param]+chr[a,-b,-c]t1[b]u[c]
]


Tensor/:CovariantD[t1_Tensor,u_Tensor?OnCurveQ,avoidInds_:{}]:=
Module[{chr,chrC,inds,a,covD},
	
	If[TensorName[Metric@t1]=!=TensorName[Metric@u],
		Print["Cannot take covariant derivative along 4-velocity from different metric."]; 
		Abort[]
	];
	If[Not[Rank[u]==={1,0}],
		Print["Four velocity that we differentiate along must be rank {1,0}."];
		Abort[]
	];
	If[Curve[t1]=!=Undefined&&(TensorName[Curve[t1]]=!=TensorName[Curve[u]]),
		Print["Cannot take covariant derivative along 4-velocity from different curves."]; 
		Abort[]
	];
	inds=Indices[t1];
	a=SelectFirst[PossibleIndices[t1],Not[MemberQ[Join[inds,{avoidInds}]/.(-n_Symbol:>n),#]]&];

	covD=CovariantD[t1,-a]/.t_Tensor:>ToTensorOnCurve[t,Curve[u]];
	
	u[a]covD
]


End[];

EndPackage[];
