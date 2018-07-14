(* ::Package:: *)

BeginPackage["GeneralRelativityTensors`TensorDerivatives`",{"GeneralRelativityTensors`TensorDefinitions`","GeneralRelativityTensors`TensorManipulation`"}];


ChristoffelSymbol::usage="ChristoffelSymbol[m] returns the Christoffel symbol computed from the metric Tensor m.";
CovariantD::usage="CovariantD[t,ind1,ind2,...] returns the covariant derivative of tensor \
t with respect to the indices ind1, ind2, etc. as a sum and product of Tensors.
CovariantD[expr,ind1,ind2,...] returns the covariant derivative of the Tensor expression expr. \
with respect to the indices ind1, ind2, etc.
CovariantD[t,u] returns the covariant derivative of tensor t with along a four-velocity u.";


Begin["`Private`"];


Options[ChristoffelSymbol]={"ActWith"->Identity};
DocumentationBuilder`OptionDescriptions["ChristoffelSymbol"] = {"ActWith"->"Function which is applied to the elements of ChristoffelSymbol after they are calculated."};


Options[CovariantD]=Join[Options[ChristoffelSymbol],{"ActWithNested"->Identity,"Merge"->False,"MergeNested"->False}];
DocumentationBuilder`OptionDescriptions["CovariantD"] = {"ActWith"->"Function which is applied to the values that CovariantD produces",
"ActWithNested"->"If multiple derivatives are taken, this functions will be applied to the values of each sub-derivative.",
"Merge"->"Boolean controlling whether the return value of CovariantD should be merged into one Tensor",
"MergeNested"->"If multiple derivatives are taken, this Boolean controlling whether merging happens after each sub-derivative."};


Tensor/:ChristoffelSymbol[t_Tensor?MetricQ,opts:OptionsPattern[]]:=
Module[{n,g,ig,xx,vals,gT,name,simpFn,a,b,c,chrValue},
	simpFn=OptionValue["ActWith"];
	gT=Metric[t];
	xx=Coordinates[gT];
	{a,b,c}=Take[PossibleIndices[gT],3];
	n=Dimensions[gT];
	g=RawTensorValues[gT];
	ig=RawTensorValues@InverseMetric[gT];
	name="ChristoffelSymbol"<>TensorName[t];

	chrValue[ii_,jj_,kk_]:=chrValue[ii,jj,kk]=simpFn[1/2 Sum[ig[[ii,s]](-D[g[[jj,kk]],xx[[s]]]+D[g[[jj,s]],xx[[kk]]]+D[g[[s,kk]],xx[[jj]]]),{s,1,n}]];
	chrValue[ii_,jj_,kk_]/;jj<kk:=chrValue[ii,kk,jj];
	
	vals=
		If[RawTensorValues[name,{"Up","Down","Down"}]===Undefined,
			(*Map[simpFn,Table[(1/2)Sum[ig[[i,s]](-D[g[[j,k]],xx[[s]]]+D[g[[j,s]],xx[[k]]]+D[g[[s,k]],xx[[j]]]),{s,1,n}],{i,1,n},{j,1,n},{k,1,n}],{3}],*)
			Table[chrValue[i,j,k],{i,1,n},{j,1,n},{k,1,n}],
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


Tensor/:D[t1_Tensor,-a_Symbol,simpFn_:Identity] :=
Module[{vals,inds,repeatedInds,tvs,dims,itrs,indsLocal,local,indsFinal,coords,valsSimp},

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
	valsSimp=Map[simpFn,vals,{Length@indsFinal}];

	ToTensor[Join[KeyDrop[Association@@t1,{"DisplayName","Metric","Name","IsMetric","Indices","Values"}],
					Association["IsMetric"->False,
								"Metric"->Metric[t1],
								"Values"->valsSimp,
								"DisplayName"->"(\[PartialD]"<>TensorDisplayName[t1]<>")",
								"Name"->"(PartialD"<>TensorName[t1]<>")-Auto",
								"Indices"->indsFinal]]]
								
] /;MemberQ[PossibleIndices[t1],a];


Tensor/:D[t1_Tensor,a_Symbol,simpFn_:Identity]:=
Module[{b},
	b=First[Complement[PossibleIndices[t1],Join[{a},Indices[t1]]/.{-ind_:>ind}]];
	Metric[t1][a,b]D[t1,-b,simpFn]
] /;MemberQ[PossibleIndices[t1],a];


Tensor/:D[t1_Tensor?OnCurveQ,param_Symbol,simpFn_:Identity] :=
Module[{vals},

	vals=Map[simpFn[D[#,param]]&,RawTensorValues[t1],{Total@Rank@t1}];

	ToTensor[Join[KeyDrop[Association@@t1,{"DisplayName","Name","Values","IsCurve","Curve"}],
					Association["IsCurve"->False,
								"Curve"->Curve[t1],
								"Values"->vals,
								"DisplayName"->"(d"<>TensorDisplayName[t1]<>"/d"<>ToString[param]<>")",
								"Name"->"(d"<>TensorName[t1]<>"/d"<>ToString[param]<>")-Auto"]]]
								
] /;(CurveParameter[t1]===param);


Clear[chrTerm]
chrTerm[t_Tensor,tensorInd_,derivInd_,simpFn_,avoidInds_]:=
Module[{inds,dummy,chr,chrDummy,newInds,tNew,tensorIndUp},
	inds=Indices[t];
	tensorIndUp=tensorInd/.-sym_Symbol:>sym;
	dummy=First[Complement[PossibleIndices[t],Join[{tensorInd,derivInd},inds,avoidInds]/.-sym_Symbol:>sym]];
	chr=ChristoffelSymbol[Metric[t],"ActWith"->simpFn];
	chrDummy=If[MatchQ[tensorInd,-_Symbol],chr[dummy,tensorInd,derivInd],chr[tensorInd,-dummy,derivInd]];

	newInds=inds/.tensorIndUp->dummy;
	tNew=ToTensor[Join[KeyDrop[(Association@@t),{"Indices","Metric","IsMetric"}],
						Association["Indices"->newInds,
									"Metric"->Metric[t],
									"IsMetric"->False]]];
	If[tensorIndUp===tensorInd,1,-1]tNew chrDummy
]


Clear[CovariantD]
CovariantD[expr_,inds__,avoidInds_List,opts:OptionsPattern[]]:=
Module[{simpFn,simpFnNest,merge,mergeNest},
	
	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];

	mergeNest=OptionValue["MergeNested"];
	merge=If[mergeNest===False,OptionValue["Merge"],mergeNest];
	
	CovariantD[Fold[CovariantD[#1,#2,avoidInds,"ActWith"->simpFnNest,"Merge"->mergeNest]&,expr,Most@{inds}],Last@{inds},"ActWith"->simpFn,"Merge"->merge]
];
CovariantD[expr_,inds__,opts:OptionsPattern[]]:=CovariantD[expr,inds,{},opts]


CovariantD[expr_,-a_Symbol,avoidInds_List,opts:OptionsPattern[]] := 
Module[{simpFn,t1Simp,expr1,expr2,expr3,aInds,met,pis,tempD,exprExpand,merge},

	ValidateTensorExpression[expr];
	simpFn=OptionValue["ActWith"];
	merge=OptionValue["Merge"];
	exprExpand=Expand[expr];
	aInds=DeleteDuplicates@Flatten[Join[Indices/@List@@(exprExpand),avoidInds]/.-aa_Symbol:>aa];
	met=Metric[expr];
	pis=PossibleIndices[met];

	If[Not[MemberQ[pis,a]],Print["Index ", a, " is not in the list of PossibleIndices of ",met]; Abort[]];

	expr1=Which[MatchQ[Expand[expr],_Times],tempD[exprExpand,-a],
			MatchQ[exprExpand,_Plus],tempD[#,-a]&/@exprExpand,
			True,
			Print["Expression ", exprExpand, " does not have Head Plus or Times. Cannot differentiate."];
			Abort[]
	];

	expr2 = expr1/.(tempD[coeff_ t:(_Tensor|Times[_Tensor, __Tensor]),-a]/;Not@MatchQ[coeff,_Tensor|Times[_Tensor, __Tensor] ]):>coeff covDProd[Sequence@t,-a,aInds,Identity];
	expr3 = expr2/.{tempD[t_Tensor,-a]:>CovariantD[t,-a,aInds],tempD[t:Times[_Tensor, __Tensor],-a]:>covDProd[Sequence@@t,-a,aInds,Identity]};
	If[merge, MergeTensors[expr3,"ActWith"->simpFn], Replace[expr3,t_Tensor :> ActOnTensorValues[simpFn,t]],2]
]
CovariantD[expr_,-a_Symbol,opts:OptionsPattern[]] := CovariantD[expr,-a,{},opts];


CovariantD[expr_,a_Symbol,avoidInds_List,opts:OptionsPattern[]] :=
Module[{b,aInds,met,pis,simpFn,merge,expr1},
	
	ValidateTensorExpression[expr];
	simpFn=OptionValue["ActWith"];
	merge=OptionValue["Merge"];

	aInds=DeleteDuplicates@Flatten[Join[Indices/@List@@(Expand[expr]),avoidInds,{a}]/.-aa_Symbol:>aa];
	met=Metric[expr];
	pis=PossibleIndices[met];
	
	If[Not[MemberQ[pis,a]],Print["Index ", a, " is not in the list of PossibleIndices of ",met]; Abort[]];

	b=SelectFirst[pis,Not[MemberQ[aInds,#]]&];

	expr1 = met[a,b]CovariantD[expr,-b,aInds];
	
	If[merge, MergeTensors[expr1,"ActWith"->simpFn], Replace[expr1,t_Tensor :> ActOnTensorValues[simpFn,t]],2]
];
CovariantD[expr_,a_Symbol,opts:OptionsPattern[]] :=CovariantD[expr,a,{},opts];


CovariantD[t1_Tensor,-a_Symbol,avoidInds_List,opts:OptionsPattern[]] := 
Module[{simpFn,expr1,merge},
	simpFn=OptionValue["ActWith"];
	merge=OptionValue["Merge"];
	expr1 = D[t1,-a]+Sum[chrTerm[t1,i,-a,simpFn,avoidInds],{i,Indices[t1]}];
	If[merge, MergeTensors[expr1,"ActWith"->simpFn], Replace[expr1,t_Tensor :> ActOnTensorValues[simpFn,t]],2]
]/; MemberQ[PossibleIndices[t1],a];
CovariantD[t1_Tensor,-a_Symbol,opts:OptionsPattern[]] := CovariantD[t1,-a,{},opts]


CovariantD[t1_Tensor,a_Symbol,avoidInds_List,opts:OptionsPattern[]] :=
Module[{b,simpFn,merge,expr1},
	simpFn=OptionValue["ActWith"];
	merge=OptionValue["Merge"];
	b=First[Complement[PossibleIndices[t1],Join[{a},Indices[t1],avoidInds]/.{-ind_:>ind}]];
	expr1 = Metric[t1][a,b]CovariantD[t1,-b,{a},"ActWith"->simpFn,"Merge"->False];
	If[merge, MergeTensors[expr1,"ActWith"->simpFn], Replace[expr1,t_Tensor :> ActOnTensorValues[simpFn,t]],2]
]/; MemberQ[PossibleIndices[t1],a];
CovariantD[t1_Tensor,a_Symbol,opts:OptionsPattern[]] := CovariantD[t1,a,{},opts]/; MemberQ[PossibleIndices[t1],a] ;


CovariantD[t1_Tensor?OnCurveQ,u_Tensor?OnCurveQ,avoidInds_List,opts:OptionsPattern[]]:=
Module[{chr,chrC,a,b,c,x1,x2,param,simpFn},
	
	simpFn=OptionValue["ActWith"];

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
	
	chr=ActOnTensorValues[simpFn,ToTensorOnCurve[ChristoffelSymbol[Metric[x1]],x1]];

	D[t1[a],param,simpFn]+chr[a,-b,-c]ActOnTensorValues[simpFn,t1[b]]ActOnTensorValues[simpFn,u[c]]
];
CovariantD[t1_Tensor?OnCurveQ,u_Tensor?OnCurveQ,opts:OptionsPattern[]]:=CovariantD[t1,u,{},opts]


CovariantD[t1_Tensor,u_Tensor?OnCurveQ,avoidInds_List,opts:OptionsPattern[]]:=
Module[{chr,chrC,inds,a,covD,simpFn},

	simpFn=OptionValue["ActWith"];
	
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
	
	ActOnTensorValues[simpFn,u[a]]ActOnTensorValues[simpFn,covD]
];
CovariantD[t1_Tensor,u_Tensor?OnCurveQ,opts:OptionsPattern[]]:=CovariantD[t1,u,{},opts]


Clear[covDProd]
covDProd[t1_Tensor,-a_Symbol,avoidInds_List,simpFn_]:=CovariantD[t1,-a,avoidInds,"ActWith"->simpFn];
covDProd[t1_Tensor,t2__Tensor,-a_Symbol,avoidInds_List,simpFn_]:=CovariantD[t1,-a,avoidInds,"ActWith"->simpFn]Times[t2]+t1 covDProd[First[{t2}],Sequence@@Rest[{t2}],-a,avoidInds,simpFn];


End[];

EndPackage[];
