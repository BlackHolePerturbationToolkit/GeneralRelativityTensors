(* ::Package:: *)

BeginPackage["Tensors`TensorManipulation`",{"Tensors`TensorDefinitions`"}];


TensorManipulation`Private`str[a_String]:=ToString[Style[a,Italic],TraditionalForm]
TensorManipulation`Private`str[args___]:=StringRiffle[TensorManipulation`Private`str/@{args},","]


ContractIndices::usage="ContractIndices["<>TensorManipulation`Private`str["t","n"]<>"] contracts all repeated indices of \
Tensor "<>TensorManipulation`Private`str["t"]<>", returning the resulting lower-rank Tensor with name "<>TensorManipulation`Private`str["n"]<>".
ContractIndices["<>TensorManipulation`Private`str["t"]<>"] is equivalent to \
ContractIndices["<>TensorManipulation`Private`str["t"]<>",{TensorName["<>TensorManipulation`Private`str["t"]<>"],TensorDisplayName["<>TensorManipulation`Private`str["t"]<>"]}].";
ShiftIndices::usage="ShiftIndices["<>TensorManipulation`Private`str["t","inds"]<>"] raises and/or lowers the indices of \
Tensor "<>TensorManipulation`Private`str["t"]<>" according to the given List "<>TensorManipulation`Private`str["inds"]<>", adjusting \
the values using the Tensor's associated metric.";
ValidateIndices::usage="ValidateIndices["<>TensorManipulation`Private`str["t","inds"]<>"] checks that the List of indices \
"<>TensorManipulation`Private`str["inds"]<>" is valid for Tensor "<>TensorManipulation`Private`str["t"]<>". An error is printed and \
operation is aborted if the list is not valid.";
TensorRules::usage="TensorRules["<>TensorManipulation`Private`str["t"]<>"] returns a List of Rules with possible \
coordinates of Tensor "<>TensorManipulation`Private`str["t"]<>" as keys and TensorValues as values.";

MergeTensors::usage="MergeTensors["<>TensorManipulation`Private`str["expr","n"]<>"] calls MultiplyTensors, MultiplyTensorScalar, \
and SumTensors to merge the Tensor expression "<>TensorManipulation`Private`str["expr"]<>" into one Tensor with TensorName "<>TensorManipulation`Private`str["n"]<>".
MergeTensors["<>TensorManipulation`Private`str["expr"]<>"] merges the Tensor expression "<>TensorManipulation`Private`str["expr"]<>" and \
forms a new TensorName and TensorDisplayName from a combination of the Tensors making up the expression.";
SumTensors::usage="SumTensors["<>TensorManipulation`Private`str["t1","t2","...","n"]<>"] sums the Tensors "<>TensorManipulation`Private`str["t1","t2"]<>", \
etc., forming a new Tensor with TensorName "<>TensorManipulation`Private`str["n"]<>".
SumTensors["<>TensorManipulation`Private`str["t1","t2","..."]<>"] sums the Tensors "<>TensorManipulation`Private`str["t1","t2"]<>", etc., and \
forms a new TensorName and TensorDisplayName from a combination of the Tensors making up the expression.";
MultiplyTensors::usage="MultiplyTensors["<>TensorManipulation`Private`str["t1","t2","...","n"]<>"] forms the outer product of the \
Tensors "<>TensorManipulation`Private`str["t1","t2"]<>", etc., creating a new Tensor with TensorName "<>TensorManipulation`Private`str["n"]<>".
MultiplyTensors["<>TensorManipulation`Private`str["t1","t2","..."]<>"] forms the outer product of the Tensors \
"<>TensorManipulation`Private`str["t1","t2"]<>", etc., and forms a new TensorName \
and TensorDisplayName from a combination of the Tensors making up the expression.";
MultiplyTensorScalar::usage="MultiplyTensorScalar["<>TensorManipulation`Private`str["a","t","n"]<>"] \
or MultiplyTensorScalar["<>TensorManipulation`Private`str["t","a","n"]<>"] forms the product \
of the scalar "<>TensorManipulation`Private`str["a"]<>" with the Tensor "<>TensorManipulation`Private`str["t"]<>", \
creating a new Tensor with TensorName "<>TensorManipulation`Private`str["n"]<>".
MultiplyTensorScalar["<>TensorManipulation`Private`str["a","t"]<>"] forms the product of the "<>TensorManipulation`Private`str["a"]<>" \
and "<>TensorManipulation`Private`str["t"]<>", and forms a new TensorName and TensorDisplayName from a combination of \
the scalar and Tensor making up the expression.";
TraceReverse::usage="TraceReverse["<>TensorManipulation`Private`str["t","n"]<>"] returns the trace reversed version of \
the Tensor "<>TensorManipulation`Private`str["t"]<>" with the TensorName "<>TensorManipulation`Private`str["n"]<>".
TraceReverse["<>TensorManipulation`Private`str["t"]<>"] is equivalent to TraceReverse["<>TensorManipulation`Private`str["t","n"]<>"], \
but with the returned TensorName and DisplayTensorName auto-generated.";

RepeatedIndexQ::usage="RepeatedIndexQ["<>TensorManipulation`Private`str["t"]<>"] returns True if the Tensor \
"<>TensorManipulation`Private`str["t"]<>" has repeated indices which can be traced.";
Component::usage="Component["<>TensorManipulation`Private`str["t","inds"]<>"] returns the component of Tensor "<>TensorManipulation`Private`str["t"]<>" \
with (appropriately covariant and contravariant) List of indices "<>TensorManipulation`Private`str["inds"]<>". \
All elements of "<>TensorManipulation`Private`str["inds"]<>" must be Coordinates of "<>TensorManipulation`Private`str["t"]<>".";


Begin["`Private`"];


Tensor/:RepeatedIndexQ[t_Tensor]:=Length[DeleteDuplicates@(Indices[t]/.-sym_Symbol:>sym)]<Length[Indices[t]];
Tensor/:t_Tensor[inds__]/;Complement[{inds}/.-sym_Symbol:>sym,PossibleIndices[t]]==={}:=ShiftIndices[t,{inds}]
Tensor/:t_Tensor[inds__]/;(Coordinates[t]=!=Undefined)&&Complement[{inds}/.-sym_Symbol:>sym,Coordinates[t]]==={}:=Component[t,{inds}]
Tensor/:t_Tensor[inds__]:=(Print["The given indices ",{inds}, " are neither entirely in the List of PossibleIndices, nor Coordinates of ", t];Abort[])


(*The commented code is for when we add unique indices with dollar signs*)
(*Tensor/:UniqueIndices[t_Tensor,n_Integer/;n>=0]:=Unique[PadRight[{},n,PossibleIndices[t]]]*)


Clear[ValidateIndices]
Tensor/:ValidateIndices[t_Tensor,{inds___}]:=
Module[{posInds,indsUp,repeatedInds},

	posInds=PossibleIndices[t];
	indsUp={inds}/.-sym_Symbol:>sym;
	repeatedInds=Cases[{inds},#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp]);

	If[Complement[indsUp,posInds]=!={},
		Print["The following indices are not included in the list of PossibleIndices for tensor ", t, ": ", Complement[indsUp,posInds]];
		Abort[]
	];
	(*The commented code is for when we add unique indices with dollar signs*)
	(*If[Complement[Symbol[StringTrim[ToString[Unique[#]],"$"~~__]]&/@indsUp,posInds]=!={},
		Print["The following indices are not included in the list of PossibleIndices for tensor ", t, ": ", Complement[indsUp,posInds]];
		Abort[]
	];*)
	If[Length[indsUp]=!=Total[Rank[t]],
		Print["The tensor ", t, " expects " ,Total[Rank[t]], " indices, but ", Length[indsUp], If[Length[indsUp]===1," index was ", " indices were "],"given."];
		Abort[]
	];
	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];
		Abort[]
	];

	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],#[[1]]/.-sym_Symbol:>sym,##&[]]&/@repeatedInds];
		Abort[]
	];
	
	If[Length[{inds}]=!=1 && CurveQ[t],
		Print["The curve ", t, " can only have 1 index, but ", Length[{inds}]," were given."];
		Abort[]
	];
	
]


Clear[ShiftIndices]
Options[ShiftIndices]={"SimplifyFunction"->Simplify};
Tensor/:ShiftIndices[t_Tensor,inds:{__},opts:OptionsPattern[]]:=
Module[{},
	ValidateIndices[t,inds];
	
	Fold[shiftIndex[#1,#2,OptionValue["SimplifyFunction"]]&,t,Thread[{Range@Length[inds],inds}]]
]


Clear[shiftIndex]
shiftIndex[t_Tensor,{pos_Integer,ind_},simpFn_]:=
Module[{gOrInvG,inds,indPos,indPosNew,tvs,indsBefore,indsAfter,n,newTVs,
		coordsPTemp,temp,coords,param,itrBefore,itrAfter,vals,i,itrTot,itr,newPos,
		newMet,newInds,coordsP,coordsPRules},
	
	newPos=If[MatchQ[ind,_Symbol],"Up","Down"];
	indPos=IndexPositions[t];

	If[pos>Length@indPos,Print["Tensor ", t, " has only ", Length@indPos ," indices. Cannot raise at position ", pos,"."];Abort[]];
	indPosNew=ReplacePart[indPos,pos->newPos];
	inds=Indices[t];
	
	vals=Which[indPos[[pos]]===newPos,
			
			RawTensorValues[t],
			
			RawTensorValues[t]===Undefined && Metric[t]===Undefined,
			Print["Cannot shift tensor indices without a metric."];
			Abort[],
				
			RawTensorValues[t]===Undefined && Metric[t]=!=Undefined,
			
			Undefined,
		
			RawTensorValues[TensorName[t],indPosNew]===Undefined,
			
			tvs=RawTensorValues[t];
			n=Dimensions[t];
			indsBefore=Table[itr[ii],{ii,1,pos-1}];
			indsAfter=Table[itr[ii],{ii,pos+1,Length@indPos}];
			itrBefore=({#,1,n}&/@indsBefore);
			itrAfter=({#,1,n}&/@indsAfter);
			itrTot=Join[itrBefore,{{i,1,n}},itrAfter];
			
			gOrInvG = If[ParametrizedValuesQ[t],TensorValues,RawTensorValues][If[newPos==="Up",InverseMetric[t],Metric[t]]];
			Table[Sum[gOrInvG[[i,s]]tvs[[Sequence@@indsBefore,s,Sequence@@indsAfter]],{s,1,n}],Evaluate[Sequence@@itrTot]],
			
			True,
	
			RawTensorValues[TensorName[t],indPosNew]
	];

	newInds=Flatten@{Take[inds,pos-1],ind,Drop[inds,pos]};
	newMet=If[MetricQ[t]&&(If[MatchQ[#,_Symbol],"Up","Down"]&/@newInds)==={"Down","Down"},"Self",Metric[t]];

	ToTensor[Join[KeyDrop[Association@@t,{"Indices","Metric"}],
					Association["Values"->vals,
								"Metric"->newMet,
								"Indices"->newInds]]]
]


Clear[ContractIndices]
ContractIndices[expr_]:=expr/.t_Tensor:>ContractIndices[t]
Tensor/:ContractIndices[t_Tensor]:=NestWhile[contractIndex,t,RepeatedIndexQ]
Tensor/:ContractIndices[t_Tensor,name_String]:=SetTensorName[ContractIndices[t],name]
Tensor/:ContractIndices[t_Tensor,{name_String,displayName_String}]:=SetTensorName[ContractIndices[t],{name,displayName}]


Clear[contractIndex]
contractIndex[t_Tensor]:=
Module[{indsUp,rptInd,rptIndsPos,indPos,indPosNew,inds,indsNew,tvsFull,n,vals,traceIndex,
	indsBefore,indsBetween,indsAfter,itrBefore,itrBetween,itrAfter,itrTot,tvs,itr},
	
	indPos=IndexPositions[t];
	inds=Indices[t];
	indsUp=inds/.-sym_Symbol:>sym;
	rptInd=First[If[Count[indsUp,#]===2,#,##&[]]&/@DeleteDuplicates@indsUp];
	rptIndsPos=Flatten@Position[indsUp,rptInd];

	indPosNew=Delete[indPos,{#}&/@Flatten@rptIndsPos];
	indsNew=Delete[inds,{#}&/@Flatten@rptIndsPos];

	tvs=RawTensorValues[t];
	n=Dimensions[t];
	indsBefore=Table[itr[ii],{ii,1,rptIndsPos[[1]]-1}];
	indsBetween=Table[itr[ii],{ii,rptIndsPos[[1]]+1,rptIndsPos[[2]]-1}];
	indsAfter=Table[itr[ii],{ii,rptIndsPos[[2]]+1,Length@indPos}];
	itrBefore=({#,1,n}&/@indsBefore);
	itrBetween=({#,1,n}&/@indsBetween);
	itrAfter=({#,1,n}&/@indsAfter);
	itrTot=Join[itrBefore,itrBetween,itrAfter];
	vals = If[itrTot==={},
				Sum[tvs[[Sequence@@indsBefore,s,Sequence@@indsBetween,s,Sequence@@indsAfter]],{s,1,n}],
				Table[Sum[tvs[[Sequence@@indsBefore,s,Sequence@@indsBetween,s,Sequence@@indsAfter]],{s,1,n}],Evaluate[Sequence@@itrTot]]
		];
	ToTensor[Join[KeyDrop[Association@@t,{"Indices","Name"}],
					Association["Name"->TensorName[t]<>"-Auto",
								"Values"->vals,
								"Indices"->indsNew]]]
]


Tensor/:Component[t_Tensor,inds___List]:=
Module[{indsPos,indsAbstr,indsAbstrUp,coordsPos,indsUp},
	If[Length[inds]=!=Total@Rank[t],
		Print["Tensor ", t," expected ",Total@Rank[t]," indices to select a component, but ", Length[inds], If[Length[inds]===1," index was ", " indices were "],"given."];
		Abort[]
	];
	indsUp=inds/.-sym_Symbol:>sym;
	coordsPos=Flatten[Position[Coordinates[t],#]&/@indsUp];
	indsAbstrUp=Indices[t]/.-sym_Symbol:>sym;
	indsAbstr=MapThread[If[MatchQ[#1,_Symbol],#2,-#2]&,{inds,indsAbstrUp}];
	Part[TensorValues[t[Sequence@@indsAbstr]],Sequence@@coordsPos]
]


Tensor/:TensorRules[t_Tensor]:=
Module[{pmList,lhs},
	pmList=If[#==="Up",1,-1]&/@IndexPositions[t];
	lhs=pmList #&/@Tuples[Coordinates[t],Total@Rank[t]];
	(#->Component[t,#])&/@lhs
]


Clear[validateSumIndices]
validateSumIndices[inds1_List,inds2_List]:=
If[Sort[inds1]=!=Sort[inds2],
		Print["Cannot add Tensors with different indices, ",Sort[inds1]," and ",Sort[inds2]];
		Abort[]
]


Clear[SumTensors]
Tensor/:SumTensors[t1_Tensor,t2_Tensor]:=
Module[{posInds,vals,inds,tvs,its,dims,itrs,local,indsLocal,indsFinal,tvFunc},

	If[AbstractQ[t1]||AbstractQ[t2],Print["Cannot sum Abstract Tensors."];Abort[]];
	If[TensorName@Metric[t1]=!=TensorName@Metric[t2],Print["Cannot sum Tensors with different metrics."];Abort[]];
	If[TensorName@Curve@t1=!=TensorName@Curve@t2,Print["Cannot sum Tensors on different curves."];Abort[]];
	
	tvFunc=If[ParametrizedValuesQ@t1||ParametrizedValuesQ@t2,TensorValues,RawTensorValues];
			
	posInds=Union[PossibleIndices[t1],PossibleIndices[t2]];

	inds[1]=Indices[t1];
	inds[2]=Indices[t2];
	validateSumIndices[inds[1],inds[2]];

	local[sym_]:=If[MatchQ[sym,-_Symbol],Symbol["cov"<>ToString[-sym]],Symbol["con"<>ToString[sym]]];
	indsLocal[1]=local/@inds[1];
	indsLocal[2]=local/@inds[2];
	indsLocal["Tot"]=Sort@indsLocal[1];
	indsFinal=indsLocal["Tot"]/.(local[#]->#&/@inds[1]);

	tvs[1]=tvFunc[t1];
	tvs[2]=tvFunc[t2];
	dims=Dimensions[t1];
	itrs={#,1,dims}&/@indsLocal["Tot"];
	
	vals=Table[tvs[1][[Sequence@@indsLocal[1]]]+tvs[2][[Sequence@@indsLocal[2]]],Evaluate[Sequence@@itrs]];

	ToTensor[{"("<>TensorName[t1]<>"+"<>TensorName[t2]<>")-Auto","("<>TensorDisplayName[t1]<>"+"<>TensorDisplayName[t2]<>")"},
			indsFinal,
			"Values"->vals,
			"Metric"->Metric[t1],
			"IsMetric"->False,
			"Coordinates"->Coordinates[t1],
			"Abstract"->False,
			"PossibleIndices"->posInds,
			"Dimensions"->Dimensions[t1],
			"CurveParameter"->CurveParameter@t1,
			"ParametrizedValues"->(ParametrizedValuesQ@t1||ParametrizedValuesQ@t2),
			"Curve"->Curve@t1,
			"IsCurve"->CurveQ@t1]
]
Tensor/:SumTensors[t1_Tensor]:=t1;
Tensor/:SumTensors[t1_Tensor,t2__Tensor]:=Fold[SumTensors,t1,{t2}]
Tensor/:SumTensors[t1_Tensor,t2__Tensor,name_String]:=SetTensorName[SumTensors[t1,t2],name]
Tensor/:SumTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String}]:=SetTensorName[SumTensors[t1,t2],{name,displayName}]


Clear[validateProductIndices]
validateProductIndices[inds1_List,inds2_List]:=
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


Clear[MultiplyTensors]
Tensor/:MultiplyTensors[t1_Tensor,t2_Tensor]:=
Module[{posInds,vals,inds,repeatedInds,tvs,dims,itrs,indsLocal,local,indsFinal,tvFunc},

	If[AbstractQ[t1]||AbstractQ[t2],Print["Cannot multiply Abstract Tensors."];Abort[]];
	If[TensorName@Metric[t1]=!=TensorName@Metric[t2],Print["Cannot multiply Tensors with different metrics."];Abort[]];
	
	If[TensorName@Curve@t1=!=TensorName@Curve@t2,Print["Cannot multiply Tensors on different curves."];Abort[]];
	tvFunc=If[ParametrizedValuesQ@t1||ParametrizedValuesQ@t2,TensorValues,RawTensorValues];

	posInds=Union[PossibleIndices[t1],PossibleIndices[t2]];
	
	inds[1]=Indices[t1];
	inds[2]=Indices[t2];
	validateProductIndices[inds[1],inds[2]];
	
	local[sym_]:=If[MatchQ[sym,-_Symbol],Symbol["cov"<>ToString[-sym]],Symbol["con"<>ToString[sym]]];
	indsLocal[1]=local/@inds[1];
	indsLocal[2]=local/@inds[2];
	indsLocal["Tot"]=Sort@Join[indsLocal[1],indsLocal[2]];
	indsFinal=indsLocal["Tot"]/.(local[#]->#&/@Join[inds[1],inds[2]]);

	tvs[1]=tvFunc[t1];
	tvs[2]=tvFunc[t2];
	dims=Dimensions[t1];
	itrs={#,1,dims}&/@indsLocal["Tot"];
	vals=Table[tvs[1][[Sequence@@indsLocal[1]]]tvs[2][[Sequence@@indsLocal[2]]],Evaluate[Sequence@@itrs]];

	ToTensor[{"("<>TensorName[t1]<>"\[CenterDot]"<>TensorName[t2]<>")-Auto","("<>TensorDisplayName[t1]<>"\[CenterDot]"<>TensorDisplayName[t2]<>")"},
			indsFinal,
			"Values"->vals,
			"Metric"->Metric[t1],
			"IsMetric"->False,
			"Coordinates"->Coordinates[t1],
			"Abstract"->False,
			"PossibleIndices"->posInds,
			"Dimensions"->dims,
			"CurveParameter"->CurveParameter@t1,
			"ParametrizedValues"->(ParametrizedValuesQ@t1||ParametrizedValuesQ@t2),
			"Curve"->Curve@t1,
			"IsCurve"->CurveQ@t1]
];

Tensor/:MultiplyTensors[t1_Tensor]:=t1;
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor]:=Fold[MultiplyTensors,t1,{t2}]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,name_String]:=SetTensorName[MultiplyTensors[t1,t2],name]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String}]:=SetTensorName[MultiplyTensors[t1,t2],{name,displayName}]


Clear[MultiplyTensorScalar]
Tensor/:MultiplyTensorScalar[t_Tensor,n_]:=MultiplyTensorScalar[n,t];
Tensor/:MultiplyTensorScalar[n_,t_Tensor]:=
Module[{vals},
	If[AbstractQ[t],Print["Cannot multiply Abstract Tensors."];Abort[]];
	If[Not[MatchQ[n,(_Symbol|_Real|_Complex|_Integer|_Rational|_Times|_Plus)]],Print["Cannot multiply a Tensor by a ", Head[n]];Abort[]];
	vals=n RawTensorValues[t];

	ToTensor[{"("<>ToString[n]<>TensorName[t]<>")-Auto","("<>ToString[n]<>"\[CenterDot]"<>TensorDisplayName[t]<>")"},
			Indices[t],
			"Values"->vals,
			"Metric"->Metric[t],
			"IsMetric"->False,
			"Coordinates"->Coordinates[t],
			"Abstract"->False,
			"PossibleIndices"->PossibleIndices[t],
			"Dimensions"->Dimensions[t],
			"CurveParameter"->CurveParameter@t,
			"ParametrizedValues"->ParametrizedValuesQ@t,
			"Curve"->Curve@t,
			"IsCurve"->CurveQ@t]

]
Tensor/:MultiplyTensorScalar[t1_Tensor]:=t1;
Tensor/:MultiplyTensorScalar[n_,t1_Tensor,name_String]:=SetTensorName[MultiplyTensorScalar[n,t1],name]
Tensor/:MultiplyTensorScalar[t1_Tensor,n_,name_String]:=MultiplyTensorScalar[n,t1,name]
Tensor/:MultiplyTensorScalar[n_,t1_Tensor,{name_String,displayName_String}]:=SetTensorName[MultiplyTensorScalar[n,t1],{name,displayName}]
Tensor/:MultiplyTensorScalar[t1_Tensor,n_,{name_String,displayName_String}]:=MultiplyTensorScalar[n,t1,{name,displayName}]


Options[MergeTensors]={"SimplifyFunction"->Identity};
Clear[MergeTensors]
MergeTensors[expr_,opts:OptionsPattern[]]:=
Module[{expr1,expr2,simpFn,expr3},
	simpFn=OptionValue["SimplifyFunction"];
	expr1=Expand[expr]/.t1_Tensor t2__Tensor:>MultiplyTensors[t1,t2];
	(*Print[expr1];*)
	expr2=expr1//.n_ t_Tensor/;Not[MatchQ[n,_Tensor]]:>MultiplyTensorScalar[n,t];
	(*Print[expr2];*)
	expr3=ActOnTensorValues[ContractIndices[expr2]/.Plus[t1_Tensor,t2__Tensor]:>SumTensors[t1,t2],simpFn];
	(*Print[expr3];*)
	expr3	
]
MergeTensors[expr_,name_String,opts:OptionsPattern[]]:=SetTensorName[MergeTensors[expr,opts],name]
MergeTensors[expr_,{name_String,dispName_String},opts:OptionsPattern[]]:=SetTensorName[MergeTensors[expr,opts],{name,dispName}]


Clear[TraceReverse]
Options[TraceReverse]={"SimplifyFunction"->Identity};
Tensor/:TraceReverse[t_Tensor,{name_String,dispName_String},opts:OptionsPattern[]]:=
Module[{met,tTr,pis,is,simpFn},

	simpFn=OptionValue["SimplifyFunction"];
	If[Total[Rank[t]]=!=2,
		Print["TraceReverse is built only for Tensors of Rank 2"];
		Abort[]
	];

	pis=PossibleIndices[t];
	is=Indices[t];
	met=Metric[t];
	tTr=simpFn@TensorValues@ContractIndices[t[pis[[1]],-pis[[1]]]];

	MergeTensors[t[is[[1]],is[[2]]]-2met tTr/Dimensions[met],{name,dispName},"SimplifyFunction"->simpFn]
];
Tensor/:TraceReverse[t_Tensor,name_String,opts:OptionsPattern[]]:=TraceReverse[t,{name,name},opts]
Tensor/:TraceReverse[t_Tensor,opts:OptionsPattern[]]:=TraceReverse[t,{TensorName[t]<>"TraceReverse",TensorDisplayName[t]<>"Bar"},opts]


End[];

EndPackage[];
