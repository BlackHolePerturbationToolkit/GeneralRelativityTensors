(* ::Package:: *)

BeginPackage["GeneralRelativityTensors`TensorManipulation`",{"GeneralRelativityTensors`TensorDefinitions`"}];


ContractIndices::usage="ContractIndices[t,n] contracts all repeated indices of \
Tensor t, returning the resulting lower-rank Tensor with name n.
ContractIndices[t] is equivalent to \
ContractIndices[t,{TensorName[t],TensorDisplayName[t]}].";
ShiftIndices::usage="ShiftIndices[t,inds] raises and/or lowers the indices of \
Tensor t according to the given List inds, adjusting \
the values using the Tensor's associated metric.";
TensorRules::usage="TensorRules[t] returns a List of Rules with possible \
coordinates of Tensor t as keys and TensorValues as values.";

MergeTensors::usage="MergeTensors[expr,n] calls MultiplyTensors, MultiplyTensorScalar, \
SumTensors, and ContractIndices to merge the Tensor expression expr into one Tensor with TensorName n.
MergeTensors[expr] merges the Tensor expression expr and \
forms a new TensorName and TensorDisplayName from a combination of the Tensors making up the expression.";
SumTensors::usage="SumTensors[t1,t2,...,n] sums the Tensors t1, t2, \
etc., forming a new Tensor with TensorName n.
SumTensors[t1,t2,...] sums the Tensors t1, t2, etc., and \
forms a new TensorName and TensorDisplayName from a combination of the Tensors making up the expression.";
MultiplyTensors::usage="MultiplyTensors[t1,t2,...,n] forms the outer product of the \
Tensors t1, t2, etc., creating a new Tensor with TensorName n.
MultiplyTensors[t1,t2,...] forms the outer product of the Tensors \
t1, t2, etc., and forms a new TensorName \
and TensorDisplayName from a combination of the Tensors making up the expression.";
MultiplyTensorScalar::usage="MultiplyTensorScalar[a,t,n] \
or MultiplyTensorScalar[t,a,n] forms the product \
of the scalar a with the Tensor t, \
creating a new Tensor with TensorName n.
MultiplyTensorScalar[a,t] forms the product of the a \
and t, and forms a new TensorName and TensorDisplayName from a combination of \
the scalar and Tensor making up the expression.";
TraceReverse::usage="TraceReverse[t,n] returns the trace reversed version of \
the Tensor t with the TensorName n.
TraceReverse[t] is equivalent to TraceReverse[t,n], \
but with the returned TensorName and DisplayTensorName auto-generated.";

RepeatedIndexQ::usage="RepeatedIndexQ[t] returns True if the Tensor \
t has repeated indices which can be traced.";
Component::usage="Component[t,inds] returns the component of Tensor t \
with (appropriately covariant and contravariant) List of indices inds. \
All elements of inds must be Coordinates of t.";

ReorderTensorIndices::usage="ReorderTensorIndices[t,order,n] returns the Tensor t renamed n with its indices \
reordered as given by order, which is a List including all integers from 1 to the rank of the Tensor.
ReorderTensorIndices[t,order] is equivalent, but with an automatically generated name for the new Tensor.";
AntisymmetrizeTensor::usage="AntisymmetrizeTensor[t,{pos1,pos2},n] returns the Tensor t, antisymmetrized on its indices in \
positions pos1,pos2.
AntisymmetrizeTensor[t,{pos1,pos2}] is equivalent, but with an automatically generated name for the new Tensor.";
SymmetrizeTensor::usage="SymmetrizeTensor[t,{pos1,pos2},n] returns the Tensor t, symmetrized on its indices in \
positions pos1,pos2.
SymmetrizeTensor[t,{pos1,pos2}] is equivalent, but with an automatically generated name for the new Tensor.";


Begin["`Private`"];


Options[ShiftIndices]={"ActWith"->Identity};
Options[Component]=Options[ShiftIndices];
Options[TensorRules]=Options[ShiftIndices];
Options[ContractIndices]=Options[ShiftIndices];
Options[SumTensors]=Options[ShiftIndices];
Options[MultiplyTensors]=Options[ShiftIndices];
Options[MultiplyTensorScalar]=Options[ShiftIndices];
Options[TraceReverse]=Join[Options[ShiftIndices],{"ActWithNested"->Identity}];
Options[SymmetrizeTensor]=Options[TraceReverse];
Options[AntisymmetrizeTensor]=Options[TraceReverse];
Options[MergeTensors]=Join[Options[TraceReverse],{"NestQuantity"->3}];

DocumentationBuilder`OptionDescriptions["ShiftIndices"] = {"ActWith"->"Function that is applied to the elements of the tensor as they are calculated."};
DocumentationBuilder`OptionDescriptions["Component"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["TensorRules"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["ContractIndices"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["SumTensors"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["MultiplyTensors"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["MultiplyTensorScalar"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["TraceReverse"] = Join[DocumentationBuilder`OptionDescriptions["ShiftIndices"],
{"ActWithNested"->"Function that is applied to the elements of the tensor and also passed to any other functions called internally."}];
DocumentationBuilder`OptionDescriptions["SymmetrizeTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["AntisymmetrizeTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["MergeTensors"] = Join[DocumentationBuilder`OptionDescriptions["TraceReverse"],
{"NestQuantity"->"Number of times MergeTensors can call itself as it continues to try to create one Tensor from an expression"}];


Tensor/:RepeatedIndexQ[t_Tensor]:=Length[DeleteDuplicates@(Indices[t]/.-sym_Symbol:>sym)]<Length[Indices[t]];
Tensor/:t_Tensor[inds__]/;Complement[{inds}/.-sym_Symbol:>sym,PossibleIndices[t]]==={}:=ShiftIndices[t,{inds}]
Tensor/:t_Tensor[inds__]/;(Coordinates[t]=!=Undefined)&&Complement[{inds}/.-sym_Symbol:>sym,Coordinates[t]]==={}:=Component[t,{inds}]
Tensor/:t_Tensor[inds__]:=(Print["The given indices ",{inds}, " are neither entirely in the List of PossibleIndices, nor Coordinates of ", t];Abort[])


(*The commented code is for when we add unique indices with dollar signs*)
(*Tensor/:UniqueIndices[t_Tensor,n_Integer/;n>=0]:=Unique[PadRight[{},n,PossibleIndices[t]]]*)


Clear[validateTensorIndices]
Tensor/:validateTensorIndices[t_Tensor,{inds___}]:=
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
Tensor/:ShiftIndices[t_Tensor,inds:{__},opts:OptionsPattern[]]:=
Module[{},
	validateTensorIndices[t,inds];
	
	Fold[shiftIndex[#1,#2,OptionValue["ActWith"]]&,t,Thread[{Range@Length[inds],inds}]]
]


Clear[shiftIndex]
shiftIndex[t_Tensor,{pos_Integer,ind_},simpFn_]:=
Module[{gOrInvG,inds,indPos,indPosNew,tvs,indsBefore,indsAfter,n,newTVs,
		coordsPTemp,temp,coords,param,itrBefore,itrAfter,vals,i,itrTot,itr,newPos,
		newMet,newInds,coordsP,coordsPRules,newCurve},
	
	newPos=If[MatchQ[ind,_Symbol],"Up","Down"];
	indPos=IndexPositions[t];

	If[pos>Length@indPos,Print["Tensor ", t, " has only ", Length@indPos ," indices. Cannot raise at position ", pos,"."];Abort[]];
	indPosNew=ReplacePart[indPos,pos->newPos];
	inds=Indices[t];
	
	vals=simpFn@
		Which[indPos[[pos]]===newPos,
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
			
			gOrInvG = If[OnCurveQ[t],TensorValues,RawTensorValues][If[newPos==="Up",InverseMetric[t],Metric[t]]];
			Table[Sum[gOrInvG[[i,s]]tvs[[Sequence@@indsBefore,s,Sequence@@indsAfter]],{s,1,n}],Evaluate[Sequence@@itrTot]],
			
			True,
			RawTensorValues[TensorName[t],indPosNew]
	];

	newInds=Flatten@{Take[inds,pos-1],ind,Drop[inds,pos]};
	newMet=If[MetricQ[t]&&(If[MatchQ[#,_Symbol],"Up","Down"]&/@newInds)==={"Down","Down"},"Self",Metric[t]];
	newCurve=If[CurveQ[t]&&(If[MatchQ[#,_Symbol],"Up","Down"]&/@newInds)==={"Up"},"Self",Curve[t]];

	ToTensor[Join[KeyDrop[Association@@t,{"Indices","Metric","Values","Curve"}],
					Association["Values"->vals,
								"Metric"->newMet,
								"Indices"->newInds,
								"Curve"->newCurve]]]
]


Clear[ContractIndices]
Options[ContractIndices]
ContractIndices[expr_,opts:OptionsPattern[]]:=expr/.t_Tensor:>ContractIndices[t,opts]
Tensor/:ContractIndices[t_Tensor,opts:OptionsPattern[]]:=NestWhile[contractIndex[#,OptionValue["ActWith"]]&,t,RepeatedIndexQ]
Tensor/:ContractIndices[t_Tensor,name_String,opts:OptionsPattern[]]:=SetTensorName[ContractIndices[t,opts],name]
Tensor/:ContractIndices[t_Tensor,{name_String,displayName_String},opts:OptionsPattern[]]:=SetTensorName[ContractIndices[t,opts],{name,displayName}]


Clear[contractIndex]
contractIndex[t_Tensor,simpFn_]:=
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
	
	vals = Map[simpFn,
				Table[Sum[tvs[[Sequence@@indsBefore,s,Sequence@@indsBetween,s,Sequence@@indsAfter]],{s,1,n}],Evaluate[Sequence@@itrTot]],
				{Length@indsNew}];
	
	ToTensor[Join[KeyDrop[Association@@t,{"Indices","Name"}],
					Association["Name"->TensorName[t]<>"-Auto",
								"Values"->vals,
								"Indices"->indsNew]]]
]


Tensor/:Component[t_Tensor,inds___List,opts:OptionsPattern[]]:=
Module[{indsPos,indsAbstr,indsAbstrUp,coordsPos,indsUp},
	If[Length[inds]=!=Total@Rank[t],
		Print["Tensor ", t," expected ",Total@Rank[t]," indices to select a component, but ", Length[inds], If[Length[inds]===1," index was ", " indices were "],"given."];
		Abort[]
	];
	indsUp=inds/.-sym_Symbol:>sym;
	coordsPos=Flatten[Position[Coordinates[t],#]&/@indsUp];
	indsAbstrUp=Indices[t]/.-sym_Symbol:>sym;
	indsAbstr=MapThread[If[MatchQ[#1,_Symbol],#2,-#2]&,{inds,indsAbstrUp}];
	
	Part[TensorValues[ShiftIndices[t,{Sequence@@indsAbstr},opts]],Sequence@@coordsPos]
]


Tensor/:TensorRules[t_Tensor,opts:OptionsPattern[]]:=
Module[{pmList,lhs},
	pmList=If[#==="Up",1,-1]&/@IndexPositions[t];
	lhs=pmList #&/@Tuples[Coordinates[t],Total@Rank[t]];
	(#->Component[t,#,opts])&/@lhs
]


Clear[validateSumIndices]
validateSumIndices[inds1_List,inds2_List]:=
If[Sort[inds1]=!=Sort[inds2],
		Print["Cannot add Tensors with different indices, ",Sort[inds1]," and ",Sort[inds2]];
		Abort[]
]


Clear[SumTensors]
Tensor/:SumTensors[t1_Tensor,t2_Tensor,opts:OptionsPattern[]]:=
Module[{simpFn,posInds,vals,inds,tvs,its,dims,itrs,local,indsLocal,indsFinal,tvFunc},

	If[AbstractQ[t1]||AbstractQ[t2],Print["Cannot sum Abstract Tensors."];Abort[]];
	If[TensorName@Metric[t1]=!=TensorName@Metric[t2],Print["Cannot sum Tensors with different metrics."];Abort[]];
	If[TensorName@Curve@t1=!=TensorName@Curve@t2,Print["Cannot sum Tensors on different curves."];Abort[]];
	
	simpFn=OptionValue["ActWith"];
	tvFunc=If[OnCurveQ@t1||OnCurveQ@t2,TensorValues,RawTensorValues];
			
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
	
	vals=Map[simpFn,
			Table[tvs[1][[Sequence@@indsLocal[1]]]+tvs[2][[Sequence@@indsLocal[2]]],Evaluate[Sequence@@itrs]],
			{Length@indsFinal}];
	
	ToTensor[Join[KeyDrop[Association@@t1,{"DisplayName","Name","Metric","IsMetric","Values","Indices","PossibleIndices"}],
					Association["IsMetric"->False,
								"Metric"->Metric[t1],
								"Indices"->indsFinal,
								"Values"->vals,
								"PossibleIndices"->posInds,
								"Name"->"("<>TensorName[t1]<>"+"<>TensorName[t2]<>")-Auto",
								"DisplayName"->"("<>TensorDisplayName[t1]<>"+"<>TensorDisplayName[t2]<>")"]]]
];
Tensor/:SumTensors[t1_Tensor,opts:OptionsPattern[]]:=t1;
Tensor/:SumTensors[t1_Tensor,t2__Tensor,opts:OptionsPattern[]]:=Fold[SumTensors[#1,#2,opts]&,t1,{t2}]
Tensor/:SumTensors[t1_Tensor,t2__Tensor,name_String,opts:OptionsPattern[]]:=SetTensorName[SumTensors[t1,t2,opts],name]
Tensor/:SumTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String},opts:OptionsPattern[]]:=SetTensorName[SumTensors[t1,t2,opts],{name,displayName}]


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
Tensor/:MultiplyTensors[t1_Tensor,t2_Tensor,opts:OptionsPattern[]]:=
Module[{simpFn,posInds,vals,inds,repeatedInds,tvs,dims,itrs,indsLocal,local,indsFinal,tvFunc},

	If[AbstractQ[t1]||AbstractQ[t2],Print["Cannot multiply Abstract Tensors."];Abort[]];
	If[TensorName@Metric[t1]=!=TensorName@Metric[t2],Print["Cannot multiply Tensors with different metrics."];Abort[]];
	If[TensorName@Curve@t1=!=TensorName@Curve@t2,Print["Cannot multiply Tensors on different curves."];Abort[]];
	
	simpFn=OptionValue["ActWith"];
	tvFunc=If[OnCurveQ@t1||OnCurveQ@t2,TensorValues,RawTensorValues];

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
	vals=Map[simpFn,
			Table[tvs[1][[Sequence@@indsLocal[1]]]tvs[2][[Sequence@@indsLocal[2]]],Evaluate[Sequence@@itrs]],
			{Length@indsFinal}];
	
	ToTensor[Join[KeyDrop[Association@@t1,{"DisplayName","Name","Metric","IsMetric","Values","Indices","PossibleIndices"}],
					Association["IsMetric"->False,
								"Metric"->Metric[t1],
								"Indices"->indsFinal,
								"Values"->vals,
								"PossibleIndices"->posInds,
								"Name"->"("<>TensorName[t1]<>"\[CenterDot]"<>TensorName[t2]<>")-Auto",
								"DisplayName"->"("<>TensorDisplayName[t1]<>"\[CenterDot]"<>TensorDisplayName[t2]<>")"]]]

];

Tensor/:MultiplyTensors[t1_Tensor,opts:OptionsPattern[]]:=t1;
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,opts:OptionsPattern[]]:=Fold[MultiplyTensors[#1,#2,opts]&,t1,{t2}]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,name_String,opts:OptionsPattern[]]:=SetTensorName[MultiplyTensors[t1,t2,opts],name]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String},opts:OptionsPattern[]]:=SetTensorName[MultiplyTensors[t1,t2,opts],{name,displayName}]


Clear[MultiplyTensorScalar]
Tensor/:MultiplyTensorScalar[t_Tensor,n_,opts:OptionsPattern[]]:=MultiplyTensorScalar[n,t,opts];
Tensor/:MultiplyTensorScalar[n_,t_Tensor,opts:OptionsPattern[]]:=
Module[{simpFn,vals,name,dispName,ratStr},
	If[AbstractQ[t],Print["Cannot multiply Abstract Tensors."];Abort[]];
	If[Not[MatchQ[n,(_Symbol|_Real|_Complex|_Integer|_Rational|_Times|_Plus|_SeriesData)]],Print["Cannot multiply a Tensor by a ", Head[n]];Abort[]];

	simpFn=OptionValue["ActWith"];
	vals= Map[simpFn[n #]&, RawTensorValues[t],{Total@Rank[t]}];
	
	ratStr=If[MatchQ[n,_Rational],n/.Rational[a_,b_]:>ToString[a]<>"/"<>ToString[b],ToString[n]];
	{name,dispName}=
	If[MatchQ[n,_Plus],
		{"(("<>ToString[n]<>")"<>TensorName[t]<>")-Auto","(("<>ratStr<>")\[CenterDot]"<>TensorDisplayName[t]<>")"},
		{"("<>ToString[n]<>TensorName[t]<>")-Auto","("<>ratStr<>"\[CenterDot]"<>TensorDisplayName[t]<>")"}
	];
	ToTensor[Join[KeyDrop[Association@@t,{"DisplayName","Name","IsMetric","Values","Metric"}],
					Association["IsMetric"->False,
								"Metric"->Metric[t],
								"Values"->vals,
								"DisplayName"->dispName,
								"Name"->name]]]
];
Tensor/:MultiplyTensorScalar[t1_Tensor,opts:OptionsPattern[]]:=t1;
Tensor/:MultiplyTensorScalar[n_,t1_Tensor,name_String,opts:OptionsPattern[]]:=SetTensorName[MultiplyTensorScalar[n,t1,opts],name]
Tensor/:MultiplyTensorScalar[t1_Tensor,n_,name_String,opts:OptionsPattern[]]:=MultiplyTensorScalar[n,t1,name,opts]
Tensor/:MultiplyTensorScalar[n_,t1_Tensor,{name_String,displayName_String},opts:OptionsPattern[]]:=SetTensorName[MultiplyTensorScalar[n,t1,opts],{name,displayName}]
Tensor/:MultiplyTensorScalar[t1_Tensor,n_,{name_String,displayName_String},opts:OptionsPattern[]]:=MultiplyTensorScalar[n,t1,{name,displayName},opts]


Clear[MergeTensors]
MergeTensors[expr_,opts:OptionsPattern[]]:=
Module[{expr1,expr2,expr3,expr4,simpFn,simpFnNest,nestNum},
	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];
	nestNum=OptionValue["NestQuantity"];
	expr1=Expand[expr]/.t1_Tensor t2__Tensor:>MultiplyTensors[t1,t2,"ActWith"->simpFnNest];
	expr2=expr1 /. n_ t_Tensor/;Not[MatchQ[n,_Tensor]]:>MultiplyTensorScalar[n,t,"ActWith"->simpFnNest];
	expr3=ContractIndices[expr2,"ActWith"->simpFnNest]/.Plus[t1_Tensor,t2__Tensor]:>SumTensors[t1,t2,"ActWith"->simpFnNest];
	expr4=If[MatchQ[expr3,_Tensor]||nestNum==0,expr3,MergeTensors[expr3,"ActWithNested"->simpFnNest,"ActWith"->simpFn,"NestQuantity"->(nestNum-1)]];
	If[MatchQ[expr4,_Tensor],ActOnTensorValues[simpFn,expr4],expr4]
]
MergeTensors[expr_,name_String,opts:OptionsPattern[]]:=SetTensorName[MergeTensors[expr,opts],name]
MergeTensors[expr_,{name_String,dispName_String},opts:OptionsPattern[]]:=SetTensorName[MergeTensors[expr,opts],{name,dispName}]


MergeTensors[t_Tensor,opts:OptionsPattern[]]:=
Module[{simpFnNest,simpFn},
	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];
	ContractIndices[t,"ActWith"->simpFn]
]


Clear[TraceReverse]
Tensor/:TraceReverse[t_Tensor,{name_String,dispName_String},opts:OptionsPattern[]]:=
Module[{met,tTr,simpFn,simpFnNest,a,b,c},

	If[Rank[t]=!={0,2},
		Print["TraceReverse is built only for Tensors of Rank {0,2}"];
		Abort[]
	];

	{a,b}=Indices[t];
	c=SelectFirst[PossibleIndices[t],Not@MemberQ[({a,b}/.-n_:>n),#]&];
	MergeTensors[t[a,b] - 2 Metric[t][a,b] t[c,-c]/Dimensions[t],{name,dispName},opts]
];
Tensor/:TraceReverse[t_Tensor,name_String,opts:OptionsPattern[]]:=TraceReverse[t,{name,name},opts]
Tensor/:TraceReverse[t_Tensor,opts:OptionsPattern[]]:=TraceReverse[t,{TensorName[t]<>"TraceReverse",TensorDisplayName[t]<>"Bar"},opts]


Clear[ReorderTensorIndices]
Tensor/:ReorderTensorIndices[t_Tensor,inds_List,{name_String,displayName_String}]:=
Module[{is,pis},
	is=Indices[t];

	If[Sort@inds=!=Range[Total@Rank@t],
		Print["Tensor ", t, " is of rank ", Total@Rank@t  ". ReorderTensorIndices requires a list of index positions including exactly the numbers ",Range[Total@Rank@t], " in some order." ];
		Abort[]
	];

	ToTensor[Join[KeyDrop[Association@@t,{"DisplayName","Name","Metric","IsMetric","Indices"}],
			Association["Metric"->Metric[t],
						"IsMetric"->False,
						"Values"->Transpose[RawTensorValues[t],inds],
						"DisplayName"->displayName,
						"Name"->name,
						"Indices"->(is[[#]]&/@inds)]]]
]
Tensor/:ReorderTensorIndices[t_Tensor,inds_List,name_String]:=ReorderTensorIndices[t,inds,{name,name}]
Tensor/:ReorderTensorIndices[t_Tensor,inds_List]:=ReorderTensorIndices[t,inds,{TensorName[t]<>"Reorder"<>ToString[inds],TensorDisplayName[t]}]


Clear[SymmetrizeTensor]
Tensor/:SymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},{name_String,displayName_String},opts:OptionsPattern[]]:=
Module[{ips,inds,inds2,indsBefore,indsBetween,indsAfter,indsA,indsB},
	
	If[pos1>pos2,
	Print["Indices must be given to SymmetrizeTensor in ascending order. Given as ",{pos1, pos2} ];
		Abort[]
	];
	If[pos1>Total@Rank@t||pos2>Total@Rank@t,
	Print["Tensor ", t, " is of rank ", Total@Rank@t  ". Cannot symmetrize on indices of positions ",{pos1, pos2} ];
		Abort[]
	];
	If[pos1==pos2,
	Print["Cannot symmetrize on indices of the same position: ",pos1 ];
		Abort[]
	];
	ips=IndexPositions[t];
	If[ips[[pos1]]=!=ips[[pos2]],
		Print["Symmetrize indices must be both contravariant or covariant"];
		Abort[]
	];

	indsBefore=Range[1,pos1-1];
	indsBetween=Range[pos1+1,pos2-1];
	indsAfter=Range[pos2+1,Total@Rank@t];
	
	inds =Indices[t];
	indsA=Part[inds,#]&/@Flatten[{indsBefore,pos1,indsBetween,pos2,indsAfter }];
	indsB=Part[inds,#]&/@Flatten[{indsBefore,pos2,indsBetween,pos1,indsAfter }];
	
	MergeTensors[1/2 (t@@indsA+t@@indsB),{name,displayName},opts]
]
Tensor/:SymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},name_String,opts:OptionsPattern[]]:=SymmetrizeTensor[t,{pos1,pos2},{name,name},opts]
Tensor/:SymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},opts:OptionsPattern[]]:=SymmetrizeTensor[t,{pos1,pos2},{TensorName[t]<>"Symmetric"<>ToString[{pos1,pos2}],TensorDisplayName[t]<>"("<>ToString[pos1]<>","<>ToString[pos2]<>")"},opts]


Clear[AntisymmetrizeTensor]
Tensor/:AntisymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},{name_String,displayName_String},opts:OptionsPattern[]]:=
Module[{ips,inds,inds2,indsBefore,indsBetween,indsAfter,indsA,indsB},
	
	If[pos1>pos2,
	Print["Indices must be given to AntisymmetrizeTensor in ascending order. Given as ",{pos1, pos2} ];
		Abort[]
	];
	If[pos1>Total@Rank@t||pos2>Total@Rank@t,
	Print["Tensor ", t, " is of rank ", Total@Rank@t  ". Cannot symmetrize on indices of positions ",{pos1, pos2} ];
		Abort[]
	];
	If[pos1==pos2,
	Print["Cannot antisymmetrize on indices of the same position: ",pos1 ];
		Abort[]
	];
	ips=IndexPositions[t];
	If[ips[[pos1]]=!=ips[[pos2]],
		Print["Antisymmetrize indices must be both contravariant or covariant"];
		Abort[]
	];

	indsBefore=Range[1,pos1-1];
	indsBetween=Range[pos1+1,pos2-1];
	indsAfter=Range[pos2+1,Total@Rank@t];
	
	inds =Indices[t];
	indsA=Part[inds,#]&/@Flatten[{indsBefore,pos1,indsBetween,pos2,indsAfter }];
	indsB=Part[inds,#]&/@Flatten[{indsBefore,pos2,indsBetween,pos1,indsAfter }];
	
	MergeTensors[1/2 (t@@indsA-t@@indsB),{name,displayName},opts]
]
Tensor/:AntisymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},name_String,opts:OptionsPattern[]]:=AntisymmetrizeTensor[t,{pos1,pos2},{name,name},opts]
Tensor/:AntisymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},opts:OptionsPattern[]]:=AntisymmetrizeTensor[t,{pos1,pos2},{TensorName[t]<>"Antisymmetric"<>ToString[{pos1,pos2}],TensorDisplayName[t]<>"["<>ToString[pos1]<>","<>ToString[pos2]<>"]"},opts]


End[];

EndPackage[];
