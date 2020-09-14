(* ::Package:: *)

BeginPackage["GeneralRelativityTensors`BasicTensorManipulation`",{"GeneralRelativityTensors`BasicTensors`"},{"GeneralRelativityTensors`Utils`"}];


ContractIndices::usage="ContractIndices[t,n] contracts all repeated indices of \
Tensor t, returning the resulting lower-rank Tensor with name n.
ContractIndices[t] is equivalent to \
ContractIndices[t,{TensorName[t],TensorDisplayName[t]}].";
ShiftIndices::usage="ShiftIndices[t,inds] raises and/or lowers the indices of \
Tensor t according to the given List inds, adjusting \
the values using the Tensor's associated metric. \
ShiftIndices[te,inds] shifts indices for all the terms in \
TensorExpression te.";
TensorRules::usage="TensorRules[t] returns a List of Rules with possible \
coordinates of Tensor t as keys and TensorValues as values.";
MergeTensors::usage="MergeTensors[expr,n] calls MultiplyTensors, MultiplyTensorScalar, \
AddTensors, and ContractIndices to merge the Tensor expression expr into one Tensor with TensorName n.
MergeTensors[expr] merges the Tensor expression expr and \
forms a new TensorName and TensorDisplayName from a combination of the Tensors making up the expression.";
AddTensors::usage="AddTensors[t1,t2,...,n] sums the Tensors t1, t2, \
etc., forming a new Tensor with TensorName n.
AddTensors[t1,t2,...] sums the Tensors t1, t2, etc., and \
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
(*TraceReverse::usage="TraceReverse[t,n] returns the trace reversed version of \
the Tensor t with the TensorName n.
TraceReverse[t] is equivalent to TraceReverse[t,n], \
but with the returned TensorName and DisplayTensorName auto-generated.";
*)

RepeatedIndexQ::usage="RepeatedIndexQ[t] returns True if the Tensor \
t has repeated indices which can be traced.";
Component::usage="Component[t,inds] returns the component of Tensor t \
with (appropriately covariant and contravariant) List of indices inds. \
All elements of inds must be Coordinates of t.";

ReorderTensorIndices::usage="ReorderTensorIndices[t,order,n] returns the Tensor t renamed n with its indices \
reordered as given by order, which is a List including all integers from 1 to the rank of the Tensor.
ReorderTensorIndices[t,order] is equivalent, but with an automatically generated name for the new Tensor.";
(*AntisymmetrizeTensor::usage="AntisymmetrizeTensor[t,{pos1,pos2},n] returns the Tensor t, antisymmetrized on its indices in \
positions pos1,pos2.
AntisymmetrizeTensor[t,{pos1,pos2}] is equivalent, but with an automatically generated name for the new Tensor.";
SymmetrizeTensor::usage="SymmetrizeTensor[t,{pos1,pos2},n] returns the Tensor t, symmetrized on its indices in \
positions pos1,pos2.
SymmetrizeTensor[t,{pos1,pos2}] is equivalent, but with an automatically generated name for the new Tensor.";*)


Begin["`Private`"];


Options[ShiftIndices]={"ActWith"->Identity};
Options[Component]=Options[ShiftIndices];
Options[TensorRules]=Options[ShiftIndices];
Options[ContractIndices]=Options[ShiftIndices];
Options[AddTensors]=Options[ShiftIndices];
Options[MultiplyTensors]=Join[Options[ShiftIndices],{"ForceMerge"->False}];
Options[MultiplyTensorScalar]=Options[ShiftIndices];
(*Options[TraceReverse]=Join[Options[ShiftIndices],{"ActWithNested"->Identity}];
Options[SymmetrizeTensor]=Options[TraceReverse];
Options[AntisymmetrizeTensor]=Options[TraceReverse];
*)
(*Options[MergeTensors]=DeleteDuplicates@Join[Options[TraceReverse],Options[MultiplyTensors],{"NestQuantity"->3}];*)
Options[MergeTensors]=DeleteDuplicates@Join[Options[MultiplyTensors],{"ActWithNested"->Identity},{"NestQuantity"->3}];

DocumentationBuilder`OptionDescriptions["ShiftIndices"] = {"ActWith"->"Function that is applied to the elements of the tensor as they are calculated."};
DocumentationBuilder`OptionDescriptions["Component"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["TensorRules"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["ContractIndices"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["AddTensors"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
DocumentationBuilder`OptionDescriptions["MultiplyTensors"] = Join[DocumentationBuilder`OptionDescriptions["ShiftIndices"],
{"ForceMerge"->"Boolean that must be set to True to merge Tensors with different Metrics into one."}];
DocumentationBuilder`OptionDescriptions["MultiplyTensorScalar"] = DocumentationBuilder`OptionDescriptions["ShiftIndices"];
(*DocumentationBuilder`OptionDescriptions["TraceReverse"] = Join[DocumentationBuilder`OptionDescriptions["ShiftIndices"],
{"ActWithNested"->"Function that is applied to the elements of the tensor and also passed to any other functions called internally."}];
DocumentationBuilder`OptionDescriptions["SymmetrizeTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];
DocumentationBuilder`OptionDescriptions["AntisymmetrizeTensor"] = DocumentationBuilder`OptionDescriptions["TraceReverse"];*)
(*DocumentationBuilder`OptionDescriptions["MergeTensors"] = DeleteDuplicates@Join[DocumentationBuilder`OptionDescriptions["TraceReverse"],
																				DocumentationBuilder`OptionDescriptions["MultiplyTensors"],
{"NestQuantity"->"Number of times MergeTensors can call itself as it continues to try to create one Tensor from an expression"}];
*)
DocumentationBuilder`OptionDescriptions["MergeTensors"] = DeleteDuplicates@Join[DocumentationBuilder`OptionDescriptions["MultiplyTensors"],
{"ActWithNested"->"Function that is applied to the elements of the tensor and also passed to any other functions called internally."},
{"NestQuantity"->"Number of times MergeTensors can call itself as it continues to try to create one Tensor from an expression"}];



RepeatedIndexQ[t_Tensor]:=Length[DeleteDuplicates@(Indices[t]/.-sym_Symbol:>sym)]<Length[Indices[t]];
t_Tensor[inds__]/;Complement[{inds}/.-sym_Symbol:>sym,Flatten[PossibleIndices[t]]]==={}:=ShiftIndices[t,{inds}]
t_Tensor[inds__]/;(Coordinates[t]=!=Undefined)&&Complement[{inds}/.-sym_Symbol:>sym,Flatten[Coordinates[t]]]==={}:=Component[t,{inds}]
t_Tensor[inds__]:=(Print["The given indices ",{inds}, " are not entirely in the List of PossibleIndices, or Coordinates of ", t, ", and they are not not all Patterns."];Abort[])


(*The commented code is for when we add unique indices with dollar signs*)
(*UniqueIndices[t_Tensor,n_Integer/;n>=0]:=Unique[PadRight[{},n,PossibleIndices[t]]]*)


def@
ShiftIndices[t_Tensor,inds_List,opts:OptionsPattern[]]:=
Module[{tests,allInds,tempInds,tTemp,posInds,uds,tempT},
	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."}};
	TestOptions[tests,{opts}];

	ValidateIndices[t,inds];
	
	uds=IndexPositions[t]/.{"Up"->1,"Down"->-1};
	tempInds=uds(Unique/@(Indices[t]/.-sym_Symbol:>sym));
	tempT=SetIndices[t,tempInds];
	
	Fold[shiftIndex[#1,#2,OptionValue["ActWith"]]&,tempT,Thread[{Range@Length[inds],inds}]]
]


testDef@
shiftIndex[t_Tensor,{pos_Integer,ind_},simpFn_]:=
Module[{gOrInvG,inds,currentUD,newUD,tvs,indsBefore,indsAfter,dims,
		itrBefore,itrAfter,vals,i,itrTot,itr,indUD,newMet,newInds},
	
	indUD=If[MatchQ[ind,_Symbol],"Up","Down"];
	currentUD=IndexPositions[t];

	If[pos>Length@currentUD,Print["Tensor ", t, " has only ", Length@currentUD," indices. Cannot raise/lower at position ", pos,"."];AbortVerbose[]];
	newUD=ReplacePart[currentUD,pos->indUD];
	inds=Indices[t];
	
	vals=simpFn@
		Which[currentUD[[pos]]===indUD,
			RawTensorValues[t],
			
			RawTensorValues[TensorName[t],newUD]===Undefined,
			tvs=RawTensorValues[t];
			dims=ManifoldDimensions[t,PerIndex->True];

			indsBefore=Table[itr[ii],{ii,1,pos-1}];
			indsAfter=Table[itr[ii],{ii,pos+1,Length@currentUD}];
		
			itrBefore = MapThread[{#1,1,#2}&,{indsBefore,Take[dims,Length[indsBefore]]}];
			itrAfter=MapThread[{#1,1,#2}&,{indsAfter,Take[dims,-Length[indsAfter]]}];
			itrTot=Join[itrBefore,{{i,1,dims[[pos]]}},itrAfter];
			
			gOrInvG = RawTensorValues[If[indUD==="Up",InverseMetric[t,"PerIndex"->True][[pos]],Metric[t,"PerIndex"->True][[pos]]]];
	
			Table[Sum[gOrInvG[[i,s]]tvs[[Sequence@@indsBefore,s,Sequence@@indsAfter]],{s,1,dims[[pos]]}],Evaluate[Sequence@@itrTot]],
			
			True,
			RawTensorValues[TensorName[t],newUD]
	];

	newInds=ReplacePart[inds,pos->ind];
	newMet=If[MetricQ[t]&&(If[MatchQ[#,_Symbol],"Up","Down"]&/@newInds)==={"Down","Down"},"Self",stripDollarSigns/@Metric[t,"PerIndex"->True]];

	ToTensor[KeySort@Join[KeyDrop[Association@@t,{"Indices","Metric","Values"}],
					Association["Values"->vals,
								"Metric"->newMet,
								"Indices"->newInds]]]
]


def@
stripDollarSigns[t_Tensor]:=
Module[{uds,indsUp,newInds},
	indsUp=Indices[t]/.-sym_Symbol:>sym;
	uds=IndexPositions[t]/.{"Up"->1,"Down"->-1};
	newInds=uds(stripDollarSignFromSymbol/@indsUp);

	SetIndices[t,newInds]
]


def@
ContractIndices[t_Tensor,opts:OptionsPattern[]]:=
Module[{tests},
	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."}};
	TestOptions[tests,{opts}];
	
	NestWhile[contractIndex[#,OptionValue["ActWith"]]&,t,RepeatedIndexQ]
];

reDef@ContractIndices[t_Tensor,name_String,opts:OptionsPattern[]]:=SetTensorName[ContractIndices[t,opts],name];
reDef@ContractIndices[t_Tensor,{name_String,displayName_String},opts:OptionsPattern[]]:=SetTensorName[ContractIndices[t,opts],{name,displayName}];
reDef@ContractIndices[expr_,opts:OptionsPattern[]]:=
Module[{expr1,tests},
	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."}};
	TestOptions[tests,{opts}];
	expr1 = contractIndicesByTerm[expr,opts];
	expr1/.t_Tensor:>ContractIndices[t,opts]
]
reDef@ContractIndices[te_TensorExpression,opts:OptionsPattern[]]:=ToTensorExpression[ContractIndices[Normal@te,opts],TensorExpressionDisplayName[te]]


testDef@
contractIndex[t_Tensor,simpFn_]:=
Module[{indsUp,rptInd,rptIndsPos,currentUD,inds,indsNew,dims,vals,
	indsBefore,indsBetween,indsAfter,itrBefore,itrBetween,itrAfter,itrTot,tvs,itr,
	posIndsNew,metsNew,coordsNew,dimsNew},
	
	currentUD=IndexPositions[t];

	inds=Indices[t];
	indsUp=inds/.-sym_Symbol:>sym;
	rptInd=First[If[Count[indsUp,#]===2,#,##&[]]&/@DeleteDuplicates@indsUp];
	rptIndsPos=Flatten@Position[indsUp,rptInd];

	indsNew=Delete[inds,{#}&/@Flatten@rptIndsPos];

	tvs=RawTensorValues[t];
	dims=ManifoldDimensions[t,"PerIndex"->True];

	indsBefore=Table[itr[ii],{ii,1,rptIndsPos[[1]]-1}];
	indsBetween=Table[itr[ii],{ii,rptIndsPos[[1]]+1,rptIndsPos[[2]]-1}];
	indsAfter=Table[itr[ii],{ii,rptIndsPos[[2]]+1,Length@currentUD}];

	itrBefore = MapThread[{#1,1,#2}&,{indsBefore,Take[dims,Length[indsBefore]]}];
	itrBetween=MapThread[{#1,1,#2}&,{indsBetween,Take[dims,{First@rptIndsPos+1,Last@rptIndsPos-1}]}];
	itrAfter=MapThread[{#1,1,#2}&,{indsAfter,Take[dims,-Length[indsAfter]]}];

	itrTot=Join[itrBefore,itrBetween,itrAfter];
	
	vals = Map[simpFn,
				Table[Sum[tvs[[Sequence@@indsBefore,s,Sequence@@indsBetween,s,Sequence@@indsAfter]],{s,1,dims[[rptIndsPos[[1]]]]}],Evaluate[Sequence@@itrTot]],
				{Length@indsNew}];

	posIndsNew=Delete[PossibleIndices[t,"PerIndex"->True],{#}&/@Flatten@rptIndsPos];
	metsNew=Delete[Metric[t,"PerIndex"->True],{#}&/@Flatten@rptIndsPos];
	coordsNew=Delete[Coordinates[t,"PerIndex"->True],{#}&/@Flatten@rptIndsPos];
	dimsNew=Delete[dims,{#}&/@Flatten@rptIndsPos];
	
	If[Length@indsNew===0,
		vals,
		ToTensor[KeySort@Join[Association@@t,
			Association["Name"->TensorName[t]<>"-Auto",
						"Values"->vals,
						"Indices"->indsNew,
						"PossibleIndices"->posIndsNew,
						"Metric"->metsNew,
						"Dimensions"->dimsNew,
						"Coordinates"->coordsNew]]]
	]
]


def@
contractIndicesByTerm[expr_,opts:OptionsPattern[]]:=
Module[{rules,expr1},
	
	rules=rules={(g:TensorPattern[_,{x_Symbol,y_Symbol}])(c:TensorPattern[_,{c1___,-x_Symbol,c2___}])/;MetricQ[g]:> ShiftIndices[c,{c1,y,c2},opts],
	(g:TensorPattern[_,{x_Symbol,y_Symbol}])(c:TensorPattern[_,{c1___,-y_Symbol,c2___}])/;MetricQ[g]:> ShiftIndices[c,{c1,x,c2},opts],
		(g:TensorPattern[_,{-x_Symbol,-y_Symbol}])(c:TensorPattern[_,{c1___,x_Symbol,c2___}])/;MetricQ[g]:> ShiftIndices[c,{c1,-y,c2},opts],
	(g:TensorPattern[_,{-x_Symbol,-y_Symbol}])(c:TensorPattern[_,{c1___,y_Symbol,c2___}])/;MetricQ[g]:> ShiftIndices[c,{c1,-x,c2},opts],
		(g:TensorPattern[_,{-x_Symbol,y_Symbol}])(c:TensorPattern[_,{c1___,x_Symbol,c2___}])/;MetricQ[g]:>  ShiftIndices[c,{c1,y,c2},opts],
		(g:TensorPattern[_,{-x_Symbol,y_Symbol}])(c:TensorPattern[_,{c1___,-y_Symbol,c2___}])/;MetricQ[g]:>  ShiftIndices[c,{c1,-x,c2},opts],
		(g:TensorPattern[_,{x_Symbol,-y_Symbol}])(c:TensorPattern[_,{c1___,-x_Symbol,c2___}])/;MetricQ[g]:> ShiftIndices[c,{c1,-y,c2},opts],
		(g:TensorPattern[_,{x_Symbol,-y_Symbol}])(c:TensorPattern[_,{c1___,y_Symbol,c2___}])/;MetricQ[g]:>  ShiftIndices[c,{c1,x,c2},opts],
		(g:TensorPattern[_,{x_Symbol,-x_Symbol}]|TensorPattern[_,{-x_Symbol,x_Symbol}])/;MetricQ[g]:> ManifoldDimensions[g]};
		
	expr1 = expr//.rules;
	expr1//.Times[a___,b:TensorPattern[_,{___,x_Symbol,___}],c:TensorPattern[_,{___,-x_Symbol,___}]]:>a MultiplyTensors[b,c,opts]
]


reDef@
Component[t_Tensor,inds_List,opts:OptionsPattern[]]:=
Module[{indsPos,indsAbstr,indsAbstrUp,coordsPos,indsUp,tests,coords,tfList},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."}};
	TestOptions[tests,{opts}];
	
	indsUp=inds/.-sym_Symbol:>sym;
	coords=Coordinates[t,"PerIndex"->True];
	If[Length[inds]=!=Total@Rank[t],
		Print["Tensor ", t," expected ",Total@Rank[t]," indices to select a component, but ", Length[inds], If[Length[inds]===1," index was ", " indices were "],"given."];
		AbortVerbose[]
	];
	
	tfList=MapThread[MemberQ[#1,#2]&,{coords,indsUp}];
	If[DeleteDuplicates@tfList=!={True},
		MapThread[If[Not@#1,Print["Given index ", #2, " not found in List of Coordinates ", #3],Nothing]&,{tfList,indsUp,coords}];
		AbortVerbose[];
	];

	coordsPos=Flatten@MapThread[Position[#1,#2]&,{coords,indsUp}];
	indsAbstrUp=Indices[t]/.-sym_Symbol:>sym;
	indsAbstr=MapThread[If[MatchQ[#1,_Symbol],#2,-#2]&,{inds,indsAbstrUp}];

	Part[TensorValues[ShiftIndices[t,{Sequence@@indsAbstr},opts]],Sequence@@coordsPos]
]


def@
TensorRules[t_Tensor,opts:OptionsPattern[]]:=
Module[{pmList,lhs,tests},
	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."}};
	TestOptions[tests,{opts}];

	pmList=If[#==="Up",1,-1]&/@IndexPositions[t];
	lhs=pmList #&/@Tuples[Coordinates[t],Total@Rank[t]];
	(#->Component[t,#,opts])&/@lhs
]


def@
AddTensors[t1_Tensor,t2_Tensor,opts:OptionsPattern[]]:=
Module[{simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."}};
	TestOptions[tests,{opts}];

	simpFn=OptionValue["ActWith"];
	addTensors[t1,t2,simpFn]
]
reDef@AddTensors[t1_Tensor,opts:OptionsPattern[]]:=t1;
reDef@AddTensors[t1_Tensor,t2__Tensor,opts:OptionsPattern[]]:=Fold[AddTensors[#1,#2,opts]&,t1,{t2}]
reDef@AddTensors[t1_Tensor,t2__Tensor,name_String,opts:OptionsPattern[]]:=SetTensorName[AddTensors[t1,t2,opts],name]
reDef@AddTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String},opts:OptionsPattern[]]:=SetTensorName[AddTensors[t1,t2,opts],{name,displayName}]


testDef@
addTensors[t1_Tensor,t2_Tensor,simpFn_]:=
Module[{vals,inds,tvs,its,dims,itrs,local,indsLocal,indsFinal},

	If[TensorName/@Metric[t1,"PerIndex"->True]=!=TensorName/@Metric[t2,"PerIndex"->True],Print["Cannot sum Tensors with different metrics:",
	Metric[t1,"PerIndex"->True], " and ", Metric[t2,"PerIndex"->True]];
	AbortVerbose[]];
	
	inds[1]=Indices[t1];
	inds[2]=Indices[t2];
	If[Sort[inds[1]]=!=Sort[inds[2]],Print["Cannot add Tensors ",t1," and ",t2," with different indices."];AbortVerbose[]];

	local[sym_]:=If[MatchQ[sym,-_Symbol],Symbol["cov"<>ToString[-sym]],Symbol["con"<>ToString[sym]]];
	indsLocal[1]=local/@inds[1];
	indsLocal[2]=local/@inds[2];

	indsLocal["Tot"]=indsLocal[1];
	indsFinal=indsLocal["Tot"]/.(local[#]->#&/@inds[1]);

	tvs[1]=RawTensorValues[t1];
	tvs[2]=RawTensorValues[t2];
	dims=ManifoldDimensions[t1,"PerIndex"->True];

	itrs = MapThread[{#1,1,#2}&,{indsLocal["Tot"],dims}];
	
	vals=Map[simpFn,
			Table[tvs[1][[Sequence@@indsLocal[1]]]+tvs[2][[Sequence@@indsLocal[2]]],Evaluate[Sequence@@itrs]],
			{Length@indsFinal}];
	
	ToTensor[KeySort@Join[Association@@t1,
					Association["MetricQ"->False,
								"Metric"->Metric[t1,"PerIndex"->True],
								"Values"->vals,
								"Name"->"("<>TensorName[t1]<>"+"<>TensorName[t2]<>")-Auto",
								"DisplayName"->"("<>TensorDisplayName[t1]<>"+"<>TensorDisplayName[t2]<>")"]]]
]


def@
MultiplyTensors[t1_Tensor,t2_Tensor,opts:OptionsPattern[]]:=
Module[{simpFn,fm,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ForceMerge"->{BooleanQ,"ForceMerge of MultiplyTensors must be True or False."}};
	TestOptions[tests,{opts}];

	simpFn=OptionValue["ActWith"];
	fm=OptionValue["ForceMerge"];

	If[DeleteDuplicates[(TensorName/@Metric[t1,"PerIndex"->True])]===DeleteDuplicates[(TensorName/@Metric[t2,"PerIndex"->True])],
		multiplyTensors[t1,t2,simpFn],
			
		If[fm,
			multiplyTensors[t1,t2,simpFn],
			Print["Default behavior is not to multiply Tensors with different metrics."];
			Print["To force this merge and create a Tensor with multiple Metrics use the Option \"ForceMerge\"->True."];
			AbortVerbose[]
		]
	]
];
reDef@MultiplyTensors[t1_Tensor,opts:OptionsPattern[]]:=t1;
reDef@MultiplyTensors[t1_Tensor,t2__Tensor,opts:OptionsPattern[]]:=Fold[MultiplyTensors[#1,#2,opts]&,t1,{t2}];
reDef@MultiplyTensors[t1_Tensor,t2__Tensor,name_String,opts:OptionsPattern[]]:=SetTensorName[MultiplyTensors[t1,t2,opts],name];
reDef@MultiplyTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String},opts:OptionsPattern[]]:=SetTensorName[MultiplyTensors[t1,t2,opts],{name,displayName}];


testDef@
multiplyTensors[t1_Tensor,t2_Tensor,simpFn_]:=
Module[{vals,inds,repeatedInds,tvs,dims,itrs,
	indsLocal,local,indsFinal,mmVal},

	mmVal[func_]:=Join[func[t1,"PerIndex"->True],func[t2,"PerIndex"->True]];

	inds[1]=Indices[t1];
	inds[2]=Indices[t2];
	validateProductIndices[inds[1],inds[2]];
	
	local[sym_]:=If[MatchQ[sym,-_Symbol],Symbol["cov"<>ToString[-sym]],Symbol["con"<>ToString[sym]]];
	indsLocal[1]=local/@inds[1];
	indsLocal[2]=local/@inds[2];
	indsLocal["Tot"]=Join[indsLocal[1],indsLocal[2]];
	indsFinal=indsLocal["Tot"]/.(local[#]->#&/@Join[inds[1],inds[2]]);

	tvs[1]=RawTensorValues[t1];
	tvs[2]=RawTensorValues[t2];
	dims=mmVal[ManifoldDimensions];

	itrs = MapThread[{#1,1,#2}&,{indsLocal["Tot"],dims}];

	vals=Map[simpFn,
			Table[tvs[1][[Sequence@@indsLocal[1]]]tvs[2][[Sequence@@indsLocal[2]]],Evaluate[Sequence@@itrs]],
			{Length@indsFinal}];
	
	ToTensor[KeySort@Association["MetricQ"->False,
								"Metric"->mmVal[Metric],
								"Indices"->indsFinal,
								"Coordinates"->mmVal[Coordinates],
								"Values"->vals,
								"PossibleIndices"->mmVal[PossibleIndices],
								"Name"->"("<>TensorName[t1]<>"\[CenterDot]"<>TensorName[t2]<>")-Auto",
								"DisplayName"->"("<>TensorDisplayName[t1]<>"\[CenterDot]"<>TensorDisplayName[t2]<>")"]]
]


testDef@
validateProductIndices[inds1_List,inds2_List]:=
Module[{indsUp,repeatedInds,inds,toCov},

	toCov[expr_]:=expr/.-sym_Symbol:>sym;
	inds=Join[inds1,inds2];
	indsUp=toCov[inds];
	repeatedInds=Cases[inds,#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp]);

	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];
		AbortVerbose[]
	];

	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],toCov[#[[1]]],##&[]]&/@repeatedInds];
		AbortVerbose[]
	];
]


def@
MultiplyTensorScalar[n_,t_Tensor,opts:OptionsPattern[]]:=
Module[{simpFn,vals,name,dispName,l1,l2,l3,tests,seq},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."}};
	TestOptions[tests,{opts}];
	seq=Sequence[_Symbol,_Symbol[__Symbol],_Real,_Complex,_Integer,_Rational,_Times,_Plus,_SeriesData];

	If[Not[MatchQ[n,(Alternatives[seq,Power[Alternatives[seq],Alternatives[seq]]])]],Print["Cannot multiply a Tensor by a ", Head[n]];AbortVerbose[]];
	simpFn=OptionValue["ActWith"];
	vals= Map[simpFn[n #]&, RawTensorValues[t],{Total@Rank[t]}];
	
	l1=If[MatchQ[n,_Times],n/.tt_Times:>List@@tt,{n}];
	l2=l1/.{Rational[a_,b_]:>"("<>ToString[a]<>"/"<>ToString[b]<>")",Power[a_,b_]:>\!\(\*
TagBox[
StyleBox[
RowBox[{"\"\<\\!\\(\\*SuperscriptBox[\\(\>\"", "<>", 
RowBox[{"ToString", "[", "a", "]"}], "<>", "\"\<\\), \\(\>\"", "<>", 
RowBox[{"ToString", "[", "b", "]"}], "<>", "\"\<\\)]\\)\>\""}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)};
	l3=StringJoin[l2//.{a___,s:(_Symbol[__Symbol]|_Symbol|_Real|_Complex|_Integer),b___}:>{a,"("<>ToString[s]<>")",b}];
	{name,dispName}=
	If[MatchQ[n,_Plus],
		{"(("<>ToString[n]<>")"<>TensorName[t]<>")-Auto","(("<>l3<>")\[CenterDot]"<>TensorDisplayName[t]<>")"},
		{"("<>ToString[n]<>TensorName[t]<>")-Auto","("<>l3<>"\[CenterDot]"<>TensorDisplayName[t]<>")"}
	];
	ToTensor[KeySort@Join[KeyDrop[Association@@t,{"DisplayName","Name","MetricQ","Values","Metric"}],
					Association["MetricQ"->False,
								"Metric"->Metric[t,"PerIndex"->True],
								"Values"->vals,
								"DisplayName"->dispName,
								"Name"->name]]]
];
reDef@MultiplyTensorScalar[t_Tensor,n_,opts:OptionsPattern[]]:=MultiplyTensorScalar[n,t,opts];
reDef@MultiplyTensorScalar[t1_Tensor,opts:OptionsPattern[]]:=t1;
reDef@MultiplyTensorScalar[n_,t1_Tensor,name_String,opts:OptionsPattern[]]:=SetTensorName[MultiplyTensorScalar[n,t1,opts],name];
reDef@MultiplyTensorScalar[t1_Tensor,n_,name_String,opts:OptionsPattern[]]:=MultiplyTensorScalar[n,t1,name,opts];
reDef@MultiplyTensorScalar[n_,t1_Tensor,{name_String,displayName_String},opts:OptionsPattern[]]:=SetTensorName[MultiplyTensorScalar[n,t1,opts],{name,displayName}];
reDef@MultiplyTensorScalar[t1_Tensor,n_,{name_String,displayName_String},opts:OptionsPattern[]]:=MultiplyTensorScalar[n,t1,{name,displayName},opts];


def@
MergeTensors[expr_,opts:OptionsPattern[]]:=
Module[{expr1,expr2,expr3,expr4,expr5,simpFn,simpFnNest,nestNum,exprExpand,tests,fm},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."},
			"NestQuantity" ->{(IntegerQ[#]&&NonNegative[#])&,"OptionValue of NestQuantity must be a non negative Integer."},
			"ForceMerge"->{BooleanQ,"ForceMerge of MultiplyTensors must be True or False."}};
	TestOptions[tests,{opts}];

	simpFnNest=OptionValue["ActWithNested"];
	fm=OptionValue["ForceMerge"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];
	nestNum=OptionValue["NestQuantity"];
	
	exprExpand=Expand[expr];
	expr1=If[Cases[{exprExpand},Times[___, _Tensor ,__Tensor],3]=!={},exprExpand/.t1_Tensor t2__Tensor:>MultiplyTensors[t1,t2,"ActWith"->simpFnNest,"ForceMerge"->fm],exprExpand];
	expr2=If[Cases[{expr1},Times[n_,_Tensor]/;Not[MatchQ[n,_Tensor]],3]=!={},expr1/.n_ t_Tensor/;Not[MatchQ[n,_Tensor]]:>MultiplyTensorScalar[n,t,"ActWith"->simpFnNest],expr1];
	expr3=ContractIndices[expr2];
	expr4=If[Cases[{expr3},Plus[_Tensor,__Tensor],3]=!={},expr3/.Plus[t1_Tensor,t2__Tensor]:>AddTensors[t1,t2,"ActWith"->simpFnNest],expr3];
	expr5=If[MatchQ[expr4,_Tensor]||nestNum==0,expr4,MergeTensors[expr4,"ActWithNested"->simpFnNest,"ActWith"->simpFn,"NestQuantity"->(nestNum-1)]];

	If[MatchQ[expr5,_Tensor],ActOnTensorValues[simpFn,expr5],expr5]
];
reDef@MergeTensors[expr_,name_String,opts:OptionsPattern[]]:=SetTensorName[MergeTensors[expr,opts],name];
reDef@MergeTensors[expr_,{name_String,dispName_String},opts:OptionsPattern[]]:=SetTensorName[MergeTensors[expr,opts],{name,dispName}];


reDef@
MergeTensors[t_Tensor,opts:OptionsPattern[]]:=
Module[{simpFnNest,simpFn,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."},
			"NestQuantity" ->{(IntegerQ[#]&&NonNegative[#])&,"OptionValue of NestQuantity must be a non negative Integer."},
			"ForceMerge"->{BooleanQ,"ForceMerge of MultiplyTensors must be True or False."}};
	TestOptions[tests,{opts}];
	
	simpFnNest=OptionValue["ActWithNested"];
	simpFn=If[simpFnNest===Identity,OptionValue["ActWith"],simpFnNest];
	ContractIndices[t,"ActWith"->simpFn]
]


def@
TraceReverse[t_Tensor,{name_String,dispName_String},opts:OptionsPattern[]]:=
Module[{met,tTr,simpFn,simpFnNest,a,b,c,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];
	
	If[Rank[t]=!={0,2},
		Print["TraceReverse is built only for Tensors of Rank {0,2}"];
		AbortVerbose[]
	];

	{a,b}=Indices[t];
	c=SelectFirst[PossibleIndices[t],Not@MemberQ[({a,b}/.-n_:>n),#]&];
	MergeTensors[t[a,b] - 2 Metric[t][a,b] t[c,-c]/ManifoldDimensions[t],{name,dispName},opts]
];
reDef@TraceReverse[t_Tensor,name_String,opts:OptionsPattern[]]:=TraceReverse[t,{name,name},opts];
reDef@TraceReverse[t_Tensor,opts:OptionsPattern[]]:=TraceReverse[t,{TensorName[t]<>"TraceReverse","\!\(\*OverscriptBox[\("<>TensorDisplayName[t]<>"\), \(_\)]\)"},opts];


def@
ReorderTensorIndices[t_Tensor,inds_List,{name_String,displayName_String}]:=
Module[{is,pis},

	If[MultipleMetricsQ[t],
		Print["Can only reorder indices of tensors with a single metric."];
		AbortVerbose[]
	];
	
	is=Indices[t];

	If[Sort@inds=!=Range[Total@Rank@t],
		Print["Tensor ", t, " is of rank ", Total@Rank@t  ". ReorderTensorIndices requires a list of index positions including exactly the numbers ",Range[Total@Rank@t], " in some order." ];
		AbortVerbose[]
	];

	ToTensor[KeySort@Join[KeyDrop[Association@@t,{"DisplayName","Name","Metric","MetricQ","Indices"}],
			Association["Metric"->Metric[t,"PerIndex"->True],
						"MetricQ"->False,
						"Values"->Transpose[RawTensorValues[t],inds],
						"DisplayName"->displayName,
						"Name"->name,
						"Indices"->(is[[#]]&/@inds)]]]
];
reDef@ReorderTensorIndices[t_Tensor,inds_List,name_String]:=ReorderTensorIndices[t,inds,{name,name}];
reDef@ReorderTensorIndices[t_Tensor,inds_List]:=ReorderTensorIndices[t,inds,{TensorName[t]<>"Reorder"<>ToString[inds],TensorDisplayName[t]}];


(*def@
SymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},{name_String,displayName_String},opts:OptionsPattern[]]:=
Module[{ips,inds,inds2,indsBefore,indsBetween,indsAfter,indsA,indsB,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];
	
	If[pos1>pos2,
	Print["Indices must be given to SymmetrizeTensor in ascending order. Given as ",{pos1, pos2} ];
		AbortVerbose[]
	];
	If[pos1>Total@Rank@t||pos2>Total@Rank@t,
	Print["Tensor ", t, " is of rank ", Total@Rank@t  ". Cannot symmetrize on indices of positions ",{pos1, pos2} ];
		AbortVerbose[]
	];
	If[pos1==pos2,
	Print["Cannot symmetrize on indices of the same position: ",pos1 ];
		AbortVerbose[]
	];
	ips=IndexPositions[t];
	If[ips[[pos1]]=!=ips[[pos2]],
		Print["Symmetrize indices must be both contravariant or covariant"];
		AbortVerbose[]
	];

	indsBefore=Range[1,pos1-1];
	indsBetween=Range[pos1+1,pos2-1];
	indsAfter=Range[pos2+1,Total@Rank@t];
	
	inds =Indices[t];
	indsA=Part[inds,#]&/@Flatten[{indsBefore,pos1,indsBetween,pos2,indsAfter }];
	indsB=Part[inds,#]&/@Flatten[{indsBefore,pos2,indsBetween,pos1,indsAfter }];
	
	MergeTensors[1/2 (t@@indsA+t@@indsB),{name,displayName},opts]
];
reDef@SymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},name_String,opts:OptionsPattern[]]:=SymmetrizeTensor[t,{pos1,pos2},{name,name},opts];
reDef@SymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},opts:OptionsPattern[]]:=
SymmetrizeTensor[t,{pos1,pos2},{TensorName[t]<>"Symmetric"<>ToString[{pos1,pos2}],TensorDisplayName[t]<>"("<>ToString[pos1]<>","<>ToString[pos2]<>")"},opts];*)


(*def@
AntisymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},{name_String,displayName_String},opts:OptionsPattern[]]:=
Module[{ips,inds,inds2,indsBefore,indsBetween,indsAfter,indsA,indsB,tests},

	tests = {"ActWith" ->{MatchQ[#,_]&,"OptionValue of ActWith can be any function."},
			"ActWithNested" ->{MatchQ[#,_]&,"OptionValue of ActWithNested can be any function."}};
	TestOptions[tests,{opts}];
	
	If[pos1>pos2,
	Print["Indices must be given to AntisymmetrizeTensor in ascending order. Given as ",{pos1, pos2} ];
		AbortVerbose[]
	];
	If[pos1>Total@Rank@t||pos2>Total@Rank@t,
	Print["Tensor ", t, " is of rank ", Total@Rank@t  ". Cannot symmetrize on indices of positions ",{pos1, pos2} ];
		AbortVerbose[]
	];
	If[pos1==pos2,
	Print["Cannot antisymmetrize on indices of the same position: ",pos1 ];
		AbortVerbose[]
	];
	ips=IndexPositions[t];
	If[ips[[pos1]]=!=ips[[pos2]],
		Print["Antisymmetrize indices must be both contravariant or covariant"];
		AbortVerbose[]
	];

	indsBefore=Range[1,pos1-1];
	indsBetween=Range[pos1+1,pos2-1];
	indsAfter=Range[pos2+1,Total@Rank@t];
	
	inds =Indices[t];
	indsA=Part[inds,#]&/@Flatten[{indsBefore,pos1,indsBetween,pos2,indsAfter }];
	indsB=Part[inds,#]&/@Flatten[{indsBefore,pos2,indsBetween,pos1,indsAfter }];
	
	MergeTensors[1/2 (t@@indsA-t@@indsB),{name,displayName},opts]
];
reDef@AntisymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},name_String,opts:OptionsPattern[]]:=AntisymmetrizeTensor[t,{pos1,pos2},{name,name},opts];
reDef@AntisymmetrizeTensor[t_Tensor,{pos1_Integer,pos2_Integer},opts:OptionsPattern[]]:=
AntisymmetrizeTensor[t,{pos1,pos2},{TensorName[t]<>"Antisymmetric"<>ToString[{pos1,pos2}],TensorDisplayName[t]<>"["<>ToString[pos1]<>","<>ToString[pos2]<>"]"},opts];*)


End[];

EndPackage[];
