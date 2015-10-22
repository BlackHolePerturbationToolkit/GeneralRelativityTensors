(* ::Package:: *)

BeginPackage["Tensors`"];


Tensor::usage="Tensor is a Head created with the command ToTensor.";
ToTensor::usage="ToTensor[{n1,n2},{inds}] returns a tensor with Name n1, DisplayName n2 and indices {inds}.
ToTensor[n,{inds}] is equivalent to ToTensor[{n,n},{inds}].
ToTensor[{n1,n2},m,vals] returns a contravariant tensor with Name n1 and DisplayName n2. The (non-Abstract) metric m and values vals (given as a consistently sized List) are assigned.
ToTensor[n,m,vals] is equivalent to ToTensor[{n,n},m,vals]";
ToMetric::usage="ToMetric[{n1,n2}] returns an Abtract metric Tensor with Name n1 and DisplayName n2.
ToMetric[n] is equivalent to ToMetric[{n,n}]
ToMetric[{n1,n2},coords,vals,inds] returns an non-Abtract metric Tensor with Name n1, DisplayName n2, Coordinates coords, TensorValues vals, and PossibleIndices inds \
(where inds can be \"Greek\",\"Latin\",\"CaptialLatin\" or a list of Symbols).
ToMetric[{n1,n2},coords,vals] is equivalent to ToMetric[{n1,n2},coords,vals \"Greek\"].
ToMetric[n,coords,vals,inds] is equivalent to ToMetric[{n,n},coords,vals,inds].
ToMetric[n,coords,vals] is equivalent to ToMetric[n,coords,vals,\"Greek\"].
ToMetric[builtIn] returns a built-in metric Tensor, where builtIn can be \"Minkowski\", \"Schwarzschild\", or \"Kerr\".";
Coordinates::usage="Coordinates[t] returns a List of symbols used for the coordinates of the Tensor t, or Undefined if coordinates were not set.";
Metric::usage="Metric[t] returns the metric tensor associated with the Tensor t, or Undefined if no metric was set. If t is a metric, \"Self\" is returned.";
InverseMetric::usage="InverseMetric[t] returns the inverse metric tensor associated with the Tensor t, or Undefined if no metric was set.";
Rank::usage="Rank[t] returns the tensor rank of the Tensor t as a List {p,q}, where p is the number of contravariant indices and q the number of covariant indices.";
Indices::usage="Indices[t] returns a List of Symbols representing the indices of the Tensor t. Positive Symbols are contravariant and negative Symbols are covariant.";
PossibleIndices::usage="PossibleIndices[t] returns a List of all possible Symbols that can represent the indices of the Tensor t.";
Name::usage="Name[t] returns the Name of Tensor t which is used for storing cached values in the Symbol TensorValues.";
DisplayName::usage="DisplayName[t] returns the Name of Tensor t that is used for formatted output.";
IndexPositions::usage="IndexPositions[t] returns a List of elements \"Up\" and \"Down\" which represent (respectively) the contravariant and covariant positions of the indices of Tensor t.";
ChristoffelSymbol::usage="ChristoffelSymbol[m] returns the Christoffel symbol computed from the metric tensor m.";
RiemannTensor::usage="RiemannTensor[m] returns the Riemann tensor with indices {\"Up\",\"Down\",\"Down\",\"Down\"} computed from the metric tensor m.";
RicciTensor::usage="RicciTensor[m] returns the Ricci tensor with indices {\"Down\",\"Down\"} computed from the metric tensor m.";
RicciScalar::usage="RicciScalar[m] returns the Ricci scalar computed from the metric tensor m.";
ContractIndices::usage="ContractIndices[t,{n1,n2}] contracts all repeated indices of Tensor t, returning the resulting lower-rank tensor with Name n1 and DisplayName n2.
ContractIndices[t,n] is equivalent to ContractIndices[t,{n,n}].
ContractIndices[t] is equivalent to ContractIndices[t,{Name[t],DisplayName[t]}].";
ShiftIndices::usage="ShiftIndices[t,{inds}] raises and/or lowers the indices of Tensor t according to the given list inds, adjusting the values using the tensor's associated metric.";
ValidateIndices::usage="ValidateIndices[t,{inds}] checks that the list of indices {inds} is valid for Tensor t. An error is printed an operation is aborted if the list is not valid.";
TensorValues::usage="TensorValues[n,{inds}] returns the cached values of a Tensor with Name n and indices in positions {inds} or Undefined if none have been computed. The List {inds} should contain elements \"Up\" and/or \"Down\".
TensorValues[t] is equivalent to TensorValues[Name[t],IndexPositions[t]].";
RenameTensor::usage="RenameTensor[t,{n1,n2}] returns the Tensor t with its Name changed to n1 and DisplayName changed to n2.
RenameTensor[t,n] is equivalent to RenameTensor[t,{n,n}].";
MergeTensors::usage="MergeTensors[exprm{n1,n2}] calls MultiplyTensors, MultiplyTensorScalar, and SumTensors to merge the tensor expression expr  into one Tensor with Name n1 and DisplayName n2. Multiplication is assumed to \
be entered with NonCommutativeMultiply (**).
MergeTensors[expr,n] is equivalent to MergeTensors[t,{n,n}].
MergeTensors[expr] merges the tensor expression expr and forms a new Name and DisplayName from a combination of the tensors making up the expression.";
SumTensors::usage="SumTensors[t1,t2,..,{n1,n2}] sums the Tensors t1, t2, etc., forming a new Tensor with Name n1 and DisplayName n2.
SumTensors[t1,t2,..,n] is equivalent to SumTensors[t1,t2,..,{n,n}].
SumTensors[t1,t2,..] sums the Tensors t1, t2, etc., and forms a new Name and DisplayName from a combination of the Tensors making up the expression.";
MultiplyTensors::usage="MultiplyTensors[t1,t2,..,{n1,n2}] forms the outer product of the Tensors t1, t2, etc., creating a new Tensor with Name n1 and DisplayName n2.
MultiplyTensors[t1,t2,..,n] is equivalent to MultiplyTensors[t1,t2,..,{n,n}].
MultiplyTensors[t1,t2,..] forms the outer product of the Tensors t1, t2, etc., and forms a new Name and DisplayName from a combination of the Tensors making up the expression.";
MultiplyTensorScalar::usage="MultiplyTensorScalar[a, t, {n1,n2}] or MultiplyTensorScalar[t, a, {n1,n2}] forms the product of the scalar a with the Tensor t, creating a new Tensor with Name n1 and DisplayName n2.
MultiplyTensorScalar[a, t, n] is equivalent to MultiplyTensorScalar[a, t, {n,n}].
MultiplyTensorScalar[a, t] forms the product of the a and t, and forms a new Name and DisplayName from a combination of the scalar and Tensor making up the expression.";
RepeatedIndexQ::usage="RepeatedIndexQ[t] returns True if the Tensor t has repeated indices which can be traced.";
MetricQ::usage="MetricQ[t] returns True if the Tensor t is a metric.";
AbstractQ::usage="AbstractQ[t] returns True if the Tensor t is treated as Abstract.";
ClearCachedTensorValues::usage="ClearCachedTensorValues[n,inds] removes cached expressions stored with the Symbol TensorValues using the Tensor Name n and IndexPositions inds. Here inds is a List of \"Up\" and \"Down\".
ClearCachedTensorValues[t] removes all cached expressions stored with the Symbol TensorValues for the Tensor t.
ClearCachedTensorValues[All] removes all cached expressions associated with the Symbol TensorValues.";


Begin["`Private`"];


Tensor/:Format[t_Tensor]:=formatTensor[DisplayName@t,Indices@t]


Clear[formatTensor]
formatTensor[name_,inds_]:=
Module[{upStr,dnStr},
	If[inds==={},
		name,
		dnStr=StringJoin[If[MatchQ[#,-_Symbol],ToString[#/.-x_:>x],"  "]&/@inds];
		upStr=StringJoin[If[Not@MatchQ[#,-_Symbol],ToString[#],"  "]&/@inds];
		DisplayForm@SubsuperscriptBox[name,StringReplace[dnStr, StartOfString ~~Whitespace~~EndOfString:> ""],StringReplace[upStr, StartOfString ~~Whitespace~~EndOfString:> ""]]
	]
]


Tensor/:Coordinates[t_Tensor]:=(Association@@t)["Coordinates"]
Tensor/:Metric[t_Tensor]:=(Association@@t)["Metric"]
Tensor/:Rank[t_Tensor]:=Module[{inds,co},inds=Indices[t];co=Count[inds,-_Symbol];{Length[inds]-co,co}];
Tensor/:AbstractQ[t_Tensor]:=(Association@@t)["Abstract"]
Tensor/:Dimensions[t_Tensor]:=(Association@@t)["Dimensions"]
Tensor/:Indices[t_Tensor]:=(Association@@t)["Indices"]
Tensor/:PossibleIndices[t_Tensor]:=(Association@@t)["PossibleIndices"]
Tensor/:Name[t_Tensor]:=(Association@@t)["Name"]
Tensor/:DisplayName[t_Tensor]:=(Association@@t)["DisplayName"]
Tensor/:IndexPositions[t_Tensor]:=If[MatchQ[#,_Symbol],"Up","Down"]&/@Indices[t];
Tensor/:RepeatedIndexQ[t_Tensor]:=Length[DeleteDuplicates@(Indices[t]/.-sym_:>sym)]<Length[Indices[t]];
Tensor/:MetricQ[t_Tensor]:=Metric[t]==="Self"
Tensor/:t_Tensor[inds__]:=ShiftIndices[t,{inds}]


TensorValues[___]:=Undefined;
TensorValues[t_Tensor]:=If[#=!=Undefined,TensorValues[Name[t],IndexPositions[t]]=#,Undefined]&[(Association@@t)["Values"]];


Clear[ToTensor]
ToTensor[assoc_Association]:=
Module[{keys,nullKeys,listKeys,indexChoices},
	keys={"Metric","Coordinates","Name","DisplayName","Indices","Values","Abstract","Dimensions","PossibleIndices"};
	nullKeys={"Metric","Coordinates","Values","PossibleIndices","Dimensions"};
	listKeys={"Coordinates","PossibleIndices","Indices"};

	If[Sort@Keys[assoc]=!=Sort[keys],
		Print["The following keys are missing in the tensor formation: "<>ToString[Complement[keys,Keys[assoc]]]];
		Print["The following extra keys were in the tensor formation: "<>ToString[Complement[Keys[assoc],keys]]];
		Abort[]
	];
	If[Not@MatchQ[assoc["Indices"]/.-sym_:>sym,{___Symbol}],Print["Indices must be a list of Symbols (and negative Symbols)"];Abort[]];
	If[Not@MatchQ[assoc["PossibleIndices"],{___Symbol}],Print["PossibleIndices must be a list of Symbols"];Abort[]];

	indexChoices=Union@Join[assoc["PossibleIndices"],assoc["Indices"]/.-sym_:>sym];

	If[DeleteDuplicates[If[assoc[#]=!=Undefined,Head[assoc[#]],##&[]]&/@listKeys]=!={List},
		Print["The following Options were not given as lists: "<>ToString[If[assoc[#]=!=Undefined&&Head[assoc[#]]=!=List,#,##&[]]&/@listKeys]];
		Abort[]
	];

	If[Not[assoc["Abstract"]]&&MemberQ[assoc[#]&/@nullKeys,Undefined],
		Print["\"Abstract\"->False is inconsistent with Undefined values for "<>ToString[If[assoc[#]===Undefined,#,##&[]]&/@nullKeys]];
		Abort[]
	];
	
	If[(assoc["Coordinates"]=!=Undefined&&assoc["Dimensions"]=!=Undefined)&&Length@assoc["Coordinates"]=!=assoc["Dimensions"],
		Print["The number of coordinates given does not match the number of dimensions."];
		Abort[]
	];

	If[(MatchQ[assoc["Values"],_List]&&assoc["Values"]=!=Undefined&&assoc["Dimensions"]=!=Undefined)&&
							Dimensions[assoc["Values"]]=!=Table[assoc["Dimensions"],{Length[assoc["Indices"]]}],
		Print["Provided values are inconsistent with given tensor rank and number of dimensions."];
		Abort[]
	];

	If[Not@MatchQ[assoc["Values"],_List]&&assoc["Values"]=!=Undefined&&Length[assoc["Indices"]]=!=0,
		Print["Scalar quantity given with indices."];
		Abort[]
	];

	If[assoc["Metric"]=!=Undefined&&assoc["Metric"]=!="Self"&&Not@MetricQ[assoc["Metric"]],
		Print["Given Option \"Metric\" is not a metric tensor."];
		Abort[]
	];
	
	If[assoc["Coordinates"]=!=Undefined&&Intersection[assoc["Coordinates"],indexChoices]=!={},
		Print["The following elements appear as both indices and coordinates: "<>ToString[Intersection[assoc["Coordinates"],indexChoices]]];
		Abort[]
	];
		
	If[DeleteDuplicates[If[assoc[#]=!=Undefined,Head[assoc[#]],##&[]]&/@listKeys]=!={List},
		Print[DeleteDuplicates[If[assoc[#]=!=Undefined,Head[assoc[#]],##&[]]&/@listKeys]];
		Print["The following Options must be given as lists: "<>ToString[If[assoc[#]=!=Undefined&&Head[assoc[#]]=!=List,#,##&[]]&/@listKeys]];
		Abort[]
	];
			
	If[#=!=Undefined,TensorValues[assoc["Name"],If[MatchQ[#,_Symbol],"Up","Down"]&/@assoc["Indices"]]=#]&[assoc["Values"]];
	Tensor@@(Normal@assoc/.("PossibleIndices"->_):>("PossibleIndices"->indexChoices))
]


Options[ToTensor]={"Coordinates"->Undefined,"DisplayName"->Undefined,"Metric"->Undefined,"PossibleIndices"->{},"Abstract"->True,"Values"->Undefined,"Dimensions"->Undefined};
ToTensor[{name_String,dispName_String},{inds___},opts:OptionsPattern[]]:=
Module[{coords,vals,posInds,abstr,metric,dims},
	coords=OptionValue["Coordinates"];
	vals=OptionValue["Values"];
	posInds=OptionValue["PossibleIndices"];
	abstr=OptionValue["Abstract"];
	metric=OptionValue["Metric"];
	dims=OptionValue["Dimensions"];
	ToTensor[Association["Coordinates"->coords,"Metric"->metric,"Name"->name,"DisplayName"->dispName,"Indices"->{inds},"PossibleIndices"->posInds,"Abstract"->abstr,"Values"->vals,"Dimensions"->dims]]
]
ToTensor[name_String,{inds___},opts:OptionsPattern[]]:=ToTensor[{name,name},{inds},opts]


ToTensor[name_String,metric_Tensor?MetricQ,vals_List]:=ToTensor[{name,name},metric,vals];
ToTensor[{name_String,dispName_String},metric_Tensor?MetricQ,vals_List]:=
Module[{coords,posInds,dims,inds},

	If[AbstractQ[metric],Print["Tensor with values cannot be defined using \"Abstract\" metric."];Abort[]];

	coords=Coordinates[metric];	
	posInds=PossibleIndices[metric];
	dims=Dimensions[metric];
	inds=Take[posInds,Length@Dimensions[vals]];
	ToTensor[Association["Coordinates"->coords,"Metric"->metric,"Name"->name,"DisplayName"->dispName,"Indices"->inds,"PossibleIndices"->posInds,"Abstract"->False,"Values"->vals,"Dimensions"->dims]]
]


Clear[builtInIndices]
builtInIndices[label_]:=
Switch[label,
		"Latin",
		Symbol/@CharacterRange["a","z"],
		"CapitalLatin",
		Symbol/@Complement[CharacterRange["A","Z"],{"D","C","E","I","N"}],
		"Greek",
		Symbol/@Complement[CharacterRange["\[Alpha]","\[Omega]"],{"\[Pi]"}],
		___,
		Print["No built-in indices ", label]; Abort[]
]


Clear[ToMetric]
ToMetric[assoc_Association]:=
Module[{keys,dims,posInds,inds},
	
	keys={"Coordinates","Name","Indices","Values","Abstract","PossibleIndices","DisplayName"};

	If[Sort@Keys[assoc]=!=Sort[keys],
		Print["The following keys are missing in the metric tensor formation: "<>ToString[Complement[keys,Keys[assoc]]]];
		Print["The following extra keys were in the metric tensor formation: "<>ToString[Complement[Keys[assoc],keys]]];
		Abort[]
	];
	posInds=Complement[If[MemberQ[{"Greek","Latin","CapitalLatin"},assoc["PossibleIndices"]],builtInIndices[assoc["PossibleIndices"]],assoc["PossibleIndices"]],
						Union[If[assoc["Coordinates"]=!=Undefined,assoc["Coordinates"],##&[]],Cases[assoc["Values"],_Symbol,Infinity]]];
	inds=If[assoc["Indices"]===Undefined,-Take[posInds,2],assoc["Indices"]];

	If[Not@MatchQ[inds,{-_Symbol,-_Symbol}]||(inds[[1]]===inds[[2]]),Print["Metric indices must be a pair of distinct covariant symbols"];Abort[]];
	If[assoc["Values"]=!=Undefined&&(Not@MatchQ[assoc["Values"],{Repeated[{__}]}]||Dimensions[assoc["Values"]]=!={Length@assoc["Coordinates"],Length@assoc["Coordinates"]}),
		Print["To be consistent with given coordinates, metric values must be given as a ",Length@assoc["Coordinates"], " \[Times] ", Length@assoc["Coordinates"], " matrix."];
		Abort[]
	];

	dims=If[assoc["Coordinates"]=!=Undefined,Length@assoc["Coordinates"],assoc["Coordinates"]];
	ToTensor[Join[KeyDrop[assoc,{"PossibleIndices","Indices"}],Association["Metric"->"Self","Dimensions"->dims,"PossibleIndices"->posInds,"Indices"->inds]]]
]


ToMetric[{name_String,dispName_String},coords_,posIndsParam_:"Greek"]:=
Module[{inds,posInds},
	posInds=Complement[
					If[MemberQ[{"Greek","Latin","CapitalLatin"},posIndsParam],builtInIndices[posIndsParam],posIndsParam],
					If[coords=!=Undefined,coords,{}]
			];
	inds=-Take[posInds,2];
	ToMetric[Association["Coordinates"->coords,"Name"->name,"DisplayName"->dispName,"Indices"->inds,"PossibleIndices"->posInds,"Abstract"->True,"Values"->Undefined]]
]
ToMetric[name_String]:=ToMetric[{name,name}]


ToMetric[{name_String,dispName_String},coords_List,vals_List,posIndsParam_]:=
Module[{inds,posInds},

	posInds=Complement[
					If[MemberQ[{"Greek","Latin","CapitalLatin"},posIndsParam],builtInIndices[posIndsParam],posIndsParam],
					Union[coords,Cases[vals,_Symbol,Infinity]]
			];

	inds=-Take[posInds,2];
	
	ToMetric[
		Association["Coordinates"->coords,
					"Name"->name,
					"DisplayName"->dispName,
					"Indices"->inds,
					"PossibleIndices"->posInds,
					"Abstract"->False,
					"Values"->vals]
		]
]
ToMetric[{name_String,dispName_String},coords_,vals_]:=ToMetric[{name,dispName},coords,vals,"Greek"]
ToMetric[name_String,coords_,vals_,posIndsParam_]:=ToMetric[{name,name},coords,vals,posIndsParam]
ToMetric[name_String,coords_,vals_]:=ToMetric[{name,name},coords,vals,"Greek"]


ToMetric["Minkowski"]:=
Module[{t,x,y,z,\[Alpha],\[Beta]},	

	{t,x,y,z,\[Alpha],\[Beta]}=Symbol/@{"t","x","y","z","\[Alpha]","\[Beta]"};

	ToMetric["MinkowskiMetric",
				"Coordinates"->{t,x,y,z},
				"DisplayName"->"\[Eta]",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{-1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]
]


ToMetric["Schwarzschild"]:=
Module[{t,r,\[Theta],\[Phi],M,\[Alpha],\[Beta]},	

	{t,r,\[Theta],\[Phi],M,\[Alpha],\[Beta]}=Symbol/@{"t","r","\[Theta]","\[Phi]","M","\[Alpha]","\[Beta]"};
	
	ToMetric["SchwarzschildMetric",
				"Coordinates"->{t,r,\[Theta],\[Phi]},
				"DisplayName"->"g",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{-1+(2 M)/r,0,0,0},{0,1/(1-(2 M)/r),0,0},{0,0,r^2,0},{0,0,0,r^2 Sin[\[Theta]]^2}}]
]


ToMetric["Kerr"]:=
Module[{t,r,\[Theta],\[Phi],M,a,\[Alpha],\[Beta]},	

	{t,r,\[Theta],\[Phi],M,a,\[Alpha],\[Beta]}=Symbol/@{"t","r","\[Theta]","\[Phi]","M","a","\[Alpha]","\[Beta]"};

	ToMetric["KerrMetric",
				"Coordinates"->{t,r,\[Theta],\[Phi]},
				"DisplayName"->"g",
				"Indices"->{-\[Alpha],-\[Beta]},
				"PossibleIndices"->"Greek",
				"Abstract"->False,
				"Values"->{{(-a^2+2 M r-r^2+a^2 Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2),0,0,-((2 a M r Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2))},
							{0,(r^2+a^2 Cos[\[Theta]]^2)/(a^2-2 M r+r^2),0,0},
							{0,0,r^2+a^2 Cos[\[Theta]]^2,0},
							{-((2 a M r Sin[\[Theta]]^2)/(r^2+a^2 Cos[\[Theta]]^2)),0,0,(Sin[\[Theta]]^2 ((a^2+r^2)^2-a^2 (a^2-2 M r+r^2) Sin[\[Theta]]^2))/(r^2+a^2 Cos[\[Theta]]^2)}}]
]


Clear[InverseMetric]
Tensor/:InverseMetric[t_Tensor]:=If[Metric@t===Undefined,Undefined,InverseMetric@Metric@t];
Tensor/:InverseMetric[t_Tensor?MetricQ]:=InverseMetric[t]=
Module[{assoc,tvStored,tv,posUp},

	posUp={"Up","Up"};
	tvStored=TensorValues[Name[t],posUp];
	tv=If[tvStored===Undefined,
			If[TensorValues[t]===Undefined,
				Undefined,
				TensorValues[Name[t],posUp]=Simplify@Inverse[TensorValues[t]]
			],
			tvStored
		];
	
	assoc=Association@@t;
	ToTensor[Join[KeyDrop[assoc,"Indices"],Association["Indices"->Indices[t]/.-sym_Symbol:>sym,"Values"->tv]]]
]


Tensor/:ChristoffelSymbol[gT_Tensor?MetricQ]:=
Module[{n,g,ig,xx,vals,posInds},
	xx=Coordinates[gT];
	posInds=PossibleIndices[gT];
	n=Dimensions[gT];
	g=TensorValues[gT];
	ig=TensorValues@InverseMetric[gT];
	vals=Simplify@Table[(1/2)Sum[ig[[i,s]](-D[g[[j,k]],xx[[s]]]+D[g[[j,s]],xx[[k]]]+D[g[[s,k]],xx[[j]]]),{s,1,n}],{i,1,n},{j,1,n},{k,1,n}];

	ToTensor[Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","Indices"}],
			Association["Metric"->gT,"Values"->vals,"DisplayName"->"\[CapitalGamma]","Name"->"Christoffel","Indices"->{posInds[[1]],-posInds[[2]],-posInds[[3]]}]]]
]


Tensor/:RiemannTensor[gT_Tensor?MetricQ]:=
Module[{n,g,ig,xx,chr,vals,posInds},
	xx=Coordinates[gT];
	posInds=PossibleIndices[gT];
	n=Dimensions[gT];
	g=TensorValues[gT];
	ig=TensorValues@InverseMetric[gT];
	chr=TensorValues@ChristoffelSymbol[gT];
	vals=Simplify@Table[D[chr[[i,k,m]],xx[[l]]]-D[chr[[i,k,l]],xx[[m]]]
			+Sum[chr[[i,s,l]]chr[[s,k,m]],{s,1,n}]
			-Sum[chr[[i,s,m]]chr[[s,k,l]],{s,1,n}],
							{i,1,n},{k,1,n},{l,1,n},{m,1,n}];
	ToTensor[Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","Indices"}],
		Association["Metric"->gT,"Values"->vals,"DisplayName"->"R","Name"->"RiemannTensor","Indices"->{posInds[[1]],-posInds[[2]],-posInds[[3]],-posInds[[4]]}]]]
]


Tensor/:RicciTensor[gT_Tensor?MetricQ]:=
Module[{rie,vals,n,xx,posInds},
	xx=Coordinates[gT];
	posInds=PossibleIndices[gT];
	n=Dimensions[gT];
	rie=TensorValues@RiemannTensor[gT];
	vals=Simplify@Table[Sum[rie[[s,i,s,j]],{s,1,n}],{i,1,n},{j,1,n}];
	
	ToTensor[Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","Indices"}],Association["Metric"->gT,"Values"->vals,"DisplayName"->"R","Name"->"RicciTensor","Indices"->{-posInds[[1]],-posInds[[2]]}]]]
]


Tensor/:RicciScalar[gT_Tensor?MetricQ]:=
Module[{ricc,ig,vals,n,xx,posInds},
	xx=Coordinates[gT];
	posInds=PossibleIndices[gT];
	n=Dimensions[gT];
	ricc=TensorValues@RicciTensor[gT];
	ig=TensorValues@InverseMetric[gT];
	vals=Simplify@Sum[ig[[s,i]] ricc[[s,i]],{s,1,n},{i,1,n}];
	ToTensor[Join[KeyDrop[Association@@gT,{"DisplayName","Name","Metric","Indices"}],Association["Metric"->gT,"Values"->vals,"DisplayName"->"R","Name"->"RicciScalar","Indices"->{}]]]
]


Clear[ValidateIndices]
Tensor/:ValidateIndices[t_Tensor,{inds___}]:=
Module[{posInds,indsUp,repeatedInds},

	posInds=PossibleIndices[t];
	indsUp={inds}/.-sym_:>sym;
	repeatedInds=Cases[{inds},#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp]);

	If[Complement[indsUp,posInds]=!={},
		Print["The following indices are not included in the list of PossibleIndices for tensor ", t, ": ", Complement[indsUp,posInds]];
		Abort[]
	];
	If[Length[indsUp]=!=Total[Rank[t]],
		Print["The tensor ", t, " expects " ,Total[Rank[t]], " indices, but ", Length[indsUp]," indices were given."];
		Abort[]
	];
	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];
		Abort[]
	];

	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],#[[1]]/.-sym_:>sym,##&[]]&/@repeatedInds];
		Abort[]
	];
]


Clear[ShiftIndices]
Tensor/:ShiftIndices[t_Tensor,inds:{__}]:=
Module[{},
	ValidateIndices[t,inds];
	If[Length[inds]=!=Total[Rank[t]],Print["Tensor ", Name[t], " has ", Total[Rank[t]], If[Total[Rank[t]]===1," index"," indices"]];Abort[]];
	If[Not@MatchQ[inds/.-sym_:>sym,{__Symbol}],Print["Indices must be a list of Symbols (and negative Symbols)"];Abort[]];
	If[Complement[inds/.-sym_:>sym,PossibleIndices[t]]=!={},
		Print["The tensor ", Name[t]," does not use the following indices: ",Complement[inds/.-sym_:>sym,PossibleIndices[t]]];
		Abort[]
	];

	Fold[shiftIndex,t,Thread[{Range@Length[inds],inds}]]
]


Clear[shiftIndex]
shiftIndex[t_Tensor,{pos_Integer,ind_}]:=
Module[{gOrInvG,inds,indPos,indPosNew,tvs,indsBefore,indsAfter,n,itrBefore,itrAfter,vals,i,itrTot,itr,newPos},
	
	newPos=If[MatchQ[ind,_Symbol],"Up","Down"];
	indPos=IndexPositions[t];

	If[pos>Length@indPos,Print["Tensor ", t, " has only ",Length@indPos ," indices. Cannot raise at position ", pos,"."];Abort[]];
	indPosNew=ReplacePart[indPos,pos->newPos];
	inds=Indices[t];
	
	vals=
	If[indPos[[pos]]===newPos,
		TensorValues[t],
		
		If[TensorValues[Name[t],indPosNew]===Undefined,

			gOrInvG=If[newPos==="Up",TensorValues[InverseMetric[t]],If[MetricQ[t],TensorValues[Name[t],{"Down","Down"}],TensorValues[Metric[t]]]];
			tvs=TensorValues[t];
			n=Dimensions[t];
			indsBefore=Table[itr[ii],{ii,1,pos-1}];
			indsAfter=Table[itr[ii],{ii,pos+1,Length@indPos}];
			itrBefore=({#,1,n}&/@indsBefore);
			itrAfter=({#,1,n}&/@indsAfter);
			itrTot=Join[itrAfter,{{i,1,n}},itrBefore];
			Simplify@Table[Sum[gOrInvG[[i,s]]tvs[[Sequence@@indsBefore,s,Sequence@@indsAfter]],{s,1,n}],Evaluate[Sequence@@itrTot]],
			
			TensorValues[Name[t],indPosNew]
		]
	];
		
	ToTensor[Join[KeyDrop[Association@@t,"Indices"],Association["Values"->vals,"Indices"->Flatten@{Take[inds,pos-1],ind,Drop[inds,pos]}]]]
]


Clear[ContractIndices]
Tensor/:ContractIndices[t_Tensor]:=NestWhile[contractIndex,t,RepeatedIndexQ]
Tensor/:ContractIndices[t_Tensor,name_String]:=RenameTensor[ContractIndices[t],name]
Tensor/:ContractIndices[t_Tensor,{name_String,displayName_String}]:=RenameTensor[ContractIndices[t],{name,displayName}]


Clear[contractIndex]
contractIndex[t_Tensor]:=
Module[{indsUp,rptInd,rptIndsPos,indPos,indPosNew,inds,indsNew,tvsFull,n,vals,traceIndex,
	indsBefore,indsBetween,indsAfter,itrBefore,itrBetween,itrAfter,itrTot,tvs,itr},
	
	indPos=IndexPositions[t];
	inds=Indices[t];
	indsUp=inds/.-sym_:>sym;
	rptInd=First[If[Count[indsUp,#]===2,#,##&[]]&/@DeleteDuplicates@indsUp];
	rptIndsPos=Flatten@Position[indsUp,rptInd];

	indPosNew=Delete[indPos,{#}&/@Flatten@rptIndsPos];
	indsNew=Delete[inds,{#}&/@Flatten@rptIndsPos];

	vals=
		If[TensorValues[Name[t],indPosNew]===Undefined,

			tvs=TensorValues[t];
			n=Dimensions[t];
			indsBefore=Table[itr[ii],{ii,1,rptIndsPos[[1]]-1}];
			indsBetween=Table[itr[ii],{ii,rptIndsPos[[1]]+1,rptIndsPos[[2]]-1}];
			indsAfter=Table[itr[ii],{ii,rptIndsPos[[2]]+1,Length@indPos}];
			itrBefore=({#,1,n}&/@indsBefore);
			itrBetween=({#,1,n}&/@indsBetween);
			itrAfter=({#,1,n}&/@indsAfter);
			itrTot=Join[itrAfter,itrBetween,itrBefore];
			Simplify@If[itrTot==={},
						Sum[tvs[[Sequence@@indsBefore,s,Sequence@@indsBetween,s,Sequence@@indsAfter]],{s,1,n}],
						Table[Sum[tvs[[Sequence@@indsBefore,s,Sequence@@indsBetween,s,Sequence@@indsAfter]],{s,1,n}],Evaluate[Sequence@@itrTot]]
					],
						
			TensorValues[Name[t],indPosNew]
		];
	ToTensor[Join[KeyDrop[Association@@t,"Indices"],Association["Values"->vals,"Indices"->indsNew]]]
]


Tensor/:RenameTensor[t_Tensor,{name_String,displayName_String}]:=ToTensor[Join[KeyDrop[(Association@@t),{"Name","DisplayName"}],Association["Name"->name,"DisplayName"->displayName,"Values"->TensorValues[t]]]]
Tensor/:RenameTensor[t_Tensor,name_String]:=RenameTensor[t,{name,name}]


Clear[SumTensors]
Attributes[SumTensors]={Orderless};
Tensor/:SumTensors[t1_Tensor,t2_Tensor]:=
Module[{posInds,vals},
	If[AbstractQ[t1]||AbstractQ[t2],Print["Cannot sum Abstract Tensors."];Abort[]];
	If[Metric[t1]=!=Metric[t2]&&Not[Metric[t1]==="Self"&&t1===Metric[t2]]&&Not[Metric[t2]==="Self"&&t2===Metric[t1]],
		Print["Cannot sum Tensors with different metrics."];
		Abort[]
	];
	If[Indices[t1]=!=Indices[t2],Print["Cannot sum Tensors with different indices ",Indices[t1]," and ",Indices[t2]];Abort[]];
	posInds=Union[PossibleIndices[t1],PossibleIndices[t2]];
	vals=TensorValues[t1]+TensorValues[t2];
	
	ToTensor[{"("<>Name[t1]<>"+"<>Name[t2]<>")","("<>DisplayName[t1]<>"+"<>DisplayName[t2]<>")"},
			Indices[t1],
			"Values"->vals,
			"Metric"->Metric[t1],
			"Coordinates"->Coordinates[t1],
			"Abstract"->False,
			"PossibleIndices"->posInds,
			"Dimensions"->Dimensions[t1]]
]
Tensor/:SumTensors[t1_Tensor]:=t1;
Tensor/:SumTensors[t1_Tensor,t2__Tensor]:=Fold[SumTensors,t1,{t2}]
Tensor/:SumTensors[t1_Tensor,t2__Tensor,name_String]:=RenameTensor[SumTensors[t1,t2],name]
Tensor/:SumTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String}]:=RenameTensor[SumTensors[t1,t2],{name,displayName}]


Clear[MultiplyTensors]
Tensor/:MultiplyTensors[t1_Tensor,t2_Tensor]:=
Module[{posInds,vals,inds,indsUp,repeatedInds},
	If[AbstractQ[t1]||AbstractQ[t2],Print["Cannot multiply Abstract Tensors."];Abort[]];
	If[Metric[t1]=!=Metric[t2]&&Not[Metric[t1]==="Self"&&t1===Metric[t2]]&&Not[Metric[t2]==="Self"&&t2===Metric[t1]],
		Print["Cannot multiply Tensors with different metrics."];
		Abort[]
	];
	posInds=Union[PossibleIndices[t1],PossibleIndices[t2]];
	inds=Join[Indices[t1],Indices[t2]];
	indsUp=inds/.-sym_:>sym;
	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];Abort[]];
	repeatedInds=Cases[inds,#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp]);
	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],#[[1]]/.-sym_:>sym,##&[]]&/@repeatedInds];
		Abort[]
	];

	vals=Outer[Times,TensorValues[t1],TensorValues[t2]];

	ToTensor[{"("<>Name[t1]<>"\[CenterDot]"<>Name[t2]<>")","("<>DisplayName[t1]<>"\[CenterDot]"<>DisplayName[t2]<>")"},
			inds,
			"Values"->vals,
			"Metric"->Metric[t1],
			"Coordinates"->Coordinates[t1],
			"Abstract"->False,
			"PossibleIndices"->posInds,
			"Dimensions"->Dimensions[t1]]
]

Tensor/:MultiplyTensors[t1_Tensor]:=t1;
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor]:=Fold[MultiplyTensors,t1,{t2}]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,name_String]:=RenameTensor[MultiplyTensors[t1,t2],name]
Tensor/:MultiplyTensors[t1_Tensor,t2__Tensor,{name_String,displayName_String}]:=RenameTensor[MultiplyTensors[t1,t2],{name,displayName}]


Clear[MultiplyTensorScalar]
Tensor/:MultiplyTensorScalar[t_Tensor,n_]:=MultiplyTensorScalar[n,t];
Tensor/:MultiplyTensorScalar[n_,t_Tensor]:=
Module[{posInds,vals},
	If[AbstractQ[t],Print["Cannot multiply Abstract Tensors."];Abort[]];
	If[Not[MatchQ[n,(_Symbol|_Real|_Complex|_Integer|_Rational)]],Print["Cannot multiply a Tensor by a ", Head[n]];Abort[]];
	vals=n TensorValues[t];
	ToTensor["("<>ToString[n]<>Name[t]<>")",Metric[t],vals]
]
Tensor/:MultiplyTensorScalar[t1_Tensor]:=t1;
Tensor/:MultiplyTensorScalar[n_,t1_Tensor,name_String]:=RenameTensor[MultiplyTensorScalar[n,t1],name]
Tensor/:MultiplyTensorScalar[n_,t1_Tensor,{name_String,displayName_String}]:=RenameTensor[MultiplyTensorScalar[n,t1],{name,displayName}]


Clear[MergeTensors]
MergeTensors[expr_]:=
Module[{expr1,expr2},
	expr1=expr/.NonCommutativeMultiply[t__Tensor]:>MultiplyTensors[t];
	expr2=expr1/.n_ t_Tensor/;Not[MatchQ[n,_Tensor]]:>MultiplyTensorScalar[n,t];
	expr2/.Plus[t1_Tensor,t2__Tensor]:>SumTensors[t1,t2]
]
MergeTensors[expr_,name_]:=RenameTensor[MergeTensors[expr],name]


Tensor/:Times[t1_Tensor,t2__Tensor]:=(Print["To multiply Tensors use NonCommutativeMultiply, e.g.: t1**t2."];Abort[])


Clear[ClearCachedTensorValues]
ClearCachedTensorValues[s_String,inds_]:=If[TensorValues[s,inds]=!=Undefined,Unset[TensorValues[s,inds]]]
ClearCachedTensorValues[t_Tensor]:=Scan[ClearCachedTensorValues[Name[t],#]&,Tuples[{"Up","Down"},Total[Rank[t]]]]
ClearCachedTensorValues[All]:=Scan[ClearCachedTensorValues[Sequence@@#]&,DeleteDuplicates@Cases[DownValues[TensorValues]/.(a_:>b_):>a/.Verbatim[HoldPattern][Verbatim[TensorValues][x__]]:>{x},{_String,{__String}}]]


End[];

EndPackage[];
