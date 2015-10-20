(* ::Package:: *)

BeginPackage["Tensors`"];


Tensor;
Coordinates;
Metric;
Rank;
AbstractQ;
Indices;
PossibleIndices;
Name;
DisplayName;
IndexPositions;
RepeatedIndexQ;
MetricQ;
TensorValues;
ToTensor;
ToMetric;
InverseMetric;
ChristoffelSymbol;
RiemannTensor;
RicciTensor;
RicciScalar;
ContractIndices;
ShiftIndices;
ValidateIndices;


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
TensorValues[t_Tensor]:=TensorValues[Name[t],IndexPositions[t]]


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
	Tensor@@(Normal@KeyDrop[assoc,"Values"]/.("PossibleIndices"->_):>("PossibleIndices"->indexChoices))
]


Options[ToTensor]={"Coordinates"->Undefined,"DisplayName"->Undefined,"Metric"->Undefined,"PossibleIndices"->{},"Abstract"->True,"Values"->Undefined,"Dimensions"->Undefined};
ToTensor[name_String,{inds___},opts:OptionsPattern[]]:=
Module[{dispName,coords,vals,posInds,abstr,metric,dims},
	dispName=If[OptionValue["DisplayName"]=!=Undefined,OptionValue["DisplayName"],name];
	coords=OptionValue["Coordinates"];
	vals=OptionValue["Values"];
	posInds=OptionValue["PossibleIndices"];
	abstr=OptionValue["Abstract"];
	metric=OptionValue["Metric"];
	dims=OptionValue["Dimensions"];
	ToTensor[Association["Coordinates"->coords,"Metric"->metric,"Name"->name,"DisplayName"->dispName,"Indices"->{inds},"PossibleIndices"->posInds,"Abstract"->abstr,"Values"->vals,"Dimensions"->dims]]
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
	posInds=Complement[
			Switch[assoc["PossibleIndices"],
				"Latin",
				Symbol/@CharacterRange["a","z"],
				"CapitalLatin",
				Symbol/@Complement[CharacterRange["A","Z"],{"D","E","I","N"}],
				"Greek",
				Symbol/@Complement[CharacterRange["\[Alpha]","\[Omega]"],{"\[Pi]"}],
				___,
				assoc["PossibleIndices"]
	
			],Union[If[assoc["Coordinates"]=!=Undefined,assoc["Coordinates"],##&[]],Cases[assoc["Values"],_Symbol,Infinity]]];
	inds=If[assoc["Indices"]===Undefined,-Take[posInds,2],assoc["Indices"]];

	If[Not@MatchQ[inds,{-_Symbol,-_Symbol}]||(inds[[1]]===inds[[2]]),Print["Metric indices must be a pair of distinct covariant symbols"];Abort[]];
	If[assoc["Values"]=!=Undefined&&(Not@MatchQ[assoc["Values"],{Repeated[{__}]}]||Dimensions[assoc["Values"]]=!={Length@assoc["Coordinates"],Length@assoc["Coordinates"]}),
		Print["To be consistent with given coordinates, metric values must be given as a ",Length@assoc["Coordinates"], " \[Times] ", Length@assoc["Coordinates"], " matrix."];
		Abort[]
	];

	dims=If[assoc["Coordinates"]=!=Undefined,Length@assoc["Coordinates"],assoc["Coordinates"]];
	ToTensor[Join[KeyDrop[assoc,{"PossibleIndices","Indices"}],Association["Metric"->"Self","Dimensions"->dims,"PossibleIndices"->posInds,"Indices"->inds]]]
]


Options[ToMetric]={"Coordinates"->Undefined,"DisplayName"->Undefined,"Indices"->Undefined,"PossibleIndices"->"Greek","Abstract"->True,"Values"->Undefined};
ToMetric[name_String,opts:OptionsPattern[]]:=
Module[{dispName,coords,vals,inds,posInds,abstr},
	dispName=If[OptionValue["DisplayName"]=!=Undefined,OptionValue["DisplayName"],name];
	coords=OptionValue["Coordinates"];
	vals=OptionValue["Values"];
	inds=OptionValue["Indices"];
	posInds=OptionValue["PossibleIndices"];
	abstr=OptionValue["Abstract"];
	ToMetric[Association["Coordinates"->coords,"Name"->name,"DisplayName"->dispName,"Indices"->inds,"PossibleIndices"->posInds,"Abstract"->abstr,"Values"->vals]]
]


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
Tensor/:InverseMetric[t_Tensor]:=InverseMetric@Metric@t;
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
	If[Complement[indsUp,posInds]=!={},Print["The following indices are not included in the list of PossibleIndices for tensor ", t, ": ", Complement[indsUp,posInds]];Abort[]];
	If[Length[indsUp]=!=Total[Rank[t]],Print["The tensor ", t, " expects " ,Total[Rank[t]], " indices, but ", Length[indsUp]," indices were given."];Abort[]];
	If[Union@Join[Count[indsUp,#]&/@DeleteDuplicates[indsUp],{1,2}]=!=Sort@{1,2},
		Print["The following indices were repeated more than twice: ",If[Count[indsUp,#]>2,#,##&[]]&/@DeleteDuplicates[indsUp]];Abort[]];
	repeatedInds=Cases[{inds},#|-#]&/@(If[Count[indsUp,#]>1,#,##&[]]&/@DeleteDuplicates[indsUp]);
	If[If[#[[1]]=!=-#[[2]],#,##&[]]&/@repeatedInds=!={},
		Print["The following indices were given in the same position (both up or both down): ",If[#[[1]]=!=-#[[2]],#[[1]]/.-sym_:>sym,##&[]]&/@repeatedInds];Abort[]];
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

	ContractIndices[Fold[shiftIndex,t,Thread[{Range@Length[inds],inds}]]]
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


End[];

EndPackage[];
