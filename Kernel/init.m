(* ::Package:: *)

Get["GeneralRelativityTensors`Kernel`GeneralRelativityTensors`"];


Block[{},
	Unprotect[$Packages];
	$Packages = Complement[$Packages, GeneralRelativityTensors`Private`packages];
	Protect[$Packages];

	Scan[Needs, GeneralRelativityTensors`Private`packages];
]
