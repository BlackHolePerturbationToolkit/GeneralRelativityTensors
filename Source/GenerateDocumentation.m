(* ::Package:: *)

<< Tensors`;
<< ApplicationTools`;

packages =
{ 
  "TensorDefinitions",
  "TensorDerivatives",
  "TensorManipulation",
  "CommonTensors"
};

packageSymbols = Map[# -> DocumentedSymbols["Tensors", #] &, packages];

appSymbols = "Tensors" -> DocumentedSymbols["Tensors"];
$PackageSymbols = Join[packageSymbols, {appSymbols}]; (* Used in the Overview.md file *)

undocumentedSymbols = Map[# -> UndocumentedSymbols["Tensors", #] &, packages] /. (_ -> {}) -> Sequence[]

Map[Print["Undocumented symbols for package "<>#[[1]]<>" skipped:\n", #[[2]]]&, undocumentedSymbols];

Print["Building symbol reference pages"];
docPackage[package_ -> symbols_] :=
  Map[(Print[#]; BuildSymbolReference["Tensors", #, "Source"]) &, symbols];
Scan[docPackage, packageSymbols];
docPackage[appSymbols];

Print["Building guides"];
sourceGuides = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Guides"}], Infinity];
destGuides =
  FileNameJoin[{Directory[], FileNameDrop[DirectoryName[#], 1],
      FileBaseName[#] <> ".nb"}] & /@ sourceGuides;
MapThread[BuildGuide, {sourceGuides, destGuides}];

Print["Building tutorials"];
tutorialSources = FileNames["*.md", FileNameJoin[{"Source", "Documentation", "English", "Tutorials"}], Infinity];
Map[(Print[#]; BuildTutorial[FileNameJoin[{Directory[], #}]])&, tutorialSources];

Print["Indexing Documentation"];
BuildIndex["Tensors"];

Print["Done"];