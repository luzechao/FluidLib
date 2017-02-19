(* ::Package:: *)

BeginPackage["FluidLib`"];


(* Export section *)

(* Every function that the package provides will have a usage message here. *)
(* There will be no function definitions in this section, only usage messages. *)
(* Public functions will have names starting with capitals, by convention. *)

(* The Following function is for look-up table for Ideal Gas (Table A-23 p. 965 in textbook)*)
IdealGasProp::usage = "IdealGasProp[Name, T, PropertyName]
Ideal Gass Property on Molar Bases: T[K], h/u [kJ/kmol], s^o [kJ/kmol-K]";

Begin["`Private`"]; (* note ` character both before and after Private *)

idealgas = Import[FileNameJoin[{NotebookDirectory[], "data", "ideal_gas.csv"}]];
air = Import[FileNameJoin[{NotebookDirectory[], "data", "air.csv"}]];
(* ImplemeNeentation section *)

(* Function definitions will go into this section *)

IdealGasProp[Name_, T_, PropertyName_]:=Which[Name=="CO2",Which[PropertyName=="h",co2enthalpy[T],
PropertyName=="u",Interpolation[idealgas[[All,{1,3}]]][T],
PropertyName=="s",Interpolation[idealgas[[All,{1,4}]]][T]
],
Name=="CO",
Which[PropertyName=="h",Interpolation[idealgas[[All,{1,5}]]][T],
PropertyName=="u",Interpolation[idealgas[[All,{1,6}]]][T],
PropertyName=="s",Interpolation[idealgas[[All,{1,7}]]][T]
],
Name=="H2O",
Which[PropertyName=="h",Interpolation[idealgas[[All,{1,8}]]][T],
PropertyName=="u",Interpolation[idealgas[[All,{1,9}]]][T],
PropertyName=="s",Interpolation[idealgas[[All,{1,10}]]][T]
],
Name=="O2",
Which[PropertyName=="h",Interpolation[idealgas[[All,{1,11}]]][T],
PropertyName=="u",Interpolation[idealgas[[All,{1,12}]]][T],
PropertyName=="s",Interpolation[idealgas[[All,{1,13}]]][T]
],
Name=="N2",
Which[PropertyName=="h",Interpolation[idealgas[[All,{1,14}]]][T],
PropertyName=="u",Interpolation[idealgas[[All,{1,15}]]][T],
PropertyName=="s",Interpolation[idealgas[[All,{1,16}]]][T]
],
Name=="Air",
Which[PropertyName=="h",Interpolation[air[[All,{1,2}]]][T],
PropertyName=="u",Interpolation[air[[All,{1,3}]]][T],
PropertyName=="s",Interpolation[air[[All,{1,4}]]][T],
PropertyName=="pr",Interpolation[air[[All,{1,5}]]][T],
PropertyName=="vr",Interpolation[air[[All,{1,6}]]][T]
]
]
(*If[Name\[Equal]"CO2", If[PropertyName\[Equal]"Enthalpy",co2enthalpy[T],Print["Function is not implemented"]],Print["Function is not implemented"]]*)


(* All functions which are not public, and are only used in the 
   internal implementation of the package, go into this section.
   These have non-capital names by convention. *)

(*Enthalpy on molar basis *)
co2enthalpy := Interpolation[idealgas[[All,{1,2}]]]


End[]; (* `Private` *)
EndPackage[]; (* MyPack` *)
