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
(* ImplemeNeentation section *)

(* Function definitions will go into this section *)

IdealGasProp[Name_, T_, PropertyName_]:=Which[Name=="CO2",Which[PropertyName=="h",co2enthalpy[T]]]
(*If[Name\[Equal]"CO2", If[PropertyName\[Equal]"Enthalpy",co2enthalpy[T],Print["Function is not implemented"]],Print["Function is not implemented"]]*)


(* All functions which are not public, and are only used in the 
   internal implementation of the package, go into this section.
   These have non-capital names by convention. *)

(*Enthalpy on molar basis *)
co2enthalpy := Interpolation[idealgas[[All,{1,2}]]]


End[]; (* `Private` *)
EndPackage[]; (* MyPack` *)
