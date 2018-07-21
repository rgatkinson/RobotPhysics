(* Miscellanous utilities we use all over the place *)


(* mapQuantities[] replaces every occurence of a Quantity[] with its mapping under the provided function*)
mapQuantities[expr_, f_] := 
 expr /.  Quantity[val_, unit_] :> f[Quantity[val, unit]]
 
(*-----------------------------------------------------------------------------------------------*) 
 
(* simplifyUnits[] runs UnitSimplify on all Quantities[]*) 
simplifyUnits[expr_] := mapQuantities[expr, UnitSimplify]

(* siUnits[] converts all Quantities to SI units *)
siUnits[expr_] := mapQuantities[expr, UnitConvert]

(* clearUnits[] replaces all Quantities with their (existing) magnitudes*)
clearUnits[expr_] := expr /. Quantity[val_, unit_] :> val

(* parseUnit[] canonicalizes unit strings into the internal strings that Mathematica perfers. 
 * It also does a sanity check that the unit is actually one that is known. *)
parseUnit[unit_String] := Quantity[1, unit] // QuantityUnit

(*-----------------------------------------------------------------------------------------------*) 

Options[plotLaplaceTransform] = Options[Plot];
plotLaplaceTransform[fn_, sRange_, opts : OptionsPattern[]] 	       := plotLaplaceTransform[fn, sRange, sRange, opts];
plotLaplaceTransform[fn_, reRange_, imRange_, opts : OptionsPattern[]] := plotLaplaceTransform[fn, {-reRange, reRange}, {-imRange, imRange},  opts]
plotLaplaceTransform[fn_, {rmin_, rmax_}, {imin_, imax_}, opts : OptionsPattern[]] := Module[
	{},
  	{
   	Plot3D[fn[x + I y] // Abs, {x, rmin, rmax}, {y, imin, imax}, 
		ImageSize -> Medium, MaxRecursion -> 8, 
	    AxesLabel -> {"Re", "Im"}, opts],
	    
	Plot3D[fn[x + I y] // Arg, {x, rmin, rmax}, {y, imin, imax}, 
	    ImageSize -> Medium, MaxRecursion -> 4, 
	    AxesLabel -> {"Re", "Im"}, 
	    Evaluate[FilterRules[{opts}, Except[PlotRange -> dontCare]]]]
	}
]

prettyPrint[list_List] := Text[TraditionalForm[ColumnForm[list]], BaseStyle -> {FontSize -> 15}]
prettyPrint[list_And] := Text[TraditionalForm[ColumnForm[list]], BaseStyle -> {FontSize -> 15}]

(*-----------------------------------------------------------------------------------------------*) 

unboundQ[x_Symbol] := True
unboundQ[_] := False
unboundQ[E] := False
unboundQ[I] := False
unboundQ[Pi] := False
unboundQ[\[Pi]] := False

variables[expr_] := variables[expr, {}]
variables[expr_, except_] := 
 Complement[
  Reap[Scan[(If[unboundQ[#], Sow[#]]) &, expr, Infinity]][[2, 1]] // 
   Union, except]

(*-----------------------------------------------------------------------------------------------*)

Options[uniqueSolve] = Options[Solve]
uniqueSolve[expr_, vars_, opts: OptionsPattern[]] := Module[{result},
    result = Solve[expr, vars, opts];
	If[Length[result] > 0, result[[1]], {}]    
]
uniqueSolve[expr_, vars_, dom_, opts: OptionsPattern[]] := Module[{result},
    result = Solve[expr, vars, dom, opts];
	If[Length[result] > 0, result[[1]], {}]    
] 

(*-----------------------------------------------------------------------------------------------*)

toRational[double_Real] := toRational[double, 0]
toRational[double_Real, n_] := 
 If[Floor[double * 10^n] / 10^n == double,  
  Floor[double * 10^n] / 10^n, toRational[double, n + 1]];
  

