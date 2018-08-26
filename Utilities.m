(* Miscellanous utilities we use all over the place *)


(* mapQuantities[] replaces every occurence of a Quantity[] with its mapping under the provided function*)
mapQuantities[expr_, f_] := expr /. 
	{
 	Quantity[val_, unit_] :> f[Quantity[val, unit]],
	assoc_Association :> Association[mapQuantities[Normal[assoc], f]]
	}
 
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

prettyPrintFontSize = 15
prettyPrint[list_List] := Text[TraditionalForm[ColumnForm[list]], BaseStyle -> {FontSize -> prettyPrintFontSize}]
prettyPrint[list_And] := Text[TraditionalForm[ColumnForm[list]], BaseStyle -> {FontSize -> prettyPrintFontSize}]
prettyPrint[other_] := Text[TraditionalForm[other], BaseStyle -> {FontSize -> prettyPrintFontSize}]

(*-----------------------------------------------------------------------------------------------*)

line[{x1_, y1_}, {x2_, y2_}] := 
 Function[{x}, y1 + (y2 - y1) / (x2 - x1) * (x - x1)]
 
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

Clear[toRational]
toRational[double_Real] := Rationalize[double]
(*toRational[double_Real] := toRational[double, 0]
toRational[double_Real, n_] := 
 If[Floor[double * 10^n] / 10^n == double,  
  Floor[double * 10^n] / 10^n, toRational[double, n + 1]];*)
  

(*-----------------------------------------------------------------------------------------------*)

deleteFile[file_] := Block[{}, Quiet[DeleteFile[file]]; Null]

Clear[saveDefinitions, appendDefinitions]
SetAttributes[saveDefinitions, HoldRest]
SetAttributes[appendDefinitions, HoldRest]
saveDefinitions[file_, what_] := Block[{},
	deleteFile[file];
	Save[file, what];
]
appendDefinitions[file_, what_] := Save[file, what]


(*-----------------------------------------------------------------------------------------------*)

(* Returns a list of all the non-atomic subexpressions in the original expression. The list is sorted
   so that smaller subexpressions come first, producing a bottom-up search and replacement of the 
   common subexpressions. We also remove duplicates from the list.*)
   
listSubExprs[exp_] := Block[{reaped},
  reaped = Reap[Scan[If[! AtomQ[#], Sow[#]] &, exp, Infinity]];
  If[0 == Length[reaped[[2]]], 
   {},
   Sort[reaped [[2, 1]] // Union, LeafCount[#1] < LeafCount[#2] &]
   ]
  ]
   
(* The workhorse function. For each subexpression, if the rest of the subexpressions contain instances 
   of the subexpression, accumulate a rule replacing the subexpression with a variable name (symbol). 
   Note that for clarity, no effort is made to avoid symbols with existing values. The result is a 
   list of the original expression with variables substituted for the common parts, and a list of 
   the replacement rules. *)      

replaceRepeats[expr_] := replaceRepeats[expr, listSubExprs[expr]];
replaceRepeats[expr_, subExprsIn_] := 
  replaceRepeats[expr, subExprsIn, Count[#1, #2] > 0 &];
replaceRepeats[expr_, subExprsIn_, useExpr_] :=
  Extract[Reap[
    Module[{vnum = 0, modExpr = expr, subExprs = subExprsIn, subExpr, 
      mapping},
     While[subExprs != {},
      subExpr = First[subExprs];
      subExprs = Rest[subExprs];
      If[useExpr[subExprs, subExpr],
       mapping = 
        subExpr -> Unique["vv"];
       Sow[mapping];
       modExpr = modExpr /. mapping;
       subExprs = DeleteCases[subExprs, subExpr] /. mapping]];
     modExpr]], {{1}, {2, 1}}];
     

(*-----------------------------------------------------------------------------------------------*)
 
(* adapted from https://mathematica.stackexchange.com/questions/116623/how-to-check-the-validity-of-an-option-value *)

ClearAll[validateOptions];
validateOptions::invldopt = "Option `1` for function `2` received invalid value `3`";
validateOptions[]:=
  Function[code, 
    Module[{tag, msg, catch},
      msg = Function[{v, t},Message[validateOptions::invldopt, Sequence @@ t]; v];
      catch = Function[c, Catch[c, _tag, msg], HoldAll]; 
      catch @ ReplaceAll[
        Unevaluated @ code, 
        o:HoldPattern[OptionValue[f_,_,name_]]:> With[{val = o},
          If[!validateOption[f][name, val], 
            Throw[$Failed, tag[name, f, val]],
            (* else *)
            val
          ]
        ]
      ]
    ],
    HoldAll
];















    
