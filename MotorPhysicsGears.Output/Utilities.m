importMotorData[] := importMotorData["Characterized Motors (Gears).xlsx"]
 
importMotorData[file_] := Module[{}, 
     Import[StringJoin[NotebookDirectory[], file], {"Data", 1}][[2 ;; All]]]
 
motorParameters[motorName_, opts:OptionsPattern[]] := 
    motorParameters[motorData, motorName, opts]
 
motorParameters[motorData_, motorName_, opts:OptionsPattern[]] := 
    validateOptions[][Module[{row, paramValues, quantify, qty, assoc}, 
      row = Select[motorData, #1[[1]] == motorName & ][[1]]; 
       paramValues = (#1[[1]] -> toRational[row[[#1[[2]]]]] & ) /@ 
         {{R, 2}, {L, 4}, {KeShaft, 5}, {KtShaft, 6}, {Ke, 7}, {Kt, 8}, 
          {J, 9}, {B, 10}, {\[CapitalNu], 11}, {\[Eta], 
           If[OptionValue[Efficiency] == "Forward", 12, 13]}}; 
       quantify = Function[{name, value}, qty = name /. parameterQuantities; 
          name -> Quantity[value, QuantityUnit[qty]]]; 
       assoc = Association[(quantify @@ #1 & ) /@ paramValues]; 
       assoc[Jout] = Quantity[0, siAngularInertialUnits]; 
       assoc[Bout] = Quantity[0, siAngularViscousDragUnits]; 
       assoc[const\[Tau]auto] = Quantity[0, siTorqueUnits]; 
       If[ !OptionValue[Geared], assoc[\[CapitalNu]] = 1; assoc[\[Eta]] = 1; 
         assoc[Ke] = assoc[KeShaft]; assoc[Kt] = assoc[KtShaft]; ]; 
       assoc[KeShaft] =. ; assoc[KtShaft] =. ; simplifyUnits[siUnits[assoc]]]]
 
Options[motorParameters] = {Geared -> True, Efficiency -> "Forward"}
 
motorData = {{"Name", "R", "L (uH)", "L (H)", "Ke (>gear)", 
      "Kt (>gear, est)", "Ke (<gear)", "Kt (<gear, est)", "J", "b", "N", 
      "\[Eta]f (est)", "\[Eta]r (est)"}, {"AM 20 A", 2.3, 691., 0.000691, 
      0.351, 0.351, 0.01755, 0.01755, 9.010999999999999*^-6, 0.0022, 20., 
      0.9, 0.8}, {"AM 20 B", 1.9, 684., 0.000684, 0.389, 0.389, 
      0.019450000000000002, 0.019450000000000002, 9.010999999999999*^-6, 
      0.0025, 20., 0.9, 0.8}, {"AM 20 C", 5.1, 717., 0.000717, 0.385, 0.385, 
      0.01925, 0.01925, 8.930999999999998*^-6, 0.0028, 20., 0.9, 0.8}, 
     {"AM 40 A", 2.5, 674., 0.000674, 0.753, 0.753, 0.018825, 0.018825, 
      0.000022210000000000002, 0.2269, 40., 0.9, 0.8}, 
     {"AM 40 B", 3.8, 705., 0.000705, 0.705, 0.705, 0.017625, 0.017625, 
      0.00001741, 0.56, 40., 0.9, 0.8}, {"AM 40 C", 2.1, 716., 0.000716, 
      0.763, 0.763, 0.019075, 0.019075, 0.000024710000000000002, 0.018, 40., 
      0.9, 0.8}, {"AM 60 A", 3.3, 694., 0.000694, 1.066, 1.066, 
      0.017766666666666667, 0.017766666666666667, 0.00001041, 0.033, 60., 
      0.9, 0.8}, {"AM 60 B", 5.1, 696., 0.000696, 1.076, 1.076, 
      0.017933333333333336, 0.017933333333333336, 8.421*^-6, 0.02, 60., 0.9, 
      0.8}, {"AM 3.7 A", 8.9, 679., 0.000679, 0.099, 0.099, 
      0.026756756756756758, 0.026756756756756758, 0.000027910000000000002, 
      0.00014, 3.7, 0.9, 0.8}, {"AM 3.7 B", 2.6, 797., 0.000797, 0.108, 
      0.108, 0.029189189189189186, 0.029189189189189186, 0.00003151, 
      0.000176, 3.7, 0.9, 0.8}, {"AM 3.7 C", 8.7, 880., 0.00088, 0.105, 
      0.105, 0.028378378378378376, 0.028378378378378376, 
      0.00003091000000000001, 0.00017, 3.7, 0.9, 0.8}, 
     {"Matrix A", 3.8, 718., 0.000718, 0.34, 0.34, 0.00643939393939394, 
      0.00643939393939394, 9.430999999999998*^-6, 0.00151, 52.8, 0.9, 0.8}, 
     {"Matrix B", 7.8, 777., 0.000777, 0.363, 0.363, 0.006875, 0.006875, 
      7.760999999999999*^-6, 0.00191, 52.8, 0.9, 0.8}, 
     {"Matrix C", 20.6, 658., 0.000658, 0.338, 0.338, 0.006401515151515152, 
      0.006401515151515152, 7.231*^-6, 0.00186, 52.8, 0.9, 0.8}, 
     {"CoreHex A", 3.6, 1356., 0.001356, 0.822, 0.822, 0.022675862068965515, 
      0.022675862068965515, 0.0007331000000000001, 0.0112, 36.25, 0.9, 0.8}, 
     {"CoreHex B", 11.3, 1352., 0.001352, 0.858, 0.858, 0.02366896551724138, 
      0.02366896551724138, 0.0006551, 0.008, 36.25, 0.9, 0.8}, 
     {"CoreHex C", 5.6, 1342., 0.001342, 0.711, 0.711, 0.019613793103448275, 
      0.019613793103448275, 0.00045410000000000003, 0.0078, 36.25, 0.9, 0.8}}
 
validateOptions[] := Function[code, Module[{tag, msg, catch}, 
      msg = Function[{v, t}, Message[validateOptions::invldopt, 
           Sequence @@ t]; v]; catch = Function[c, Catch[c, _tag, msg], 
         HoldAll]; catch[Unevaluated[code] /. 
         o:HoldPattern[OptionValue[f_, _, name_]] :> With[{val = o}, 
           If[ !validateOption[f][name, val], Throw[$Failed, 
             tag[name, f, val]], val]]]], HoldAll]
 
validateOptions /: validateOptions::invldopt = 
     "Option `1` for function `2` received invalid value `3`"
 
validateOption[motorParameters][Geared, val_] := BooleanQ[val]
 
validateOption[motorParameters][Efficiency, val_] := 
    MemberQ[{"Forward", "Reverse"}, val]
 
toRational[double_Real] := Rationalize[double]
 
parameterQuantities = <|R -> Quantity[R, "Ohms"], 
     L -> Quantity[L, "Henries"], i[t] -> Quantity[i[t], "Amperes"], 
     Derivative[1][i][t] -> Quantity[Derivative[1][i][t], 
       "Amperes"/"Seconds"], Derivative[2][i][t] -> 
      Quantity[Derivative[2][i][t], "Amperes"/"Seconds"^2], 
     vbat[t] -> Quantity[vbat[t], "Volts"], constvbat -> 
      Quantity[constvbat, "Volts"], vg[t] -> Quantity[vg[t], "Volts"], 
     J -> Quantity[J, ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2], 
     Jout -> Quantity[Jout, ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2], 
     B -> Quantity[B, ("Meters"*"Newtons"*"Seconds")/"Radians"^2], 
     Bout -> Quantity[Bout, ("Meters"*"Newtons"*"Seconds")/"Radians"^2], 
     Ke -> Quantity[Ke, ("Seconds"*"Volts")/"Radians"], 
     KeShaft -> Quantity[KeShaft, ("Seconds"*"Volts")/"Radians"], 
     Kt -> Quantity[Kt, ("Meters"*"Newtons")/("Amperes"*"Radians")], 
     KtShaft -> Quantity[KtShaft, ("Meters"*"Newtons")/
        ("Amperes"*"Radians")], \[Theta][t] -> Quantity[\[Theta][t], 
       "Radians"], \[Theta]out[t] -> Quantity[\[Theta]out[t], "Radians"], 
     Derivative[1][\[Theta]][t] -> Quantity[Derivative[1][\[Theta]][t], 
       "Radians"/"Seconds"], Derivative[1][\[Theta]out][t] -> 
      Quantity[Derivative[1][\[Theta]out][t], "Radians"], 
     Derivative[2][\[Theta]][t] -> Quantity[Derivative[2][\[Theta]][t], 
       "Radians"/"Seconds"^2], Derivative[2][\[Theta]out][t] -> 
      Quantity[Derivative[2][\[Theta]out][t], "Radians"], 
     \[Omega][t] -> Quantity[\[Omega][t], "Radians"/"Seconds"], 
     \[Alpha][t] -> Quantity[\[Alpha][t], "Radians"/"Seconds"^2], 
     \[Tau][t] -> Quantity[\[Tau][t], ("Meters"*"Newtons")/"Radians"], 
     \[Tau]auto[t] -> Quantity[\[Tau]auto[t], ("Meters"*"Newtons")/
        "Radians"], const\[Tau]auto -> Quantity[const\[Tau]auto, 
       ("Meters"*"Newtons")/"Radians"], \[CapitalNu] -> 
      Quantity[\[CapitalNu], "DimensionlessUnit"], 
     \[Eta] -> Quantity[\[Eta], "DimensionlessUnit"]|>
 
Attributes[Derivative] = {NHoldAll, ReadProtected}
 
siAngularInertialUnits = ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2
 
siAngularViscousDragUnits = ("Meters"*"Newtons"*"Seconds")/"Radians"^2
 
siTorqueUnits = ("Meters"*"Newtons")/"Radians"
 
simplifyUnits[expr_] := mapQuantities[expr, UnitSimplify]
 
mapQuantities[expr_, f_] := expr /. {Quantity[val_, unit_] :> 
       f[Quantity[val, unit]], assoc_Association :> 
       Association[mapQuantities[Normal[assoc], f]]}
 
siUnits[expr_] := mapQuantities[expr, UnitConvert]
 
motorLoad[opts:OptionsPattern[]] := Module[{assoc = Association[]}, 
     assoc[Jout] = OptionValue[J] + OptionValue[Jout]; 
      assoc[Bout] = OptionValue[B] + OptionValue[Bout]; 
      assoc[const\[Tau]auto] = OptionValue[const\[Tau]auto]; assoc]
 
Options[motorLoad] = {J -> Quantity[0, ("Meters"*"Newtons"*"Seconds"^2)/
        "Radians"^2], Jout -> Quantity[0, ("Meters"*"Newtons"*"Seconds"^2)/
        "Radians"^2], B -> Quantity[0, ("Meters"*"Newtons"*"Seconds")/
        "Radians"^2], Bout -> Quantity[0, ("Meters"*"Newtons"*"Seconds")/
        "Radians"^2], const\[Tau]auto -> Quantity[0, ("Meters"*"Newtons")/
        "Radians"]}
 
flywheel[mass_, radius_] := motorLoad[
     J -> mass*(radius^2/2/Quantity[1, "Radians^2"])]
 
addMotorLoad[motor_, load_] := Module[{assoc}, assoc = Association[motor]; 
      assoc[Jout] = (Jout /. motor) + (Jout /. load); 
      assoc[Bout] = (Bout /. motor) + (Bout /. load); 
      assoc[const\[Tau]auto] = (const\[Tau]auto /. motor) + 
        (const\[Tau]auto /. load); assoc]
 
massOnPulley[mass_, radius_] := Module[{gravityAcceleration, gravityForce, 
      angularAcceleration, tangentialAcceleration, tangentialForce, tension, 
      torque, unit, parts, radians = Quantity["Radians"]}, 
     gravityAcceleration = Quantity["StandardAccelerationOfGravity"]; 
      gravityForce = gravityAcceleration*mass; angularAcceleration = 
       Quantity[Derivative[2][\[Theta]][t], siAngularAccelerationUnits]; 
      tangentialAcceleration = angularAcceleration*(radius/radians); 
      tangentialForce = tangentialAcceleration*mass; 
      tension = gravityForce + tangentialForce; 
      torque = tension*(radius/radians); unit = QuantityUnit[torque]; 
      parts = List @@ Apart[QuantityMagnitude[torque]]; 
      motorLoad[const\[Tau]auto -> Quantity[parts[[1]], unit], 
       Jout -> Quantity[parts[[2]], unit]/angularAcceleration]]
 
siAngularAccelerationUnits = "Radians"/"Seconds"^2
