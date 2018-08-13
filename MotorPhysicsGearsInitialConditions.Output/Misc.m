importMotorData[] := importMotorData["Characterized Motors (Gears)v2.xlsx"]
 
importMotorData[file_] := Module[{}, 
     Import[StringJoin[NotebookDirectory[], file], {"Data", 1}][[2 ;; All]]]
 
motorParameters[motorName_, opts:OptionsPattern[]] := 
    motorParameters[motorData, motorName, opts]
 
motorParameters[motorData_, motorName_, opts:OptionsPattern[]] := 
    validateOptions[][Module[{row, paramValues, quantify, qty, quantifiedRow, 
       result, gearbox}, row = Select[motorData, #1[[1]] == motorName & ][[
         1]]; paramValues = (#1[[1]] -> toRational[row[[#1[[2]]]]] & ) /@ 
         {{R, 2}, {L, 4}, {Ke, 5}, {Kt, 6}, {J, 7}, {B, 8}, 
          {\[CapitalNu], 9}, {\[Eta], If[OptionValue[Efficiency] == 
             "Forward", 10, 11]}}; quantify = Function[{name, value}, 
         qty = name /. parameterQuantities; name -> Quantity[value, 
            QuantityUnit[qty]]]; quantifiedRow = Association[
         (quantify @@ #1 & ) /@ paramValues]; result = Association[]; 
       result[R] = quantifiedRow[R]; result[L] = quantifiedRow[L]; 
       If[OptionValue[WithGearbox], result[\[CapitalNu]] = 
          quantifiedRow[\[CapitalNu]]; result[\[Eta]] = 
          quantifiedRow[\[Eta]]; , result[\[CapitalNu]] = 1; 
         result[\[Eta]] = 1; ]; gearbox = 
        {\[CapitalNu] -> quantifiedRow[\[CapitalNu]], 
         \[Eta] -> quantifiedRow[\[Eta]]}; result[Ke] = 
        reflectAngleCoefficient[quantifiedRow[Ke]] /. gearbox; 
       result[Kt] = reflectAngleCoefficient[quantifiedRow[Kt]] /. gearbox; 
       result[B] = reflectDrag[quantifiedRow[B]] /. gearbox; 
       result[J] = reflectInertia[quantifiedRow[J]] /. gearbox; 
       result[Jafter] = Quantity[0, siAngularInertialUnits]; 
       result[Bafter] = Quantity[0, siAngularViscousDragUnits]; 
       result[\[CapitalDelta]\[Tau]appConst] = Quantity[0, siTorqueUnits]; 
       simplifyUnits[siUnits[result]]]]
 
Options[motorParameters] = {WithGearbox -> True, Efficiency -> "Forward"}
 
motorData = {{"Name", "R", "L (uH)", "L (H)", "Ke", "Kt (est)", "J", "B", 
      "N", "\[Eta]f (est)", "\[Eta]r (est)"}, {"AM 20 A", 2.3, 691., 
      0.000691, 0.351, 0.351, 9.010999999999999*^-6, 0.0022, 20., 0.9, 0.8}, 
     {"AM 20 B", 1.9, 684., 0.000684, 0.389, 0.389, 9.010999999999999*^-6, 
      0.0025, 20., 0.9, 0.8}, {"AM 20 C", 5.1, 717., 0.000717, 0.385, 0.385, 
      8.930999999999998*^-6, 0.0028, 20., 0.9, 0.8}, 
     {"AM 40 A", 2.5, 674., 0.000674, 0.753, 0.753, 0.000022210000000000002, 
      0.2269, 40., 0.9, 0.8}, {"AM 40 B", 3.8, 705., 0.000705, 0.705, 0.705, 
      0.00001741, 0.56, 40., 0.9, 0.8}, {"AM 40 C", 2.1, 716., 0.000716, 
      0.763, 0.763, 0.000024710000000000002, 0.018, 40., 0.9, 0.8}, 
     {"AM 60 A", 3.3, 694., 0.000694, 1.066, 1.066, 0.00001041, 0.033, 60., 
      0.9, 0.8}, {"AM 60 B", 5.1, 696., 0.000696, 1.076, 1.076, 8.421*^-6, 
      0.02, 60., 0.9, 0.8}, {"", "", "", "", "", "", "", "", "", "", ""}, 
     {"AM 3.7 A", 8.9, 679., 0.000679, 0.099, 0.099, 0.000027910000000000002, 
      0.00014, 3.7, 0.9, 0.8}, {"AM 3.7 B", 2.6, 797., 0.000797, 0.108, 
      0.108, 0.00003151, 0.000176, 3.7, 0.9, 0.8}, {"AM 3.7 C", 8.7, 880., 
      0.00088, 0.105, 0.105, 0.00003091000000000001, 0.00017, 3.7, 0.9, 0.8}, 
     {"Matrix A", 3.8, 718., 0.000718, 0.34, 0.34, 9.430999999999998*^-6, 
      0.00151, 52.8, 0.9, 0.8}, {"Matrix B", 7.8, 777., 0.000777, 0.363, 
      0.363, 7.760999999999999*^-6, 0.00191, 52.8, 0.9, 0.8}, 
     {"Matrix C", 20.6, 658., 0.000658, 0.338, 0.338, 7.231*^-6, 0.00186, 
      52.8, 0.9, 0.8}, {"CoreHex A", 3.6, 1356., 0.001356, 0.822, 0.822, 
      0.0007331000000000001, 0.0112, 36.25, 0.9, 0.8}, 
     {"CoreHex B", 11.3, 1352., 0.001352, 0.858, 0.858, 0.0006551, 0.008, 
      36.25, 0.9, 0.8}, {"CoreHex C", 5.6, 1342., 0.001342, 0.711, 0.711, 
      0.00045410000000000003, 0.0078, 36.25, 0.9, 0.8}}
 
validateOptions[] := Function[code, Module[{tag, msg, catch}, 
      msg = Function[{v, t}, Message[validateOptions::invldopt, 
           Sequence @@ t]; v]; catch = Function[c, Catch[c, _tag, msg], 
         HoldAll]; catch[Unevaluated[code] /. 
         o:HoldPattern[OptionValue[f_, _, name_]] :> With[{val = o}, 
           If[ !validateOption[f][name, val], Throw[$Failed, 
             tag[name, f, val]], val]]]], HoldAll]
 
validateOptions /: validateOptions::invldopt = 
     "Option `1` for function `2` received invalid value `3`"
 
validateOption[motorParameters][WithGearbox, val_] := BooleanQ[val]
 
validateOption[motorParameters][Efficiency, val_] := 
    MemberQ[{"Forward", "Reverse"}, val]
 
toRational[double_Real] := Rationalize[double]
 
parameterQuantities = <|R -> Quantity[R, "Ohms"], 
     L -> Quantity[L, "Henries"], i[t] -> Quantity[i[t], "Amperes"], 
     Derivative[1][i][t] -> Quantity[Derivative[1][i][t], 
       "Amperes"/"Seconds"], Derivative[2][i][t] -> 
      Quantity[Derivative[2][i][t], "Amperes"/"Seconds"^2], 
     \[CapitalDelta]vapp[t] -> Quantity[\[CapitalDelta]vapp[t], "Volts"], 
     \[CapitalDelta]vappConst -> Quantity[\[CapitalDelta]vappConst, "Volts"], 
     vg[t] -> Quantity[vg[t], "Volts"], vapp0 -> Quantity[vapp0, "Volts"], 
     J -> Quantity[J, ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2], 
     Jafter -> Quantity[Jafter, ("Meters"*"Newtons"*"Seconds"^2)/
        "Radians"^2], B -> Quantity[B, ("Meters"*"Newtons"*"Seconds")/
        "Radians"^2], Bafter -> Quantity[Bafter, 
       ("Meters"*"Newtons"*"Seconds")/"Radians"^2], 
     Ke -> Quantity[Ke, ("Seconds"*"Volts")/"Radians"], 
     Kt -> Quantity[Kt, ("Meters"*"Newtons")/("Amperes"*"Radians")], 
     \[Theta][t] -> Quantity[\[Theta][t], "Radians"], 
     \[Theta]after[t] -> Quantity[\[Theta]after[t], "Radians"], 
     Derivative[1][\[Theta]][t] -> Quantity[Derivative[1][\[Theta]][t], 
       "Radians"/"Seconds"], Derivative[1][\[Theta]after][t] -> 
      Quantity[Derivative[1][\[Theta]after][t], "Radians"], 
     Derivative[2][\[Theta]][t] -> Quantity[Derivative[2][\[Theta]][t], 
       "Radians"/"Seconds"^2], Derivative[2][\[Theta]after][t] -> 
      Quantity[Derivative[2][\[Theta]after][t], "Radians"], 
     \[Omega][t] -> Quantity[\[Omega][t], "Radians"/"Seconds"], 
     \[Alpha][t] -> Quantity[\[Alpha][t], "Radians"/"Seconds"^2], 
     \[Tau][t] -> Quantity[\[Tau][t], ("Meters"*"Newtons")/"Radians"], 
     \[Tau]after[t] -> Quantity[\[Tau]after[t], ("Meters"*"Newtons")/
        "Radians"], \[CapitalDelta]\[Tau]app[t] -> 
      Quantity[\[CapitalDelta]\[Tau]app[t], ("Meters"*"Newtons")/"Radians"], 
     \[CapitalDelta]\[Tau]appConst -> Quantity[\[CapitalDelta]\[Tau]appConst, 
       ("Meters"*"Newtons")/"Radians"], \[Tau]app0 -> 
      Quantity[\[Tau]app0, ("Meters"*"Newtons")/"Radians"], 
     \[CapitalNu] -> Quantity[\[CapitalNu], "DimensionlessUnit"], 
     \[Eta] -> Quantity[\[Eta], "DimensionlessUnit"]|>
 
Attributes[Derivative] = {NHoldAll, ReadProtected}
 
reflectAngleCoefficient[coeff_] := coeff/\[CapitalNu]
 
reflectDrag[b_] := reflectTorque[reflectAngleCoefficient[b]]
 
reflectTorque[\[Tau]out_] := \[Tau]out/(\[CapitalNu]*\[Eta])
 
reflectInertia[j_] := reflectTorque[reflectAngleCoefficient[j]]
 
siAngularInertialUnits = ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2
 
siAngularViscousDragUnits = ("Meters"*"Newtons"*"Seconds")/"Radians"^2
 
siTorqueUnits = ("Meters"*"Newtons")/"Radians"
 
simplifyUnits[expr_] := mapQuantities[expr, UnitSimplify]
 
mapQuantities[expr_, f_] := expr /. {Quantity[val_, unit_] :> 
       f[Quantity[val, unit]], assoc_Association :> 
       Association[mapQuantities[Normal[assoc], f]]}
 
siUnits[expr_] := mapQuantities[expr, UnitConvert]
 
motorLoad[opts:OptionsPattern[]] := Module[{assoc = Association[]}, 
     assoc[Jafter] = OptionValue[J] + OptionValue[Jafter]; 
      assoc[Bafter] = OptionValue[B] + OptionValue[Bafter]; 
      assoc[\[CapitalDelta]\[Tau]appConst] = OptionValue[
        \[CapitalDelta]\[Tau]appConst]; assoc]
 
Options[motorLoad] = {J -> Quantity[0, ("Meters"*"Newtons"*"Seconds"^2)/
        "Radians"^2], Jafter -> Quantity[0, ("Meters"*"Newtons"*"Seconds"^2)/
        "Radians"^2], B -> Quantity[0, ("Meters"*"Newtons"*"Seconds")/
        "Radians"^2], Bafter -> Quantity[0, ("Meters"*"Newtons"*"Seconds")/
        "Radians"^2], \[CapitalDelta]\[Tau]appConst -> 
      Quantity[0, ("Meters"*"Newtons")/"Radians"]}
 
flywheel[mass_, radius_] := motorLoad[
     J -> mass*(radius^2/2/Quantity[1, "Radians^2"])]
 
addMotorLoad[motor_, load_] := Module[{assoc}, assoc = Association[motor]; 
      assoc[Jafter] = (Jafter /. motor) + (Jafter /. load); 
      assoc[Bafter] = (Bafter /. motor) + (Bafter /. load); 
      assoc[\[CapitalDelta]\[Tau]appConst] = 
       (\[CapitalDelta]\[Tau]appConst /. motor) + 
        (\[CapitalDelta]\[Tau]appConst /. load); assoc]
 
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
      motorLoad[\[CapitalDelta]\[Tau]appConst -> Quantity[parts[[1]], unit], 
       Jafter -> Quantity[parts[[2]], unit]/angularAcceleration]]
 
siAngularAccelerationUnits = "Radians"/"Seconds"^2
 
stepMotorTimeFunctionGeneric[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*
                  \[CapitalNu], R + L*s}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                   (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
              SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, 
                "\[Omega]after", Automatic}], {1 & , vappfn$[#1] & , 
              \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]], t] = 
    (-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (\[CapitalDelta]\[Tau]appConst*
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0)/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
stepMotorTimeFunctionGeneric[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*
                  \[CapitalNu], R + L*s}}, {{s*(Bafter*R + (Ke*Kt + B*R)*
                    \[Eta]*\[CapitalNu]^2), s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                      \[CapitalNu]^2)), s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                      \[CapitalNu]^2))}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Theta]after", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], t] = (Kt*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
       (t - (L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          (-((Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                    Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                        (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                        Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2])) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
             Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
              \[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
               (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                2]) - (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
             \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
             B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
           (Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) + 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
             Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
              \[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
               (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                2]) + (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
             \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
             B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
         Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
     \[CapitalDelta]\[Tau]appConst*
      ((R*t)/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         B*R*\[Eta]*\[CapitalNu]^2) - 
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                   2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                     J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                      \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                     (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
        (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
           \[CapitalNu]^2)*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])) + 
     (t*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
stepMotorTimeFunctionGeneric[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                 Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, {
                {Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Omega]", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], t] = (-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
stepMotorTimeFunctionGeneric[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                 Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, {
                {s*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
                 s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                     Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)), 
                 s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                     Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2))}}}, s, 
              SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Theta]", 
                Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
            aTime$][[1]]]; fn$]], t] = 
    (Kt*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
       (t - (L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          (-((Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                    Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                        (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                        Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2])) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
             Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
              \[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
               (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                2]) - (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
             \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
             B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
           (Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) + 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
             Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
              \[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
               (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                2]) + (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
             \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
             B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
         Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
     \[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
      ((R*t)/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         B*R*\[Eta]*\[CapitalNu]^2) - 
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                   2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                     J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                      \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                     (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
        (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
           \[CapitalNu]^2)*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])) + 
     (t*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
stepMotorTimeFunctionGeneric[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Ke*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + 
                   R*\[Tau]app0), Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*
                  \[CapitalNu]}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                   (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
              SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "vg", 
                Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
            aTime$][[1]]]; fn$]], t] = 
    (-2*Ke*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (Ke*\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Ke*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
stepMotorTimeFunctionGeneric[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
                    Ke*\[Tau]app0), Bafter + Jafter*s + (B + J*s)*\[Eta]*
                   \[CapitalNu]^2, -(Ke*\[CapitalNu])}}, {{Bafter*R + 
                  (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                     \[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "i", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], t] = (2*Ke*L*\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (\[CapitalDelta]vappConst*
       ((-2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                    (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                    Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
         Ke*\[Tau]app0))/(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
stepMotorTimeFunctionGeneric[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*(Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*
                      \[CapitalNu] - Ke*\[Tau]app0)), Kt*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), -(Ke*Kt*\[CapitalNu])}}, 
               {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Tau]", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], t] = (2*Ke*Kt*L*\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (Kt*\[CapitalDelta]vappConst*
       ((-2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                    (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                    Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Kt*(Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
          Ke*\[Tau]app0)))/(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
stepMotorTimeFunctionGeneric[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*\[Eta]*\[CapitalNu]*(Bafter*vapp0 + \[CapitalNu]*
                    (B*vapp0*\[Eta]*\[CapitalNu] - Ke*\[Tau]app0)), 
                 Kt*\[Eta]*\[CapitalNu]*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                    \[CapitalNu]^2), -(Ke*Kt*\[Eta]*\[CapitalNu]^2)}}, {
                {Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Tau]after", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], t] = (2*Ke*Kt*L*\[CapitalDelta]\[Tau]appConst*\[Eta]*
       \[CapitalNu]^2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (Kt*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
       ((-2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                    (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                    Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Kt*\[Eta]*\[CapitalNu]*(Bafter*vapp0 + \[CapitalNu]*
         (B*vapp0*\[Eta]*\[CapitalNu] - Ke*\[Tau]app0)))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
stepMotorTimeFunctionGeneric[motorTimeFunction_, t_] := 
    stepMotorTimeFunctionGeneric[motorTimeFunction, t] = 
     Module[{}, motorTimeFunction[\[CapitalDelta]vappConst & , 
        \[CapitalDelta]\[Tau]appConst & ][t]]
 
Attributes[vappfn$] = {Temporary}
 
Attributes[\[Tau]appfn$] = {Temporary}
 
Attributes[fn$] = {Temporary}
 
Attributes[aTime$] = {Temporary}
 
makeTimeDomainFunctionConvolve[model_, tInputFunctions_] := 
    Module[{s, \[Tau], modelExpr}, 
     modelExpr = InverseLaplaceTransform[model[s], s, \[Tau]][[1]]; 
      Function[{t}, Module[{convolutions}, 
        convolutions = (convolve[modelExpr[[#1]], tInputFunctions[[#1]][
              \[Tau]], \[Tau], t] & ) /@ Range[1, Length[tInputFunctions]]; 
         {Total[convolutions]}]]]
 
convolve[fExpr_, gExpr_, exprVar_, t_] := Module[{\[Tau]}, 
     Assuming[Union[{t >= 0}, parameterAssumptions], 
      Integrate[(fExpr /. exprVar -> \[Tau])*(gExpr /. 
         exprVar -> t - \[Tau]), {\[Tau], 0, t}]]]
 
parameterAssumptions = {Element[Bafter, Reals], Element[Jafter, Reals], 
     Element[Null, Reals], Element[vapp0, Reals], 
     Element[\[CapitalDelta]vappConst, Reals], 
     Element[\[CapitalDelta]\[Tau]appConst, Reals], 
     Element[\[Tau]app0, Reals], Element[i[_], Reals], Element[vg[_], Reals], 
     Element[\[Alpha][_], Reals], Element[\[CapitalDelta]vapp[_], Reals], 
     Element[\[CapitalDelta]\[Tau]app[_], Reals], Element[\[Theta][_], 
      Reals], Element[\[Tau][_], Reals], Element[\[Tau]after[_], Reals], 
     Element[\[Omega][_], Reals], Ke > 0, Kt > 0, L > 0, R > 0, \[Eta] > 0, 
     \[CapitalNu] > 0, B >= 0, J >= 0, t >= 0}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*
                  \[CapitalNu], R + L*s}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                   (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
              SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, 
                "\[Omega]after", Automatic}], {1 & , vappfn$[#1] & , 
              \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]], 
     <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/("Amperes"^2*
          "Seconds"^3)], L -> Quantity[347/500000, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^2)], \[CapitalNu] -> 60, \[Eta] -> 9/10, 
      Ke -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[0, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[12, "Volts"]|>, t] = 
    {(-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (\[CapitalDelta]\[Tau]appConst*
        ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0)/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     Quantity[-10.18846004659115, "Radians"/"Seconds"]/
       E^(6.865838702933615*t) + Quantity[0.014730410752213253, 
        "Radians"/"Seconds"]/E^(4748.837251579376*t) + 
      Quantity[10.173729635838937, "Radians"/"Seconds"], 
     10.173729635838937 + 0.014730410752213253/E^(4748.837251579376*t) - 
      10.18846004659115/E^(6.865838702933615*t)}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*
                  \[CapitalNu], R + L*s}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                   (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
              SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, 
                "\[Omega]after", Automatic}], {1 & , vappfn$[#1] & , 
              \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]], 
     <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/("Amperes"^2*
          "Seconds"^3)], L -> Quantity[347/500000, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^2)], \[CapitalNu] -> 60, \[Eta] -> 9/10, 
      Ke -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[12, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[-12, "Volts"]|>, t] = 
    {(-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (\[CapitalDelta]\[Tau]appConst*
        ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0)/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     Quantity[-0.014730410752213253, "Radians"/"Seconds"]/
       E^(4748.837251579376*t) + Quantity[7.859338180260522*^-14, 
       "Radians"/"Seconds"] + Quantity[10.18846004659115, 
        "Radians"/"Seconds"]/E^(6.865838702933615*t), 
     7.859338180260522*^-14 - 0.014730410752213253/E^(4748.837251579376*t) + 
      10.18846004659115/E^(6.865838702933615*t)}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*
                  \[CapitalNu], R + L*s}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                   (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
              SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, 
                "\[Omega]after", Automatic}], {1 & , vappfn$[#1] & , 
              \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]], 
     <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/("Amperes"^2*
          "Seconds"^3)], L -> Quantity[347/500000, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^2)], \[CapitalNu] -> 60, \[Eta] -> 9/10, 
      Ke -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[12, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[0, "Volts"]|>, t] = 
    {(-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (\[CapitalDelta]\[Tau]appConst*
        ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0)/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     (E^(-4748.837251579376*t) + E^(-6.865838702933615*t))*
       Quantity[0., "Radians"/"Seconds"] + Quantity[10.173729635839015, 
       "Radians"/"Seconds"], 10.173729635839015}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*
                  \[CapitalNu], R + L*s}}, {{s*(Bafter*R + (Ke*Kt + B*R)*
                    \[Eta]*\[CapitalNu]^2), s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                      \[CapitalNu]^2)), s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                      \[CapitalNu]^2))}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Theta]after", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^3)], L -> Quantity[347/500000, 
        ("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)], 
      \[CapitalNu] -> 60, \[Eta] -> 9/10, Ke -> Quantity[533/30000, 
        ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[0, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[12, "Volts"]|>, t] = 
    {(Kt*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
        (t - (L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
           (-((Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                      \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                     Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                         (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                         Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                  (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
              (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                  (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
                 (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                  2])) - ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                    Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                        (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                        Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) - 
            (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                      2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                        J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                         \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                        (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
              \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) - 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) - 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
              B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
              Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                  (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                  Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
            (Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                    Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                        (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                        Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) + 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
              Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^
                2 + Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                  (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                  Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
            (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                      2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                        J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                         \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                        (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
              \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) + 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) - 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
              B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
              Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                  (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                  Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
      \[CapitalDelta]\[Tau]appConst*
       ((R*t)/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          B*R*\[Eta]*\[CapitalNu]^2) - 
        ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                  Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                      (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                      Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*
                (Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
          (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
          (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                    2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                      J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                       \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                      (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*
                (Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*
            \[CapitalNu]^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                 \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                 \[CapitalNu]^2)^2]) - 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
             J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                 \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                 \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                  Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                      (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                      Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*
                (Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                    2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                      J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                       \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                      (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*
                (Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*
            \[CapitalNu]^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                 \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                 \[CapitalNu]^2)^2]) + 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
             J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                 \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                 \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
         (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2)*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
             (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
               \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
               \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])) + 
      (t*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     Quantity[-3.1018984167785534*^-6, "Radians"]/E^(4748.837251579376*t) + 
      Quantity[1.4839352462851072, "Radians"]/E^(6.865838702933615*t) + 
      Quantity[-1.4839321443866904 + 10.173729635839015*t, "Radians"], 
     -1.4839321443866904 - 3.1018984167785534*^-6/E^(4748.837251579376*t) + 
      1.4839352462851072/E^(6.865838702933615*t) + 10.173729635839015*t}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                 Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, {
                {Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Omega]", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], <|R -> 3.3, L -> 0.000694, \[CapitalNu] -> 1., \[Eta] -> 1., 
      Ke -> 0.017766666666666667, Kt -> 0.017766666666666667, 
      B -> 0.000010185185185185185, J -> 3.212962962962963*^-9, 
      Jafter -> 0.05, Bafter -> 0., \[CapitalDelta]\[Tau]appConst -> 0.|>, 
     <|t -> t, vapp0 -> 0., \[Tau]app0 -> 0., \[CapitalDelta]vappConst -> 
       12.|>, t] = {(-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
        ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     610.4237781837961 + 0.0002717373285849225/E^(4755.041314607737*t) - 
      610.4240499211247/E^(0.002116761658242995*t), 
     610.4237781837961 + 0.0002717373285849225/E^(4755.041314607737*t) - 
      610.4240499211247/E^(0.002116761658242995*t)}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                 Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, {
                {Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Omega]", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], <|R -> 3.3, L -> 0.000694, \[CapitalNu] -> 60., \[Eta] -> 0.9, 
      Ke -> 0.017766666666666667, Kt -> 0.017766666666666667, 
      B -> 0.000010185185185185185, J -> 3.212962962962963*^-9, 
      Jafter -> 0.05, Bafter -> 0., \[CapitalDelta]\[Tau]appConst -> 0.|>, 
     <|t -> t, vapp0 -> 0., \[Tau]app0 -> 0., \[CapitalDelta]vappConst -> 
       12.|>, t] = {(-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
        ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     610.4237781503716 + 0.883824645132795/E^(4748.837251579376*t) - 
      611.3076027955044/E^(6.865838702932955*t), 610.4237781503716 + 
      0.883824645132795/E^(4748.837251579376*t) - 611.3076027955044/
       E^(6.865838702932955*t)}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                 Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, {
                {Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Omega]", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^3)], L -> Quantity[347/500000, 
        ("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)], 
      \[CapitalNu] -> 60, \[Eta] -> 9/10, Ke -> Quantity[533/30000, 
        ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[0, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[12, "Volts"]|>, t] = 
    {(-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
        ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     Quantity[-611.307602795469, "Radians"/"Seconds"]/
       E^(6.865838702933615*t) + Quantity[0.8838246451327952, 
        "Radians"/"Seconds"]/E^(4748.837251579376*t) + 
      Quantity[610.4237781503363, "Radians"/"Seconds"], 
     610.4237781503363 + 0.8838246451327952/E^(4748.837251579376*t) - 
      611.307602795469/E^(6.865838702933615*t)}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                 Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, {
                {s*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
                 s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                     Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)), 
                 s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                     Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2))}}}, s, 
              SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Theta]", 
                Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
            aTime$][[1]]]; fn$]], 
     <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/("Amperes"^2*
          "Seconds"^3)], L -> Quantity[347/500000, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^2)], \[CapitalNu] -> 60, \[Eta] -> 9/10, 
      Ke -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[0, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[12, "Volts"]|>, t] = 
    {(Kt*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
        (t - (L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
           (-((Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                      \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                     Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                         (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                         Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                  (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
              (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                  (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
                 (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                  2])) - ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                    Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                        (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                        Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) - 
            (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                      2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                        J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                         \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                        (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
              \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) - 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) - 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
              B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
              Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                  (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                  Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
            (Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                    Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                        (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                        Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) + 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
              Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^
                2 + Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                  (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                  Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
            (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                      2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                        J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                         \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                        (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
              \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) + 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2]) - 
            ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                         \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                         \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                         \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                   J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
              B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
              Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                  (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                  Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
      \[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
       ((R*t)/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          B*R*\[Eta]*\[CapitalNu]^2) - 
        ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                  Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                      (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                      Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*
                (Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
          (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
          (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                    2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                      J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                       \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                      (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*
                (Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*
            \[CapitalNu]^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                 \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                 \[CapitalNu]^2)^2]) - 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
             J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                 \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                 \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                  Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                      (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                      Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*
                (Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                    2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                      J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                       \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                      (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*
                (Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*
            \[CapitalNu]^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                 \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                 \[CapitalNu]^2)^2]) + 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
            (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
          (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                  J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                       \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                       \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                       \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
             J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                 \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                 \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
            B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
            Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
         (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2)*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
             (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
               \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
               \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])) + 
      (t*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     Quantity[-0.00018611390500671325, "Radians"]/E^(4748.837251579376*t) + 
      Quantity[89.03611477710645, "Radians"]/E^(6.865838702933615*t) + 
      Quantity[-89.03592866320143 + 610.4237781503409*t, "Radians"], 
     -89.03592866320143 - 0.00018611390500671325/E^(4748.837251579376*t) + 
      89.03611477710645/E^(6.865838702933615*t) + 610.4237781503409*t}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Ke*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + 
                   R*\[Tau]app0), Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*
                  \[CapitalNu]}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                   (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
              SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "vg", 
                Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
            aTime$][[1]]]; fn$]], 
     <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/("Amperes"^2*
          "Seconds"^3)], L -> Quantity[347/500000, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^2)], \[CapitalNu] -> 60, \[Eta] -> 9/10, 
      Ke -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[0, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[12, "Volts"]|>, t] = 
    {(-2*Ke*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (Ke*\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
        ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (Ke*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     Quantity[-10.860898409666166, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Seconds"^3)]/E^(6.865838702933615*t) + 
      Quantity[0.01570261786185933, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Seconds"^3)]/E^(4748.837251579376*t) + 
      Quantity[10.845195791804308, ("Kilograms"*"Meters"^2)/
        ("Amperes"*"Seconds"^3)], 10.845195791804308 + 
      0.01570261786185933/E^(4748.837251579376*t) - 10.860898409666166/
       E^(6.865838702933615*t)}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
                    Ke*\[Tau]app0), Bafter + Jafter*s + (B + J*s)*\[Eta]*
                   \[CapitalNu]^2, -(Ke*\[CapitalNu])}}, {{Bafter*R + 
                  (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                     \[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "i", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^3)], L -> Quantity[347/500000, 
        ("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)], 
      \[CapitalNu] -> 60, \[Eta] -> 9/10, Ke -> Quantity[533/30000, 
        ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[0, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[12, "Volts"]|>, t] = 
    {(2*Ke*L*\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (\[CapitalDelta]vappConst*
        ((-2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
           (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
             \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
             \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
          Ke*\[Tau]app0))/(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     Quantity[-3.6458810357407025, "Amperes"]/E^(4748.837251579376*t) + 
      Quantity[0.3499406691502722, "Amperes"] + 
      Quantity[3.2959403665904303, "Amperes"]/E^(6.865838702933615*t), 
     0.3499406691502722 - 3.6458810357407025/E^(4748.837251579376*t) + 
      3.2959403665904303/E^(6.865838702933615*t)}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*(Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*
                      \[CapitalNu] - Ke*\[Tau]app0)), Kt*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), -(Ke*Kt*\[CapitalNu])}}, 
               {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Tau]", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^3)], L -> Quantity[347/500000, 
        ("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)], 
      \[CapitalNu] -> 60, \[Eta] -> 9/10, Ke -> Quantity[533/30000, 
        ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[0, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[12, "Volts"]|>, t] = 
    {(2*Ke*Kt*L*\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (Kt*\[CapitalDelta]vappConst*
        ((-2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
           (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
             \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
             \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (Kt*(Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
           Ke*\[Tau]app0)))/(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     Quantity[-0.06477515306832649, ("Kilograms"*"Meters"^2)/
         ("Radians"*"Seconds"^2)]/E^(4748.837251579376*t) + 
      Quantity[0.006217279221903051, ("Kilograms"*"Meters"^2)/
        ("Radians"*"Seconds"^2)] + Quantity[0.05855787384642342, 
        ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]/
       E^(6.865838702933615*t), 0.006217279221903051 - 
      0.06477515306832649/E^(4748.837251579376*t) + 0.05855787384642342/
       E^(6.865838702933615*t)}
 
stepMotorTimeFunction[Function[{vappfn$, \[Tau]appfn$}, 
      Module[{fn$}, fn$ = Function[{aTime$}, 
          makeTimeDomainFunctionConvolve[TransferFunctionModel[
              {{{Kt*\[Eta]*\[CapitalNu]*(Bafter*vapp0 + \[CapitalNu]*
                    (B*vapp0*\[Eta]*\[CapitalNu] - Ke*\[Tau]app0)), 
                 Kt*\[Eta]*\[CapitalNu]*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                    \[CapitalNu]^2), -(Ke*Kt*\[Eta]*\[CapitalNu]^2)}}, {
                {Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                 Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                    (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                   \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                     \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> {
                {"1", "vapp", "\[Tau]app"}, "\[Tau]after", Automatic}], 
             {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; 
        fn$]], <|R -> Quantity[33/10, ("Kilograms"*"Meters"^2)/
         ("Amperes"^2*"Seconds"^3)], L -> Quantity[347/500000, 
        ("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)], 
      \[CapitalNu] -> 60, \[Eta] -> 9/10, Ke -> Quantity[533/30000, 
        ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*"Seconds"^2)], 
      Kt -> Quantity[533/30000, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      B -> Quantity[11/1080000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], J -> Quantity[347/108000000000, 
        ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Jafter -> Quantity[1/20, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], \[CapitalDelta]\[Tau]appConst -> 
       Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]|>, 
     <|t -> Quantity[t, "Seconds"], vapp0 -> Quantity[0, "Volts"], 
      \[Tau]app0 -> Quantity[0, ("Meters"*"Newtons")/"Radians"], 
      \[CapitalDelta]vappConst -> Quantity[12, "Volts"]|>, t] = 
    {(2*Ke*Kt*L*\[CapitalDelta]\[Tau]appConst*\[Eta]*\[CapitalNu]^2*
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*
        ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
      (Kt*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
        ((-2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
       (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
           (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
             \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
             \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
      (Kt*\[Eta]*\[CapitalNu]*(Bafter*vapp0 + \[CapitalNu]*
          (B*vapp0*\[Eta]*\[CapitalNu] - Ke*\[Tau]app0)))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     Quantity[-3.49785826568963, ("Kilograms"*"Meters"^2)/
         ("Radians"*"Seconds"^2)]/E^(4748.837251579376*t) + 
      Quantity[0.3357330779828649, ("Kilograms"*"Meters"^2)/
        ("Radians"*"Seconds"^2)] + Quantity[3.162125187706765, 
        ("Kilograms"*"Meters"^2)/("Radians"*"Seconds"^2)]/
       E^(6.865838702933615*t), 0.3357330779828649 - 
      3.49785826568963/E^(4748.837251579376*t) + 3.162125187706765/
       E^(6.865838702933615*t)}
 
stepMotorTimeFunction[motorTimeFunction_, motorWithLoad_, input_, t_] := 
    stepMotorTimeFunction[motorTimeFunction, motorWithLoad, input, t] = 
     Module[{generic, withUnits, unitless}, 
      generic = stepMotorTimeFunctionGeneric[motorTimeFunction, t]; 
       withUnits = FullSimplify[N[siUnits[generic /. Join[input, 
             motorWithLoad]]]]; unitless = FullSimplify[
         clearUnits[withUnits]]; {generic, withUnits, unitless}]
 
clearUnits[expr_] := expr /. Quantity[val_, unit_] :> val
 
ss = {ssPos -> Indeterminate, ssVel -> 
      (\[CapitalNu]*(Kt*(vapp0 + \[CapitalDelta]vappConst)*\[Eta]*
          \[CapitalNu] + R*(\[CapitalDelta]\[Tau]appConst + \[Tau]app0)))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), ssAcc -> 0, 
     ssEmf -> (Ke*\[CapitalNu]*(Kt*(vapp0 + \[CapitalDelta]vappConst)*\[Eta]*
          \[CapitalNu] + R*(\[CapitalDelta]\[Tau]appConst + \[Tau]app0)))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     ssCur -> (Bafter*(vapp0 + \[CapitalDelta]vappConst) + 
        \[CapitalNu]*(B*(vapp0 + \[CapitalDelta]vappConst)*\[Eta]*
           \[CapitalNu] - Ke*(\[CapitalDelta]\[Tau]appConst + \[Tau]app0)))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     ssTor -> (Kt*(Bafter*(vapp0 + \[CapitalDelta]vappConst) + 
         \[CapitalNu]*(B*(vapp0 + \[CapitalDelta]vappConst)*\[Eta]*
            \[CapitalNu] - Ke*(\[CapitalDelta]\[Tau]appConst + \[Tau]app0))))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
     ssPosAfter -> Indeterminate, ssVelAfter -> 
      (Kt*(vapp0 + \[CapitalDelta]vappConst)*\[Eta]*\[CapitalNu] + 
        R*(\[CapitalDelta]\[Tau]appConst + \[Tau]app0))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), ssAccAfter -> 0, 
     ssTorAfter -> (Kt*\[Eta]*\[CapitalNu]*
        (Bafter*(vapp0 + \[CapitalDelta]vappConst) + \[CapitalNu]*
          (B*(vapp0 + \[CapitalDelta]vappConst)*\[Eta]*\[CapitalNu] - 
           Ke*(\[CapitalDelta]\[Tau]appConst + \[Tau]app0))))/
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)}
 
simpleSs = {ssPos -> Indeterminate, ssVel -> (Kt*\[CapitalDelta]vappConst)/
       (Ke*Kt + B*R), ssAcc -> 0, ssEmf -> (Ke*Kt*\[CapitalDelta]vappConst)/
       (Ke*Kt + B*R), ssCur -> (B*\[CapitalDelta]vappConst)/(Ke*Kt + B*R), 
     ssTor -> (B*Kt*\[CapitalDelta]vappConst)/(Ke*Kt + B*R), 
     ssPosAfter -> Indeterminate, ssVelAfter -> (Kt*\[CapitalDelta]vappConst)/
       (Ke*Kt + B*R), ssAccAfter -> 0, ssTorAfter -> 
      (B*Kt*\[CapitalDelta]vappConst)/(Ke*Kt + B*R)}
 
velAfterStepGeneric = (-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (\[CapitalDelta]\[Tau]appConst*
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0)/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
velStepGeneric = (-2*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
torAfterStepGeneric = (2*Ke*Kt*L*\[CapitalDelta]\[Tau]appConst*\[Eta]*
       \[CapitalNu]^2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (Kt*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
       ((-2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                    (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                    Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Kt*\[Eta]*\[CapitalNu]*(Bafter*vapp0 + \[CapitalNu]*
         (B*vapp0*\[Eta]*\[CapitalNu] - Ke*\[Tau]app0)))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
torStepGeneric = (2*Ke*Kt*L*\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (Kt*\[CapitalDelta]vappConst*
       ((-2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                    (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                    Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Kt*(Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
          Ke*\[Tau]app0)))/(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
curStepGeneric = (2*Ke*L*\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (\[CapitalDelta]vappConst*
       ((-2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                    (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                    Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
         Ke*\[Tau]app0))/(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
emfStepGeneric = (-2*Ke*Kt*L*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
       (Jafter + J*\[Eta]*\[CapitalNu]^2)*
       ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*
                \[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                    \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                    \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
              J*\[Eta]*\[CapitalNu]^2))))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
          (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
        (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2] + 
     (Ke*\[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 - 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + 
                    J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*(Jafter + J*\[Eta]*\[CapitalNu]^2))/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*Jafter*L*R*(Jafter + 
           J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L^2*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*J*L*R*\[Eta]*\[CapitalNu]^2*
          (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
          B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
        (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                     \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                     \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                     \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2))))*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])/
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2 + 
          Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
      (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]) + 
     (Ke*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
posStepGeneric = (Kt*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]^2*
       (t - (L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          (-((Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                    Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                        (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                        Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2])) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
             Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
              \[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
               (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                2]) - (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
             \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
             B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
           (Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) + 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
             Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
              \[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
               (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                2]) + (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
             \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
             B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
         Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
     \[CapitalDelta]\[Tau]appConst*\[CapitalNu]*
      ((R*t)/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         B*R*\[Eta]*\[CapitalNu]^2) - 
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                   2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                     J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                      \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                     (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
        (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
           \[CapitalNu]^2)*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])) + 
     (t*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
 
posAfterStepGeneric = (Kt*\[CapitalDelta]vappConst*\[Eta]*\[CapitalNu]*
       (t - (L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
          (-((Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                    Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                        (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                        Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                 (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
             (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                   \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                   \[Eta]*\[CapitalNu]^2)^2])) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
             Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
              \[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
               (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                2]) - (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
             \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
             B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
           (Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) + 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Jafter*R)/(Bafter*L + 
             Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
              \[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
                (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
               (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^
                2]) + (B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                    \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                   Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                       (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                       Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
                (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L*\[Eta]*
             \[CapitalNu]^2)/(Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*J*R*\[Eta]*\[CapitalNu]^2)/
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2]) - 
           ((-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                   J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                        \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                        \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                        \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                  J*\[Eta]*\[CapitalNu]^2))))*Sqrt[-4*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                  \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
             B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
             Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                 (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                 Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2])))/
         Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
     \[CapitalDelta]\[Tau]appConst*
      ((R*t)/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         B*R*\[Eta]*\[CapitalNu]^2) - 
       ((2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^
                   2 + J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + 
                     J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*
                      \[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                     (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 - Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 - 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*Bafter*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
                 Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
                     (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + (Bafter*L + 
                     Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2))))*L^2*R*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Jafter*L*R^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (4*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*Ke*Kt*L^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*B*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L^2*R*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) + 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*J*L*R^2*\[Eta]*\[CapitalNu]^2*
           (Jafter + J*\[Eta]*\[CapitalNu]^2))/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]) - 
         (2*(-1 + E^(-(t*(Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                 J*R*\[Eta]*\[CapitalNu]^2 + Sqrt[-4*L*(Jafter + J*\[Eta]*
                      \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                      \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*
                      \[Eta]*\[CapitalNu]^2)^2]))/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2))))*L*R*(Jafter + 
            J*\[Eta]*\[CapitalNu]^2)*Sqrt[-4*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                \[CapitalNu]^2) + (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*
                \[CapitalNu]^2)^2])/(Bafter*L + Jafter*R + 
           B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2 + 
           Sqrt[-4*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
               (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2) + 
             (Bafter*L + Jafter*R + (B*L + J*R)*\[Eta]*\[CapitalNu]^2)^2]))/
        (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
           \[CapitalNu]^2)*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])) + 
     (t*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0))/
      (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)
