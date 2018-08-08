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
       assoc[Jafter] = Quantity[0, siAngularInertialUnits]; 
       assoc[Bafter] = Quantity[0, siAngularViscousDragUnits]; 
       assoc[const\[Tau]appafter] = Quantity[0, siTorqueUnits]; 
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
     vapp[t] -> Quantity[vapp[t], "Volts"], constvapp -> 
      Quantity[constvapp, "Volts"], vg[t] -> Quantity[vg[t], "Volts"], 
     J -> Quantity[J, ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2], 
     Jafter -> Quantity[Jafter, ("Meters"*"Newtons"*"Seconds"^2)/
        "Radians"^2], B -> Quantity[B, ("Meters"*"Newtons"*"Seconds")/
        "Radians"^2], Bafter -> Quantity[Bafter, 
       ("Meters"*"Newtons"*"Seconds")/"Radians"^2], 
     Ke -> Quantity[Ke, ("Seconds"*"Volts")/"Radians"], 
     KeShaft -> Quantity[KeShaft, ("Seconds"*"Volts")/"Radians"], 
     Kt -> Quantity[Kt, ("Meters"*"Newtons")/("Amperes"*"Radians")], 
     KtShaft -> Quantity[KtShaft, ("Meters"*"Newtons")/
        ("Amperes"*"Radians")], \[Theta][t] -> Quantity[\[Theta][t], 
       "Radians"], \[Theta]after[t] -> Quantity[\[Theta]after[t], "Radians"], 
     Derivative[1][\[Theta]][t] -> Quantity[Derivative[1][\[Theta]][t], 
       "Radians"/"Seconds"], Derivative[1][\[Theta]after][t] -> 
      Quantity[Derivative[1][\[Theta]after][t], "Radians"], 
     Derivative[2][\[Theta]][t] -> Quantity[Derivative[2][\[Theta]][t], 
       "Radians"/"Seconds"^2], Derivative[2][\[Theta]after][t] -> 
      Quantity[Derivative[2][\[Theta]after][t], "Radians"], 
     \[Omega][t] -> Quantity[\[Omega][t], "Radians"/"Seconds"], 
     \[Alpha][t] -> Quantity[\[Alpha][t], "Radians"/"Seconds"^2], 
     \[Tau][t] -> Quantity[\[Tau][t], ("Meters"*"Newtons")/"Radians"], 
     \[Tau]appafter[t] -> Quantity[\[Tau]appafter[t], 
       ("Meters"*"Newtons")/"Radians"], const\[Tau]appafter -> 
      Quantity[const\[Tau]appafter, ("Meters"*"Newtons")/"Radians"], 
     \[CapitalNu] -> Quantity[\[CapitalNu], "DimensionlessUnit"], 
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
     assoc[Jafter] = OptionValue[J] + OptionValue[Jafter]; 
      assoc[Bafter] = OptionValue[B] + OptionValue[Bafter]; 
      assoc[const\[Tau]appafter] = OptionValue[const\[Tau]appafter]; assoc]
 
Options[motorLoad] = {J -> Quantity[0, ("Meters"*"Newtons"*"Seconds"^2)/
        "Radians"^2], Jafter -> Quantity[0, ("Meters"*"Newtons"*"Seconds"^2)/
        "Radians"^2], B -> Quantity[0, ("Meters"*"Newtons"*"Seconds")/
        "Radians"^2], Bafter -> Quantity[0, ("Meters"*"Newtons"*"Seconds")/
        "Radians"^2], const\[Tau]appafter -> Quantity[0, 
       ("Meters"*"Newtons")/"Radians"]}
 
flywheel[mass_, radius_] := motorLoad[
     J -> mass*(radius^2/2/Quantity[1, "Radians^2"])]
 
addMotorLoad[motor_, load_] := Module[{assoc}, assoc = Association[motor]; 
      assoc[Jafter] = (Jafter /. motor) + (Jafter /. load); 
      assoc[Bafter] = (Bafter /. motor) + (Bafter /. load); 
      assoc[const\[Tau]appafter] = (const\[Tau]appafter /. motor) + 
        (const\[Tau]appafter /. load); assoc]
 
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
      motorLoad[const\[Tau]appafter -> Quantity[parts[[1]], unit], 
       Jafter -> Quantity[parts[[2]], unit]/angularAcceleration]]
 
siAngularAccelerationUnits = "Radians"/"Seconds"^2
 
applyTimeFunction[Function[{vapp$, \[Tau]appafter$}, 
      Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[
          TransferFunctionModel[{{{Kt*\[Eta]*\[CapitalNu]^2, 
              (R + L*s)*\[CapitalNu]}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
             (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^
                 2)}, s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, 
             "\[Omega]", Automatic}], {vapp$[#1] & , 
           \[Tau]appafter$[#1] & }]; fn$]], <|R -> 3.3, L -> 0.000694, 
      Ke -> 0.017766666666666667, Kt -> 0.017766666666666667, 
      J -> 0.00001041, B -> 0.033, \[CapitalNu] -> 60., \[Eta] -> 0.9, 
      Jafter -> 0.0096, Bafter -> 0., const\[Tau]appafter -> 0.|>, 
     <|t -> t, constvapp -> 12.|>, t] = 
    {{1.952101107524381 + 2.1468311673524805/E^(4740.076116086594*t) - 
       4.098932274876861/E^(2482.632661195531*t)}, 
     {1.952101107524381 + 2.1468311673524805/E^(4740.076116086594*t) - 
       4.098932274876861/E^(2482.632661195531*t)}}
 
applyTimeFunction[Function[{vapp$, \[Tau]appafter$}, 
      Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[
          TransferFunctionModel[{{{Kt*\[Eta]*\[CapitalNu]^2, 
              (R + L*s)*\[CapitalNu]}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
             (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^
                 2)}, s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, 
             "\[Omega]", Automatic}], {vapp$[#1] & , 
           \[Tau]appafter$[#1] & }]; fn$]], <|R -> 3.3, L -> 0.000694, 
      Ke -> 1.066, Kt -> 1.066, J -> 0.00001041, B -> 0.033, 
      \[CapitalNu] -> 1., \[Eta] -> 1., Jafter -> 0.0096, Bafter -> 0., 
      const\[Tau]appafter -> 0.|>, <|t -> t, constvapp -> 12.|>, t] = 
    {{10.272586520362115 + 0.08685804310541265/E^(4718.9116176050775*t) - 
       10.359444563467529/E^(39.56538655923656*t)}, 
     {10.272586520362115 + 0.08685804310541265/E^(4718.9116176050775*t) - 
       10.359444563467529/E^(39.56538655923656*t)}}
 
applyTimeFunction[Function[{vapp$, \[Tau]appafter$}, 
      Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[
          TransferFunctionModel[{{{Kt*\[Eta]*\[CapitalNu]^2, 
              (R + L*s)*\[CapitalNu]}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
             (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^
                 2)}, s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, 
             "\[Omega]", Automatic}], {vapp$[#1] & , 
           \[Tau]appafter$[#1] & }]; fn$]], 
     <|R -> Quantity[33/10, "Watts"/"Amperes"^2], 
      L -> Quantity[347/500000, "Henries"], 
      Ke -> Quantity[533/500, ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*
          "Seconds"^2)], Kt -> Quantity[533/500, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      J -> Quantity[1041/100000000, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      B -> Quantity[33/1000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], \[CapitalNu] -> 1, \[Eta] -> 1, 
      Jafter -> Quantity[1, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], const\[Tau]appafter -> Quantity[0, 
        ("Meters"*"Newtons")/"Radians"]|>, <|t -> Quantity[t, "Seconds"], 
      constvapp -> Quantity[12, "Volts"]|>, t] = 
    {{Quantity[1599000/155657, "Radians"/"Seconds"] + 
       E^Quantity[(5000*(-16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[-799500/155657 - 13191978876862500/
           (155657*Sqrt[272173025490913197401]), "Radians"/"Seconds"] + 
       E^Quantity[(-5000*(16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[-799500/155657 + 13191978876862500/
           (155657*Sqrt[272173025490913197401]), "Radians"/"Seconds"]}, 
     {10.272586520362077 + 0.0008153853405064027/E^(4754.698853616825*t) - 
       10.273401905702585/E^(0.37737370535485504*t)}}
 
applyTimeFunction[Function[{vapp$, \[Tau]appafter$}, 
      Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[
          TransferFunctionModel[{{{Kt*\[Eta]*\[CapitalNu]^2, 
              (R + L*s)*\[CapitalNu]}}, s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
              (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^
                  2))}, s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, 
             "\[Theta]", Automatic}], {vapp$[#1] & , 
           \[Tau]appafter$[#1] & }]; fn$]], 
     <|R -> Quantity[33/10, "Watts"/"Amperes"^2], 
      L -> Quantity[347/500000, "Henries"], 
      Ke -> Quantity[533/500, ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*
          "Seconds"^2)], Kt -> Quantity[533/500, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      J -> Quantity[1041/100000000, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      B -> Quantity[33/1000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], \[CapitalNu] -> 1, \[Eta] -> 1, 
      Jafter -> Quantity[1, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], const\[Tau]appafter -> Quantity[0, 
        ("Meters"*"Newtons")/"Radians"]|>, <|t -> Quantity[t, "Seconds"], 
      constvapp -> Quantity[12, "Volts"]|>, t] = 
    {{E^Quantity[(-5000*(16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[5276791550745/387665626384 - 
          435273761881969325134287/(1938328131920*
            Sqrt[272173025490913197401]), "Radians"] + 
       E^Quantity[(5000*(-16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[5276791550745/387665626384 + 
          435273761881969325134287/(1938328131920*
            Sqrt[272173025490913197401]), "Radians"] + 
       Quantity[(7995*(-660011451 + 249051200*t))/193832813192, "Radians"]}, 
     {-27.223417252465424 - 1.7149042719211138*^-7/E^(4754.698853616825*t) + 
       27.223417423955848/E^(0.37737370535485504*t) + 10.27258652036208*t}}
 
applyTimeFunction[Function[{vapp$, \[Tau]appafter$}, 
      Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[
          TransferFunctionModel[{{{Ke*Kt*\[Eta]*\[CapitalNu]^2, 
              Ke*(R + L*s)*\[CapitalNu]}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
             (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^
                 2)}, s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, 
             "vg", Automatic}], {vapp$[#1] & , \[Tau]appafter$[#1] & }]; 
        fn$]], <|R -> Quantity[33/10, "Watts"/"Amperes"^2], 
      L -> Quantity[347/500000, "Henries"], 
      Ke -> Quantity[533/500, ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*
          "Seconds"^2)], Kt -> Quantity[533/500, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      J -> Quantity[1041/100000000, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      B -> Quantity[33/1000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], \[CapitalNu] -> 1, \[Eta] -> 1, 
      Jafter -> Quantity[1, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], const\[Tau]appafter -> Quantity[0, 
        ("Meters"*"Newtons")/"Radians"]|>, <|t -> Quantity[t, "Seconds"], 
      constvapp -> Quantity[12, "Volts"]|>, t] = 
    {{Quantity[1704534/155657, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Seconds"^3)] + 
       E^Quantity[(5000*(-16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[-852267/155657 - 14062649482735425/
           (155657*Sqrt[272173025490913197401]), ("Kilograms"*"Meters"^2)/
          ("Amperes"*"Seconds"^3)] + 
       E^Quantity[(-5000*(16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[-852267/155657 + 14062649482735425/
           (155657*Sqrt[272173025490913197401]), ("Kilograms"*"Meters"^2)/
          ("Amperes"*"Seconds"^3)]}, {10.950577230705976 + 
       0.0008692007729775852/E^(4754.698853616825*t) - 
       10.951446431478953/E^(0.37737370535485504*t)}}
 
applyTimeFunction[Function[{vapp$, \[Tau]appafter$}, 
      Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[
          TransferFunctionModel[{{{Bafter + Jafter*s + (B + J*s)*\[Eta]*
                \[CapitalNu]^2, -(Ke*\[CapitalNu])}}, 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
               (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
            {{"vapp", "\[Tau]appafter"}, "i", Automatic}], 
          {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]], 
     <|R -> Quantity[33/10, "Watts"/"Amperes"^2], 
      L -> Quantity[347/500000, "Henries"], 
      Ke -> Quantity[533/500, ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*
          "Seconds"^2)], Kt -> Quantity[533/500, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      J -> Quantity[1041/100000000, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      B -> Quantity[33/1000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], \[CapitalNu] -> 1, \[Eta] -> 1, 
      Jafter -> Quantity[1, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], const\[Tau]appafter -> Quantity[0, 
        ("Meters"*"Newtons")/"Radians"]|>, <|t -> Quantity[t, "Seconds"], 
      constvapp -> Quantity[12, "Volts"]|>, t] = 
    {{Quantity[49500/155657, "Amperes"] + 
       E^Quantity[(-5000*(16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[-24750/155657 - 8931135138055950/
           (155657*Sqrt[272173025490913197401]), "Amperes"] + 
       E^Quantity[(5000*(-16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[-24750/155657 + 8931135138055950/
           (155657*Sqrt[272173025490913197401]), "Amperes"]}, 
     {0.3180068997860681 - 3.636890426538978/E^(4754.698853616825*t) + 
       3.3188835267529098/E^(0.37737370535485504*t)}}
 
applyTimeFunction[Function[{vapp$, \[Tau]appafter$}, 
      Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[
          TransferFunctionModel[{{{Kt*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                 \[CapitalNu]^2), -(Ke*Kt*\[CapitalNu])}}, 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
               (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
            {{"vapp", "\[Tau]appafter"}, "\[Tau]", Automatic}], 
          {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]], 
     <|R -> Quantity[33/10, "Watts"/"Amperes"^2], 
      L -> Quantity[347/500000, "Henries"], 
      Ke -> Quantity[533/500, ("Kilograms"*"Meters"^2)/("Amperes"*"Radians"*
          "Seconds"^2)], Kt -> Quantity[533/500, ("Kilograms"*"Meters"^2)/
         ("Amperes"*"Radians"*"Seconds"^2)], 
      J -> Quantity[1041/100000000, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      B -> Quantity[33/1000, ("Kilograms"*"Meters"^2)/
         ("Radians"^2*"Seconds")], \[CapitalNu] -> 1, \[Eta] -> 1, 
      Jafter -> Quantity[1, ("Kilograms"*"Meters"^2)/"Radians"^2], 
      Bafter -> Quantity[0, ("Kilograms"*"Meters"^2)/("Radians"^2*
          "Seconds")], const\[Tau]appafter -> Quantity[0, 
        ("Meters"*"Newtons")/"Radians"]|>, <|t -> Quantity[t, "Seconds"], 
      constvapp -> Quantity[12, "Volts"]|>, t] = 
    {{Quantity[52767/155657, ("Kilograms"*"Meters"^2)/
         ("Radians"*"Seconds"^2)] + 
       E^Quantity[(-5000*(16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[-52767/311314 - 95205900571676427/
           (1556570*Sqrt[272173025490913197401]), ("Kilograms"*"Meters"^2)/
          ("Radians"*"Seconds"^2)] + 
       E^Quantity[(5000*(-16500286275 + Sqrt[272173025490913197401])*t)/
           34700361227, (Sqrt["Kilograms"]*"Meters")/("Amperes"*
            Sqrt[("Kilograms"*"Meters"^2)/("Amperes"^2*"Seconds"^2)]*
            "Seconds")]*Quantity[-52767/311314 + 95205900571676427/
           (1556570*Sqrt[272173025490913197401]), ("Kilograms"*"Meters"^2)/
          ("Radians"*"Seconds"^2)]}, {0.33899535517194856 - 
       3.8769251946905507/E^(4754.698853616825*t) + 3.5379298395186023/
        E^(0.37737370535485504*t)}}
 
applyTimeFunction[motorTimeFunction_, motorWithLoad_, input_, t_] := 
    applyTimeFunction[motorTimeFunction, motorWithLoad, input, t] = 
     Module[{generic, withUnits, unitless}, 
      generic = motorTimeFunction[constvapp & , const\[Tau]appafter & ][t]; 
       withUnits = FullSimplify[siUnits[generic /. motorWithLoad /. input]]; 
       unitless = FullSimplify[N[clearUnits[withUnits]]]; 
       {withUnits, unitless}]
 
Attributes[vapp$] = {Temporary}
 
Attributes[\[Tau]appafter$] = {Temporary}
 
Attributes[fn$] = {Temporary}
 
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
 
parameterAssumptions = {Element[Bafter, Reals], Element[constvapp, Reals], 
     Element[const\[Tau]appafter, Reals], Element[Jafter, Reals], 
     Element[i[_], Reals], Element[vapp[_], Reals], Element[vg[_], Reals], 
     Element[\[Alpha][_], Reals], Element[\[Theta][_], Reals], 
     Element[\[Tau][_], Reals], Element[\[Tau]appafter[_], Reals], 
     Element[\[Omega][_], Reals], Ke > 0, Kt > 0, L > 0, R > 0, \[Eta] > 0, 
     \[CapitalNu] > 0, B >= 0, J >= 0, t >= 0}
 
clearUnits[expr_] := expr /. Quantity[val_, unit_] :> val
