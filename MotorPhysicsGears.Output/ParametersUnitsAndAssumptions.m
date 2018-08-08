parameterUnits = <|R -> "Ohms", L -> "Henries", i[t] -> "Amperes", 
     Derivative[1][i][t] -> "Amperes"/"Seconds", Derivative[2][i][t] -> 
      "Amperes"/"Seconds"^2, vapp[t] -> "Volts", constvapp -> "Volts", 
     vg[t] -> "Volts", J -> ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2, 
     Jafter -> ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2, 
     B -> ("Meters"*"Newtons"*"Seconds")/"Radians"^2, 
     Bafter -> ("Meters"*"Newtons"*"Seconds")/"Radians"^2, 
     Ke -> ("Seconds"*"Volts")/"Radians", 
     KeShaft -> ("Seconds"*"Volts")/"Radians", 
     Kt -> ("Meters"*"Newtons")/("Amperes"*"Radians"), 
     KtShaft -> ("Meters"*"Newtons")/("Amperes"*"Radians"), 
     \[Theta][t] -> "Radians", \[Theta]after[t] -> "Radians", 
     Derivative[1][\[Theta]][t] -> "Radians"/"Seconds", 
     Derivative[1][\[Theta]after][t] -> "Radians", 
     Derivative[2][\[Theta]][t] -> "Radians"/"Seconds"^2, 
     Derivative[2][\[Theta]after][t] -> "Radians", 
     \[Omega][t] -> "Radians"/"Seconds", \[Alpha][t] -> 
      "Radians"/"Seconds"^2, \[Tau][t] -> ("Meters"*"Newtons")/"Radians", 
     \[Tau]appafter[t] -> ("Meters"*"Newtons")/"Radians", 
     const\[Tau]appafter -> ("Meters"*"Newtons")/"Radians", 
     \[CapitalNu] -> "DimensionlessUnit", \[Eta] -> "DimensionlessUnit"|>
 
Attributes[Derivative] = {NHoldAll, ReadProtected}
 
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
 
parameterAssumptions = {Element[Bafter, Reals], Element[constvapp, Reals], 
     Element[const\[Tau]appafter, Reals], Element[Jafter, Reals], 
     Element[i[_], Reals], Element[vapp[_], Reals], Element[vg[_], Reals], 
     Element[\[Alpha][_], Reals], Element[\[Theta][_], Reals], 
     Element[\[Tau][_], Reals], Element[\[Tau]appafter[_], Reals], 
     Element[\[Omega][_], Reals], Ke > 0, Kt > 0, L > 0, R > 0, \[Eta] > 0, 
     \[CapitalNu] > 0, B >= 0, J >= 0, t >= 0}
 
radiansUnits = "Radians"
 
secondsUnits = "Seconds"
 
siAngularPositionUnits = "Radians"
 
siAngularVelocityUnits = "Radians"/"Seconds"
 
siAngularAccelerationUnits = "Radians"/"Seconds"^2
 
siTorqueUnits = ("Meters"*"Newtons")/"Radians"
 
siAngularViscousDragUnits = ("Meters"*"Newtons"*"Seconds")/"Radians"^2
 
siAngularInertialUnits = ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2
