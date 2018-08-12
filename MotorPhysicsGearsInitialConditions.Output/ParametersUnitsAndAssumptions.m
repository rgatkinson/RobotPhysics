parameterUnits = <|R -> "Ohms", L -> "Henries", i[t] -> "Amperes", 
     Derivative[1][i][t] -> "Amperes"/"Seconds", Derivative[2][i][t] -> 
      "Amperes"/"Seconds"^2, \[CapitalDelta]vapp[t] -> "Volts", 
     \[CapitalDelta]vappConst -> "Volts", vg[t] -> "Volts", vapp0 -> "Volts", 
     J -> ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2, 
     Jafter -> ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2, 
     B -> ("Meters"*"Newtons"*"Seconds")/"Radians"^2, 
     Bafter -> ("Meters"*"Newtons"*"Seconds")/"Radians"^2, 
     Ke -> ("Seconds"*"Volts")/"Radians", 
     Kt -> ("Meters"*"Newtons")/("Amperes"*"Radians"), 
     \[Theta][t] -> "Radians", \[Theta]after[t] -> "Radians", 
     Derivative[1][\[Theta]][t] -> "Radians"/"Seconds", 
     Derivative[1][\[Theta]after][t] -> "Radians", 
     Derivative[2][\[Theta]][t] -> "Radians"/"Seconds"^2, 
     Derivative[2][\[Theta]after][t] -> "Radians", 
     \[Omega][t] -> "Radians"/"Seconds", \[Alpha][t] -> 
      "Radians"/"Seconds"^2, \[Tau][t] -> ("Meters"*"Newtons")/"Radians", 
     \[Tau]after[t] -> ("Meters"*"Newtons")/"Radians", 
     \[CapitalDelta]\[Tau]app[t] -> ("Meters"*"Newtons")/"Radians", 
     \[CapitalDelta]\[Tau]appConst -> ("Meters"*"Newtons")/"Radians", 
     \[Tau]app0 -> ("Meters"*"Newtons")/"Radians", 
     \[CapitalNu] -> "DimensionlessUnit", \[Eta] -> "DimensionlessUnit"|>
 
Attributes[Derivative] = {NHoldAll, ReadProtected}
 
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
 
radiansUnits = "Radians"
 
secondsUnits = "Seconds"
 
siAngularPositionUnits = "Radians"
 
siAngularVelocityUnits = "Radians"/"Seconds"
 
siAngularAccelerationUnits = "Radians"/"Seconds"^2
 
siTorqueUnits = ("Meters"*"Newtons")/"Radians"
 
siAngularViscousDragUnits = ("Meters"*"Newtons"*"Seconds")/"Radians"^2
 
siAngularInertialUnits = ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2
