parameterUnits = <|R -> "Ohms", L -> "Henries", i[t] -> "Amperes", 
     Derivative[1][i][t] -> "Amperes"/"Seconds", Derivative[2][i][t] -> 
      "Amperes"/"Seconds"^2, ea[t] -> "Volts", e[t] -> "Volts", 
     J -> ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2, 
     Jout -> ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2, 
     b -> ("Meters"*"Newtons"*"Seconds")/"Radians"^2, 
     bout -> ("Meters"*"Newtons"*"Seconds")/"Radians"^2, 
     Ke -> ("Seconds"*"Volts")/"Radians", 
     KeShaft -> ("Seconds"*"Volts")/"Radians", 
     Kt -> ("Meters"*"Newtons")/("Amperes"*"Radians"), 
     KtShaft -> ("Meters"*"Newtons")/("Amperes"*"Radians"), 
     \[Theta][t] -> "Radians", \[Theta]out[t] -> "Radians", 
     Derivative[1][\[Theta]][t] -> "Radians"/"Seconds", 
     Derivative[1][\[Theta]out][t] -> "Radians", 
     Derivative[2][\[Theta]][t] -> "Radians"/"Seconds"^2, 
     Derivative[2][\[Theta]out][t] -> "Radians", \[CapitalOmega][t] -> 
      "Radians"/"Seconds", \[Alpha][t] -> "Radians"/"Seconds"^2, 
     \[Tau][t] -> ("Meters"*"Newtons")/"Radians", 
     \[Tau]a[t] -> ("Meters"*"Newtons")/"Radians", 
     \[Tau]out[t] -> ("Meters"*"Newtons")/"Radians", 
     const\[Tau]out -> ("Meters"*"Newtons")/"Radians", 
     \[CapitalNu] -> "DimensionlessUnit", \[Eta] -> "DimensionlessUnit"|>
 
Attributes[Derivative] = {NHoldAll, ReadProtected}
 
parameterQuantities = <|R -> Quantity[R, "Ohms"], 
     L -> Quantity[L, "Henries"], i[t] -> Quantity[i[t], "Amperes"], 
     Derivative[1][i][t] -> Quantity[Derivative[1][i][t], 
       "Amperes"/"Seconds"], Derivative[2][i][t] -> 
      Quantity[Derivative[2][i][t], "Amperes"/"Seconds"^2], 
     ea[t] -> Quantity[ea[t], "Volts"], e[t] -> Quantity[e[t], "Volts"], 
     J -> Quantity[J, ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2], 
     Jout -> Quantity[Jout, ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2], 
     b -> Quantity[b, ("Meters"*"Newtons"*"Seconds")/"Radians"^2], 
     bout -> Quantity[bout, ("Meters"*"Newtons"*"Seconds")/"Radians"^2], 
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
     \[CapitalOmega][t] -> Quantity[\[CapitalOmega][t], "Radians"/"Seconds"], 
     \[Alpha][t] -> Quantity[\[Alpha][t], "Radians"/"Seconds"^2], 
     \[Tau][t] -> Quantity[\[Tau][t], ("Meters"*"Newtons")/"Radians"], 
     \[Tau]a[t] -> Quantity[\[Tau]a[t], ("Meters"*"Newtons")/"Radians"], 
     \[Tau]out[t] -> Quantity[\[Tau]out[t], ("Meters"*"Newtons")/"Radians"], 
     const\[Tau]out -> Quantity[const\[Tau]out, ("Meters"*"Newtons")/
        "Radians"], \[CapitalNu] -> Quantity[\[CapitalNu], 
       "DimensionlessUnit"], \[Eta] -> Quantity[\[Eta], "DimensionlessUnit"]|>
 
parameterAssumptions = {Element[bout, Reals], Element[Jout, Reals], 
     Element[e[_], Reals], Element[ea[_], Reals], Element[i[_], Reals], 
     Element[\[Alpha][_], Reals], Element[\[Theta][_], Reals], 
     Element[\[Tau][_], Reals], Element[\[Tau]a[_], Reals], 
     Element[\[CapitalOmega][_], Reals], Ke > 0, Kt > 0, \[CapitalNu] > 0, 
     b >= 0, J >= 0, L >= 0, R >= 0, t >= 0, \[Eta] >= 0}
 
radiansUnits = "Radians"
 
secondsUnits = "Seconds"
 
siAngularPositionUnits = "Radians"
 
siAngularVelocityUnits = "Radians"/"Seconds"
 
siAngularAccelerationUnits = "Radians"/"Seconds"^2
 
siTorqueUnits = ("Meters"*"Newtons")/"Radians"
 
siAngularViscousDragUnits = ("Meters"*"Newtons"*"Seconds")/"Radians"^2
 
siAngularInertialUnits = ("Meters"*"Newtons"*"Seconds"^2)/"Radians"^2
