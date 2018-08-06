diffEqns = {Kt*i[t] == \[Tau][t], vg[t] == Ke*Derivative[1][\[Theta]][t], 
     R*i[t] + vg[t] + L*Derivative[1][i][t] == vbat[t], 
     \[CapitalNu]*(\[Eta]*\[CapitalNu]*\[Tau][t] + \[Tau]auto[t]) == 
      (Bout + B*\[Eta]*\[CapitalNu]^2)*Derivative[1][\[Theta]][t] + 
       (Jout + J*\[Eta]*\[CapitalNu]^2)*Derivative[2][\[Theta]][t], 
     \[Theta][t] == \[CapitalNu]*\[Theta]out[t], 
     Derivative[1][\[Theta]][t] == \[CapitalNu]*Derivative[1][\[Theta]out][
        t], Derivative[2][\[Theta]][t] == \[CapitalNu]*
       Derivative[2][\[Theta]out][t]}
 
Attributes[Derivative] = {NHoldAll, ReadProtected}
 
motorPositionModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
      Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + s*(R + L*s)*(Bout + Jout*s + 
         (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
      {{"vbat", "\[Tau]auto"}, "\[Theta]", Automatic}]
 
motorVelocityModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
      Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
        \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
      {{"vbat", "\[Tau]auto"}, "\[Omega]", Automatic}]
 
motorAccelerationModel = TransferFunctionModel[
     {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
      Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
        \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
      {{"vbat", "\[Tau]auto"}, "\[Alpha]", Automatic}]
 
motorCurrentModel = TransferFunctionModel[
     {{{Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2, 
        -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
       (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
     SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "i", Automatic}]
 
motorEMFModel = TransferFunctionModel[
     {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
      Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
        \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
      {{"vbat", "\[Tau]auto"}, "vg", Automatic}]
 
motorTorqueModel = TransferFunctionModel[
     {{{Kt*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
        -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
       (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
     SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "\[Tau]", Automatic}]
 
motorPositionOutModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + 
       s*(R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
     SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "\[Theta]out", Automatic}]
 
motorVelocityOutModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Bout*(R + L*s) + 
       Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
        \[CapitalNu]^2}, s, SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, 
       "\[Omega]out", Automatic}]
 
motorAccelerationOutModel = TransferFunctionModel[
     {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, Bout*(R + L*s) + 
       Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
        \[CapitalNu]^2}, s, SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, 
       "\[Alpha]out", Automatic}]
