diffEqns = {Kt*i[t] == \[Tau][t], e[t] == Ke*Derivative[1][\[Theta]][t], 
     e[t] + R*i[t] + L*Derivative[1][i][t] == ea[t], 
     (\[Eta]*\[CapitalNu]^2*\[Tau][t] + \[CapitalNu]*\[Tau]a[t] - 
        (bout + b*\[Eta]*\[CapitalNu]^2)*Derivative[1][\[Theta]][t] - 
        (Jout + J*\[Eta]*\[CapitalNu]^2)*Derivative[2][\[Theta]][t])/
       (\[Eta]*\[CapitalNu]) == 0, \[Theta]out[t] == 
      \[Theta][t]/\[CapitalNu], Derivative[1][\[Theta]out][t] == 
      Derivative[1][\[Theta]][t]/\[CapitalNu], 
     Derivative[2][\[Theta]out][t] == Derivative[2][\[Theta]][t]/\[CapitalNu]}
 
Attributes[Derivative] = {NHoldAll, ReadProtected}
 
motorPositionModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
      s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(bout + Jout*s + 
          (b + J*s)*\[Eta]*\[CapitalNu]^2))}, s, SystemsModelLabels -> 
      {{"ea", "\[Tau]a"}, "\[Theta]", Automatic}]
 
motorVelocityModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
      Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(bout + Jout*s + 
         (b + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
      {{"ea", "\[Tau]a"}, "\[Omega]", Automatic}]
 
motorAccelerationModel = TransferFunctionModel[
     {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
      Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(bout + Jout*s + 
         (b + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
      {{"ea", "\[Tau]a"}, "\[Alpha]", Automatic}]
 
motorCurrentModel = TransferFunctionModel[
     {{{bout + Jout*s + (b + J*s)*\[Eta]*\[CapitalNu]^2, 
        -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
       (R + L*s)*(bout + Jout*s + (b + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
     SystemsModelLabels -> {{"ea", "\[Tau]a"}, "i", Automatic}]
 
motorEMFModel = TransferFunctionModel[
     {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
      Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(bout + Jout*s + 
         (b + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
      {{"ea", "\[Tau]a"}, "emf", Automatic}]
 
motorTorqueModel = TransferFunctionModel[
     {{{Kt*(bout + Jout*s + (b + J*s)*\[Eta]*\[CapitalNu]^2), 
        -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
       (R + L*s)*(bout + Jout*s + (b + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
     SystemsModelLabels -> {{"ea", "\[Tau]a"}, "\[Tau]", Automatic}]
 
motorPositionOutModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        (R + L*s)*(bout + Jout*s + (b + J*s)*\[Eta]*\[CapitalNu]^2))}, s, 
     SystemsModelLabels -> {{"ea", "\[Tau]a"}, "\[Theta]out", Automatic}]
 
motorVelocityOutModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
       (R + L*s)*(bout + Jout*s + (b + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
     SystemsModelLabels -> {{"ea", "\[Tau]a"}, "\[CapitalOmega]out", 
       Automatic}]
 
motorAccelerationOutModel = TransferFunctionModel[
     {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, 
      Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(bout + Jout*s + 
         (b + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
      {{"ea", "\[Tau]a"}, "\[Alpha]out", Automatic}]
