diffEqns = {Kt*i[t] == \[Tau][t], vg[t] == Ke*Derivative[1][\[Theta]][t], 
     R*i[t] + vg[t] + L*Derivative[1][i][t] == vapp[t], 
     (\[Eta]*\[CapitalNu]^2*\[Tau][t] + \[CapitalNu]*\[Tau]appafter[t] - 
        (Bafter + B*\[Eta]*\[CapitalNu]^2)*Derivative[1][\[Theta]][t] - 
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*Derivative[2][\[Theta]][t])/
       (\[Eta]*\[CapitalNu]) == 0, \[Theta]after[t] == 
      \[Theta][t]/\[CapitalNu], Derivative[1][\[Theta]after][t] == 
      Derivative[1][\[Theta]][t]/\[CapitalNu], 
     Derivative[2][\[Theta]after][t] == Derivative[2][\[Theta]][t]/
       \[CapitalNu]}
 
Attributes[Derivative] = {NHoldAll, ReadProtected}
 
motorPositionModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
      s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
          (B + J*s)*\[Eta]*\[CapitalNu]^2))}, s, SystemsModelLabels -> 
      {{"vapp", "\[Tau]appafter"}, "\[Theta]", Automatic}]
 
motorVelocityModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
      Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
         (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
      {{"vapp", "\[Tau]appafter"}, "\[Omega]", Automatic}]
 
motorAccelerationModel = TransferFunctionModel[
     {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
      Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
         (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
      {{"vapp", "\[Tau]appafter"}, "\[Alpha]", Automatic}]
 
motorCurrentModel = TransferFunctionModel[
     {{{Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2, 
        -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
       (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
     SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "i", Automatic}]
 
motorEMFModel = TransferFunctionModel[
     {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
      Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
         (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
      {{"vapp", "\[Tau]appafter"}, "vg", Automatic}]
 
motorTorqueModel = TransferFunctionModel[
     {{{Kt*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
        -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
       (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
     SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "\[Tau]", Automatic}]
 
motorPositionOutModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2))}, s, 
     SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "\[Theta]after", 
       Automatic}]
 
motorVelocityOutModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
       (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
     SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "\[Omega]out", 
       Automatic}]
 
motorAccelerationOutModel = TransferFunctionModel[
     {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, 
      Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
         (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
      {{"vapp", "\[Tau]appafter"}, "\[Alpha]out", Automatic}]
