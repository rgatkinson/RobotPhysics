diffEqns = {Kt*i[t] == \[Tau][t], vg[t] == Ke*Derivative[1][\[Theta]][t], 
     R*i[t] + vg[t] + L*Derivative[1][i][t] == vapp[t], 
     (\[Eta]*\[CapitalNu]^2*\[Tau][t] + \[CapitalNu]*\[Tau]app[t] - 
        (Bafter + B*\[Eta]*\[CapitalNu]^2)*Derivative[1][\[Theta]][t] - 
        (Jafter + J*\[Eta]*\[CapitalNu]^2)*Derivative[2][\[Theta]][t])/
       (\[Eta]*\[CapitalNu]) == 0, \[Tau][t] == \[Tau]after[t]/
       (\[Eta]*\[CapitalNu]), \[Theta]after[t] == \[Theta][t]/\[CapitalNu], 
     Derivative[1][\[Theta]after][t] == Derivative[1][\[Theta]][t]/
       \[CapitalNu], Derivative[2][\[Theta]after][t] == 
      Derivative[2][\[Theta]][t]/\[CapitalNu]}
 
Attributes[Derivative] = {NHoldAll, ReadProtected}
 
\[CapitalDelta]diffEqns = {Kt*i[t] == \[Tau][t], 
     vg[t] == Ke*Derivative[1][\[Theta]][t], 
     R*i[t] + vg[t] + L*Derivative[1][i][t] == 
      vapp0 + \[CapitalDelta]vapp[t], 
     (\[CapitalNu]*(\[Tau]app0 + \[CapitalDelta]\[Tau]app[t]) + 
        \[Eta]*\[CapitalNu]^2*\[Tau][t] - (Bafter + B*\[Eta]*\[CapitalNu]^2)*
         Derivative[1][\[Theta]][t] - (Jafter + J*\[Eta]*\[CapitalNu]^2)*
         Derivative[2][\[Theta]][t])/(\[Eta]*\[CapitalNu]) == 0, 
     \[Tau][t] == \[Tau]after[t]/(\[Eta]*\[CapitalNu]), 
     \[Theta]after[t] == \[Theta][t]/\[CapitalNu], 
     Derivative[1][\[Theta]after][t] == Derivative[1][\[Theta]][t]/
       \[CapitalNu], Derivative[2][\[Theta]after][t] == 
      Derivative[2][\[Theta]][t]/\[CapitalNu]}
 
initialConditions = 
    {i[ss0] == -((-(Bafter*vapp0) - B*vapp0*\[Eta]*\[CapitalNu]^2 + 
         Ke*\[CapitalNu]*\[Tau]app0)/(Bafter*R + Ke*Kt*\[Eta]*
          \[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)), 
     vg[ss0] == -((-(Ke*Kt*vapp0*\[Eta]*\[CapitalNu]^2) - 
         Ke*R*\[CapitalNu]*\[Tau]app0)/(Bafter*R + Ke*Kt*\[Eta]*
          \[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)), 
     \[Theta]after[ss0] == 0, \[Tau][ss0] == 
      (Kt*(Bafter*vapp0 + B*vapp0*\[Eta]*\[CapitalNu]^2 - 
         Ke*\[CapitalNu]*\[Tau]app0))/(Bafter*R + Ke*Kt*\[Eta]*
         \[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2), 
     \[Tau]after[ss0] == (Kt*\[Eta]*\[CapitalNu]*(Bafter*vapp0 + 
         B*vapp0*\[Eta]*\[CapitalNu]^2 - Ke*\[CapitalNu]*\[Tau]app0))/
       (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2), 
     Derivative[1][\[Theta]][ss0] == 
      -((-(Kt*vapp0*\[Eta]*\[CapitalNu]^2) - R*\[CapitalNu]*\[Tau]app0)/
        (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
          \[CapitalNu]^2)), Derivative[1][\[Theta]after][ss0] == 
      -((-(Kt*vapp0*\[Eta]*\[CapitalNu]) - R*\[Tau]app0)/
        (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
          \[CapitalNu]^2)), Derivative[2][\[Theta]after][ss0] == 0, 
     vapp[ss0] == vapp0, \[Tau]app[ss0] == \[Tau]app0, \[Theta][ss0] == 0, 
     Derivative[2][\[Theta]][ss0] == 0, Derivative[1][i][ss0] == 0}
 
\[CapitalDelta]initialConditions = 
    {vg[0] == (Ke*(Kt*vapp0*\[Eta]*\[CapitalNu]^2 + R*\[CapitalNu]*
          \[Tau]app0))/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        B*R*\[Eta]*\[CapitalNu]^2), \[Theta]after[0] == 0, 
     \[Tau][0] == -((-(Bafter*Kt*vapp0) - B*Kt*vapp0*\[Eta]*\[CapitalNu]^2 + 
         Ke*Kt*\[CapitalNu]*\[Tau]app0)/(Bafter*R + Ke*Kt*\[Eta]*
          \[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)), 
     \[Tau]after[0] == (\[Eta]*\[CapitalNu]*(Bafter*Kt*vapp0 + 
         B*Kt*vapp0*\[Eta]*\[CapitalNu]^2 - Ke*Kt*\[CapitalNu]*\[Tau]app0))/
       (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2), 
     Derivative[1][i][0] == \[CapitalDelta]vappConst/L, 
     Derivative[1][\[Theta]after][0] == 
      -((-(Kt*vapp0*\[Eta]*\[CapitalNu]) - R*\[Tau]app0)/
        (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
          \[CapitalNu]^2)), Derivative[2][\[Theta]][0] == 
      (\[CapitalDelta]\[Tau]appConst*\[CapitalNu])/
       (Jafter + J*\[Eta]*\[CapitalNu]^2), Derivative[2][\[Theta]after][0] == 
      \[CapitalDelta]\[Tau]appConst/(Jafter + J*\[Eta]*\[CapitalNu]^2), 
     \[CapitalDelta]vapp[0] == \[CapitalDelta]vappConst, 
     \[CapitalDelta]\[Tau]app[0] == \[CapitalDelta]\[Tau]appConst, 
     \[Theta][0] == 0, i[0] == 
      -((-(Bafter*vapp0) - B*vapp0*\[Eta]*\[CapitalNu]^2 + 
         Ke*\[CapitalNu]*\[Tau]app0)/(Bafter*R + Ke*Kt*\[Eta]*
          \[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)), 
     Derivative[1][\[Theta]][0] == -((-(Kt*vapp0*\[Eta]*\[CapitalNu]^2) - 
         R*\[CapitalNu]*\[Tau]app0)/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         B*R*\[Eta]*\[CapitalNu]^2))}
 
motorPositionModel = TransferFunctionModel[
     {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
        Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
      {{s*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
        s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2)), 
        s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2))}}}, s, 
     SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Theta]", 
       Automatic}]
 
motorVelocityModel = TransferFunctionModel[
     {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
        Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
      {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
           (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
     s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Omega]", 
       Automatic}]
 
motorAccelerationModel = TransferFunctionModel[
     {{{s*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
        Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
      {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
           (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
     s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Alpha]", 
       Automatic}]
 
motorCurrentModel = TransferFunctionModel[
     {{{Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
           Ke*\[Tau]app0), Bafter + Jafter*s + (B + J*s)*\[Eta]*
          \[CapitalNu]^2, -(Ke*\[CapitalNu])}}, 
      {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
           (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
     s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "i", Automatic}]
 
motorEMFModel = TransferFunctionModel[
     {{{Ke*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
        Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
      {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
           (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
     s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "vg", Automatic}]
 
motorTorqueModel = TransferFunctionModel[
     {{{Kt*(Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
            Ke*\[Tau]app0)), Kt*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
           \[CapitalNu]^2), -(Ke*Kt*\[CapitalNu])}}, 
      {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
           (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
     s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Tau]", 
       Automatic}]
 
motorPositionAfterModel = TransferFunctionModel[
     {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*\[CapitalNu], 
        R + L*s}}, {{s*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
        s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2)), 
        s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2))}}}, s, 
     SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Theta]after", 
       Automatic}]
 
motorVelocityAfterModel = TransferFunctionModel[
     {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*\[CapitalNu], 
        R + L*s}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
           (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
     s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Omega]after", 
       Automatic}]
 
motorAccelerationAfterModel = TransferFunctionModel[
     {{{s*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
        Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, 
      {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
           (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
     s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Alpha]after", 
       Automatic}]
 
motorTorqueAfterModel = TransferFunctionModel[
     {{{Kt*\[Eta]*\[CapitalNu]*(Bafter*vapp0 + \[CapitalNu]*
           (B*vapp0*\[Eta]*\[CapitalNu] - Ke*\[Tau]app0)), 
        Kt*\[Eta]*\[CapitalNu]*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
           \[CapitalNu]^2), -(Ke*Kt*\[Eta]*\[CapitalNu]^2)}}, 
      {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
           (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
     s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Tau]after", 
       Automatic}]
