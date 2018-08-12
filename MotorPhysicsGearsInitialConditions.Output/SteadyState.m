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
       (Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2)}
 
simpleSs = Text[TraditionalForm[ColumnForm[{ssPos -> Indeterminate, 
        ssVel -> (Kt*vbat)/(Ke*Kt + B*R), ssAcc -> 0, 
        ssEmf -> (Ke*Kt*vbat)/(Ke*Kt + B*R), ssCur -> (B*vbat)/(Ke*Kt + B*R), 
        ssTor -> (B*Kt*vbat)/(Ke*Kt + B*R)}]], BaseStyle -> {FontSize -> 15}]
