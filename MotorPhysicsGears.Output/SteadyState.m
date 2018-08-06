ss = {ssPos -> Indeterminate, ssVel -> 
      (\[CapitalNu]*(const\[Tau]auto*R + constvbat*Kt*\[Eta]*\[CapitalNu]))/
       (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2), 
     ssAcc -> 0, ssEmf -> (Ke*\[CapitalNu]*(const\[Tau]auto*R + 
         constvbat*Kt*\[Eta]*\[CapitalNu]))/(Bout*R + 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2), 
     ssCur -> (Bout*constvbat - const\[Tau]auto*Ke*\[CapitalNu] + 
        B*constvbat*\[Eta]*\[CapitalNu]^2)/(Bout*R + 
        Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2), 
     ssTor -> (Kt*(Bout*constvbat - const\[Tau]auto*Ke*\[CapitalNu] + 
         B*constvbat*\[Eta]*\[CapitalNu]^2))/
       (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)}
