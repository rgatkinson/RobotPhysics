ss = {ssPos -> Indeterminate, ssVel -> 
      (\[CapitalNu]*(const\[Tau]out*R + ea*Kt*\[Eta]*\[CapitalNu]))/
       (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2), 
     ssAcc -> 0, ssEmf -> (Ke*\[CapitalNu]*(const\[Tau]out*R + 
         ea*Kt*\[Eta]*\[CapitalNu]))/(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        b*R*\[Eta]*\[CapitalNu]^2), 
     ssCur -> (bout*ea - const\[Tau]out*Ke*\[CapitalNu] + 
        b*ea*\[Eta]*\[CapitalNu]^2)/(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        b*R*\[Eta]*\[CapitalNu]^2), 
     ssTor -> (Kt*(bout*ea - const\[Tau]out*Ke*\[CapitalNu] + 
         b*ea*\[Eta]*\[CapitalNu]^2))/(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        b*R*\[Eta]*\[CapitalNu]^2)}
