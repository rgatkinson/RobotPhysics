motorPosition = Function[{vbatActual$, \[Tau]autoActual$}, 
     Module[{exprT$}, exprT$ = expr$24069 /. {vbat$24069 -> vbatActual$, 
          \[Tau]auto$24069 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$24069 -> tActual$]]]
 
Attributes[vbatActual$] = {Temporary}
 
Attributes[\[Tau]autoActual$] = {Temporary}
 
Attributes[exprT$] = {Temporary}
 
Attributes[expr$24069] = {Temporary}
 
expr$24069 = {Integrate[Kt*\[Eta]*\[CapitalNu]^2*
        ((Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)^
          (-1) - (-(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                     2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                   J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                      \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                     B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                     B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^
                    2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*L) + 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*L - 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*Jout*R + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*Jout*R - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*L*\[Eta]*
            \[CapitalNu]^2 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*L*\[Eta]*
            \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*J*R*\[Eta]*
            \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*J*R*\[Eta]*
            \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]21)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2])/(2*(Bout*R + Ke*Kt*\[Eta]*
             \[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
             (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]))*vbat$24069[t$24069 - \[Tau]21], 
       {\[Tau]21, 0, t$24069}] + Integrate[\[CapitalNu]*
        (R/(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) - 
         (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*L*R - 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*L*R - 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*Jout*R^2 + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*Jout*R^2 + 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*Ke*Kt*L*
            \[Eta]*\[CapitalNu]^2 - 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*Ke*Kt*L*
            \[Eta]*\[CapitalNu]^2 + 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*L*R*\[Eta]*
            \[CapitalNu]^2 - B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*L*R*\[Eta]*
            \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*J*R^2*
            \[Eta]*\[CapitalNu]^2 + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*J*R^2*
            \[Eta]*\[CapitalNu]^2 + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*R*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]22)*R*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2])/(2*(Bout*R + Ke*Kt*\[Eta]*
             \[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
             (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]))*\[Tau]auto$24069[t$24069 - \[Tau]22], 
       {\[Tau]22, 0, t$24069}]}
 
Attributes[vbat$24069] = {Temporary}
 
Attributes[t$24069] = {Temporary}
 
Attributes[\[Tau]auto$24069] = {Temporary}
 
Attributes[tActual$] = {Temporary}
 
motorVelocity = Function[{vbatActual$, \[Tau]autoActual$}, 
     Module[{exprT$}, exprT$ = expr$24900 /. {vbat$24900 -> vbatActual$, 
          \[Tau]auto$24900 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$24900 -> tActual$]]]
 
Attributes[expr$24900] = {Temporary}
 
expr$24900 = 
    {Integrate[-(((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]23) - 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]23))*Kt*\[Eta]*\[CapitalNu]^2*
          vbat$24900[t$24900 - \[Tau]23])/
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]23, 0, t$24900}] + 
      Integrate[(\[CapitalNu]*
         (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]24)*L - 
          Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]24)*L - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]24)*Jout*R + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]24)*Jout*R + 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]24)*L*\[Eta]*\[CapitalNu]^2 - 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]24)*L*\[Eta]*\[CapitalNu]^2 - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]24)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]24)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]24)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]24)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*\[Tau]auto$24900[
          t$24900 - \[Tau]24])/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]24, 0, t$24900}]}
 
Attributes[vbat$24900] = {Temporary}
 
Attributes[t$24900] = {Temporary}
 
Attributes[\[Tau]auto$24900] = {Temporary}
 
motorAcceleration = Function[{vbatActual$, \[Tau]autoActual$}, 
     Module[{exprT$}, exprT$ = expr$26846 /. {vbat$26846 -> vbatActual$, 
          \[Tau]auto$26846 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$26846 -> tActual$]]]
 
Attributes[expr$26846] = {Temporary}
 
expr$26846 = {Integrate[(Kt*\[Eta]*\[CapitalNu]^2*
         (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]25)*L - 
          Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]25)*L + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]25)*Jout*R - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]25)*Jout*R + 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]25)*L*\[Eta]*\[CapitalNu]^2 - 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]25)*L*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]25)*J*R*\[Eta]*\[CapitalNu]^2 - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]25)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]25)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]25)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*
         vbat$26846[t$26846 - \[Tau]25])/
        (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]25, 0, t$26846}] + 
      Integrate[\[CapitalNu]*
        ((-(Bout^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                     2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                   J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                      \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                     B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                     B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^
                    2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*L) + 
           Bout^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*L + 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*Jout*R - 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*Jout*R + 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*Jout*Ke*Kt*
            \[Eta]*\[CapitalNu]^2 - 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*Jout*Ke*Kt*
            \[Eta]*\[CapitalNu]^2 - 2*B*Bout*
            E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*L*\[Eta]*
            \[CapitalNu]^2 + 2*B*Bout*E^((-Bout/(2*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
                (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                 \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
               Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                    Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^
                      2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)))*\[Tau]26)*L*\[Eta]*\[CapitalNu]^2 + 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*J*R*\[Eta]*
            \[CapitalNu]^2 - Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*J*R*\[Eta]*
            \[CapitalNu]^2 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*Jout*R*
            \[Eta]*\[CapitalNu]^2 - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*Jout*R*
            \[Eta]*\[CapitalNu]^2 + 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*J*Ke*Kt*
            \[Eta]^2*\[CapitalNu]^4 - 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*J*Ke*Kt*
            \[Eta]^2*\[CapitalNu]^4 - 
           B^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*L*\[Eta]^2*
            \[CapitalNu]^4 + B^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*L*\[Eta]^2*
            \[CapitalNu]^4 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*J*R*
            \[Eta]^2*\[CapitalNu]^4 - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*J*R*
            \[Eta]^2*\[CapitalNu]^4 - 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] - 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*\[Eta]*
            \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2)^2] - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*\[Eta]*
            \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2)^2])/
          (2*(Jout + J*\[Eta]*\[CapitalNu]^2)^2*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
             (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]) + DiracDelta[\[Tau]26]/
          (Jout + J*\[Eta]*\[CapitalNu]^2))*\[Tau]auto$26846[
         t$26846 - \[Tau]26], {\[Tau]26, 0, t$26846}]}
 
Attributes[vbat$26846] = {Temporary}
 
Attributes[t$26846] = {Temporary}
 
Attributes[\[Tau]auto$26846] = {Temporary}
 
motorCurrent = Function[{vbatActual$, \[Tau]autoActual$}, 
     Module[{exprT$}, exprT$ = expr$100914 /. {vbat$100914 -> vbatActual$, 
          \[Tau]auto$100914 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$100914 -> tActual$]]]
 
Attributes[expr$100914] = {Temporary}
 
expr$100914 = 
    {Integrate[((-(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]35)*L) + 
          Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]35)*L + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]35)*Jout*R - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]35)*Jout*R - 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]35)*L*\[Eta]*\[CapitalNu]^2 + 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]35)*L*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]35)*J*R*\[Eta]*\[CapitalNu]^2 - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]35)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]35)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]35)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*vbat$100914[
          t$100914 - \[Tau]35])/
        (2*L*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
       {\[Tau]35, 0, t$100914}] + Integrate[
       ((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]36) - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]36))*Ke*
         \[CapitalNu]*\[Tau]auto$100914[t$100914 - \[Tau]36])/
        Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2], {\[Tau]36, 0, t$100914}]}
 
Attributes[vbat$100914] = {Temporary}
 
Attributes[t$100914] = {Temporary}
 
Attributes[\[Tau]auto$100914] = {Temporary}
 
motorEMF = Function[{vbatActual$, \[Tau]autoActual$}, 
     Module[{exprT$}, exprT$ = expr$102877 /. {vbat$102877 -> vbatActual$, 
          \[Tau]auto$102877 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$102877 -> tActual$]]]
 
Attributes[expr$102877] = {Temporary}
 
expr$102877 = 
    {Integrate[-(((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]37) - 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]37))*Ke*Kt*\[Eta]*\[CapitalNu]^2*
          vbat$102877[t$102877 - \[Tau]37])/
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]37, 0, t$102877}] + 
      Integrate[(Ke*\[CapitalNu]*
         (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]38)*L - 
          Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]38)*L - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]38)*Jout*R + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]38)*Jout*R + 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]38)*L*\[Eta]*\[CapitalNu]^2 - 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]38)*L*\[Eta]*\[CapitalNu]^2 - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]38)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]38)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]38)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]38)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*\[Tau]auto$102877[
          t$102877 - \[Tau]38])/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]38, 0, t$102877}]}
 
Attributes[vbat$102877] = {Temporary}
 
Attributes[t$102877] = {Temporary}
 
Attributes[\[Tau]auto$102877] = {Temporary}
 
motorTorque = Function[{vbatActual$, \[Tau]autoActual$}, 
     Module[{exprT$}, exprT$ = expr$104840 /. {vbat$104840 -> vbatActual$, 
          \[Tau]auto$104840 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$104840 -> tActual$]]]
 
Attributes[expr$104840] = {Temporary}
 
expr$104840 = 
    {Integrate[(Kt*(-(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]39)*L) + 
          Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]39)*L + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]39)*Jout*R - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]39)*Jout*R - 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]39)*L*\[Eta]*\[CapitalNu]^2 + 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]39)*L*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]39)*J*R*\[Eta]*\[CapitalNu]^2 - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]39)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]39)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]39)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*vbat$104840[
          t$104840 - \[Tau]39])/
        (2*L*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
       {\[Tau]39, 0, t$104840}] + Integrate[
       ((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]40) - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]40))*Ke*Kt*
         \[CapitalNu]*\[Tau]auto$104840[t$104840 - \[Tau]40])/
        Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2], {\[Tau]40, 0, t$104840}]}
 
Attributes[vbat$104840] = {Temporary}
 
Attributes[t$104840] = {Temporary}
 
Attributes[\[Tau]auto$104840] = {Temporary}
 
motorPositionOut = Function[{vbatActual$, \[Tau]autoActual$}, 
     Module[{exprT$}, exprT$ = expr$106804 /. {vbat$106804 -> vbatActual$, 
          \[Tau]auto$106804 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$106804 -> tActual$]]]
 
Attributes[expr$106804] = {Temporary}
 
expr$106804 = {Integrate[Kt*\[Eta]*\[CapitalNu]*
        ((Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)^
          (-1) - (-(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                     2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                   J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                      \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                     B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                     B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^
                    2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*L) + 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*L - 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*Jout*R + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*Jout*R - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*L*\[Eta]*
            \[CapitalNu]^2 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*L*\[Eta]*
            \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*J*R*\[Eta]*
            \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*J*R*\[Eta]*
            \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2])/(2*(Bout*R + Ke*Kt*\[Eta]*
             \[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
             (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]))*vbat$106804[t$106804 - \[Tau]41], 
       {\[Tau]41, 0, t$106804}] + Integrate[
       (R/(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) - 
         (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*L*R - 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*L*R - 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*Jout*R^2 + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*Jout*R^2 + 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*Ke*Kt*L*
            \[Eta]*\[CapitalNu]^2 - 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*Ke*Kt*L*
            \[Eta]*\[CapitalNu]^2 + 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*L*R*\[Eta]*
            \[CapitalNu]^2 - B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*L*R*\[Eta]*
            \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*J*R^2*
            \[Eta]*\[CapitalNu]^2 + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*J*R^2*
            \[Eta]*\[CapitalNu]^2 + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*R*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] + 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]42)*R*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2])/(2*(Bout*R + Ke*Kt*\[Eta]*
             \[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
             (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]))*\[Tau]auto$106804[t$106804 - \[Tau]42], 
       {\[Tau]42, 0, t$106804}]}
 
Attributes[vbat$106804] = {Temporary}
 
Attributes[t$106804] = {Temporary}
 
Attributes[\[Tau]auto$106804] = {Temporary}
 
motorVelocityOut = Function[{vbatActual$, \[Tau]autoActual$}, 
     Module[{exprT$}, exprT$ = expr$107634 /. {vbat$107634 -> vbatActual$, 
          \[Tau]auto$107634 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$107634 -> tActual$]]]
 
Attributes[expr$107634] = {Temporary}
 
expr$107634 = 
    {Integrate[-(((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]43) - 
           E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]43))*Kt*\[Eta]*\[CapitalNu]*
          vbat$107634[t$107634 - \[Tau]43])/
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]43, 0, t$107634}] + 
      Integrate[((Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]44)*L - 
          Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]44)*L - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]44)*Jout*R + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]44)*Jout*R + 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]44)*L*\[Eta]*\[CapitalNu]^2 - 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]44)*L*\[Eta]*\[CapitalNu]^2 - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]44)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]44)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]44)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]44)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*\[Tau]auto$107634[
          t$107634 - \[Tau]44])/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]44, 0, t$107634}]}
 
Attributes[vbat$107634] = {Temporary}
 
Attributes[t$107634] = {Temporary}
 
Attributes[\[Tau]auto$107634] = {Temporary}
 
motorAccelerationOut = Function[{vbatActual$, \[Tau]autoActual$}, 
     Module[{exprT$}, exprT$ = expr$109623 /. {vbat$109623 -> vbatActual$, 
          \[Tau]auto$109623 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$109623 -> tActual$]]]
 
Attributes[expr$109623] = {Temporary}
 
expr$109623 = {Integrate[(Kt*\[Eta]*\[CapitalNu]*
         (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]45)*L - 
          Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]45)*L + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]45)*Jout*R - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]45)*Jout*R + 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]45)*L*\[Eta]*\[CapitalNu]^2 - 
          B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]45)*L*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]45)*J*R*\[Eta]*\[CapitalNu]^2 - 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]45)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]45)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
                 (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]45)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*vbat$109623[
          t$109623 - \[Tau]45])/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]45, 0, t$109623}] + 
      Integrate[((-(Bout^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                     2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                   J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                      \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                     B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                     B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^
                    2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*L) + 
           Bout^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*L + 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*Jout*R - 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*Jout*R + 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*Jout*Ke*Kt*
            \[Eta]*\[CapitalNu]^2 - 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*Jout*Ke*Kt*
            \[Eta]*\[CapitalNu]^2 - 2*B*Bout*
            E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*L*\[Eta]*
            \[CapitalNu]^2 + 2*B*Bout*E^((-Bout/(2*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)) - (B*\[Eta]*\[CapitalNu]^2)/
                (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                 \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
               Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                    Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^
                      2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)))*\[Tau]46)*L*\[Eta]*\[CapitalNu]^2 + 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*J*R*\[Eta]*
            \[CapitalNu]^2 - Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*J*R*\[Eta]*
            \[CapitalNu]^2 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*Jout*R*
            \[Eta]*\[CapitalNu]^2 - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*Jout*R*
            \[Eta]*\[CapitalNu]^2 + 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*J*Ke*Kt*
            \[Eta]^2*\[CapitalNu]^4 - 
           2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*J*Ke*Kt*
            \[Eta]^2*\[CapitalNu]^4 - 
           B^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*L*\[Eta]^2*
            \[CapitalNu]^4 + B^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*L*\[Eta]^2*
            \[CapitalNu]^4 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                    B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*J*R*
            \[Eta]^2*\[CapitalNu]^4 - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*J*R*
            \[Eta]^2*\[CapitalNu]^4 - 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] - 
           Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*\[Eta]*
            \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2)^2] - 
           B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (B*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                     \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]46)*\[Eta]*
            \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2)^2])/
          (2*(Jout + J*\[Eta]*\[CapitalNu]^2)^2*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
             (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]) + DiracDelta[\[Tau]46]/
          (Jout + J*\[Eta]*\[CapitalNu]^2))*\[Tau]auto$109623[
         t$109623 - \[Tau]46], {\[Tau]46, 0, t$109623}]}
 
Attributes[vbat$109623] = {Temporary}
 
Attributes[t$109623] = {Temporary}
 
Attributes[\[Tau]auto$109623] = {Temporary}
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Bout*(R + L*s) + 
        Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
         \[CapitalNu]^2}, s, SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, 
        "\[Omega]out", Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vbatActual$, \[Tau]autoActual$}, Module[{exprT$}, 
      exprT$ = expr$107634 /. {vbat$107634 -> vbatActual$, 
          \[Tau]auto$107634 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$107634 -> tActual$]]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + 
        s*(R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
      SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "\[Theta]out", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vbatActual$, \[Tau]autoActual$}, Module[{exprT$}, 
      exprT$ = expr$106804 /. {vbat$106804 -> vbatActual$, 
          \[Tau]auto$106804 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$106804 -> tActual$]]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, Bout*(R + L*s) + 
        Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
         \[CapitalNu]^2}, s, SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, 
        "\[Alpha]out", Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vbatActual$, \[Tau]autoActual$}, Module[{exprT$}, 
      exprT$ = expr$109623 /. {vbat$109623 -> vbatActual$, 
          \[Tau]auto$109623 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$109623 -> tActual$]]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
       Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
         \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
       {{"vbat", "\[Tau]auto"}, "\[Omega]", Automatic}], 
     makeTimeDomainFunctionConvolve] = 
    Function[{vbatActual$, \[Tau]autoActual$}, Module[{exprT$}, 
      exprT$ = expr$24900 /. {vbat$24900 -> vbatActual$, \[Tau]auto$24900 -> 
           \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$24900 -> tActual$]]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
       Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + s*(R + L*s)*(Bout + Jout*s + 
          (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
       {{"vbat", "\[Tau]auto"}, "\[Theta]", Automatic}], 
     makeTimeDomainFunctionConvolve] = 
    Function[{vbatActual$, \[Tau]autoActual$}, Module[{exprT$}, 
      exprT$ = expr$24069 /. {vbat$24069 -> vbatActual$, \[Tau]auto$24069 -> 
           \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$24069 -> tActual$]]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
       Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
         \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
       {{"vbat", "\[Tau]auto"}, "vg", Automatic}], 
     makeTimeDomainFunctionConvolve] = 
    Function[{vbatActual$, \[Tau]autoActual$}, Module[{exprT$}, 
      exprT$ = expr$102877 /. {vbat$102877 -> vbatActual$, 
          \[Tau]auto$102877 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$102877 -> tActual$]]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
       Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
         \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
       {{"vbat", "\[Tau]auto"}, "\[Alpha]", Automatic}], 
     makeTimeDomainFunctionConvolve] = 
    Function[{vbatActual$, \[Tau]autoActual$}, Module[{exprT$}, 
      exprT$ = expr$26846 /. {vbat$26846 -> vbatActual$, \[Tau]auto$26846 -> 
           \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$26846 -> tActual$]]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2, 
         -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
      SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "i", Automatic}], 
     makeTimeDomainFunctionConvolve] = 
    Function[{vbatActual$, \[Tau]autoActual$}, Module[{exprT$}, 
      exprT$ = expr$100914 /. {vbat$100914 -> vbatActual$, 
          \[Tau]auto$100914 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$100914 -> tActual$]]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
         -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
      SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "\[Tau]", Automatic}], 
     makeTimeDomainFunctionConvolve] = 
    Function[{vbatActual$, \[Tau]autoActual$}, Module[{exprT$}, 
      exprT$ = expr$104840 /. {vbat$104840 -> vbatActual$, 
          \[Tau]auto$104840 -> \[Tau]autoActual$}; Function[{tActual$}, 
        exprT$ /. t$104840 -> tActual$]]]
 
makeMotorTimeDomainFunction[model_] := makeMotorTimeDomainFunction[model, 
     makeTimeDomainFunctionConvolve]
 
makeMotorTimeDomainFunction[model_, builder_] := 
    makeMotorTimeDomainFunction[model, builder] = 
     Module[{vbat, \[Tau]auto, t, expr}, 
      expr = builder[model, {vbat[#1] & , \[Tau]auto[#1] & }][t]; 
       Function[{vbatActual, \[Tau]autoActual}, Module[{exprT}, 
         exprT = expr /. {vbat -> vbatActual, \[Tau]auto -> 
              \[Tau]autoActual}; Function[{tActual}, exprT /. t -> tActual]]]]
 
makeTimeDomainFunctionConvolve[model_, tInputFunctions_] := 
    Module[{s, \[Tau], modelExpr}, 
     modelExpr = InverseLaplaceTransform[model[s], s, \[Tau]][[1]]; 
      Function[{t}, Module[{convolutions}, 
        convolutions = (convolve[modelExpr[[#1]], tInputFunctions[[#1]][
              \[Tau]], \[Tau], t] & ) /@ Range[1, Length[tInputFunctions]]; 
         {Total[convolutions]}]]]
 
convolve[fExpr_, gExpr_, exprVar_, t_] := Module[{\[Tau] = Unique["\[Tau]"]}, 
     Assuming[Union[{t >= 0}, parameterAssumptions], 
      Integrate[(fExpr /. exprVar -> \[Tau])*(gExpr /. 
         exprVar -> t - \[Tau]), {\[Tau], 0, t}]]]
 
parameterAssumptions = {Element[Bout, Reals], Element[constvbat, Reals], 
     Element[const\[Tau]auto, Reals], Element[Jout, Reals], 
     Element[i[_], Reals], Element[vbat[_], Reals], Element[vg[_], Reals], 
     Element[\[Alpha][_], Reals], Element[\[Theta][_], Reals], 
     Element[\[Tau][_], Reals], Element[\[Tau]auto[_], Reals], 
     Element[\[Omega][_], Reals], Ke > 0, Kt > 0, L > 0, R > 0, \[Eta] > 0, 
     \[CapitalNu] > 0, B >= 0, J >= 0, t >= 0}
