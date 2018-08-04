motorPosition = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$18365 /. {ea$18365 -> eaActual$, 
          \[Tau]a$18365 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$18365 -> tActual$]]]
 
Attributes[eaActual$] = {Temporary}
 
Attributes[\[Tau]aActual$] = {Temporary}
 
Attributes[exprT$] = {Temporary}
 
Attributes[expr$18365] = {Temporary}
 
expr$18365 = {Integrate[Kt*\[Eta]*\[CapitalNu]^2*
        ((bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2)^
          (-1) - (-(bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                     2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                   J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                      \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                     b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                     b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^
                    2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*L) + 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*L - 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*Jout*R + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*Jout*R - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*L*\[Eta]*
            \[CapitalNu]^2 + b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*L*\[Eta]*
            \[CapitalNu]^2 - E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*J*R*\[Eta]*
            \[CapitalNu]^2 + E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*J*R*\[Eta]*
            \[CapitalNu]^2 + E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]26)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2])/(2*(bout*R + Ke*Kt*\[Eta]*
             \[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2)*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
             (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]))*ea21[t23 - \[Tau]26], 
       {\[Tau]26, 0, t23}] + Integrate[\[CapitalNu]*
        (R/(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
            \[CapitalNu]^2) - 
         (bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*L*R - 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*L*R - 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*Jout*R^2 + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*Jout*R^2 + 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*Ke*Kt*L*
            \[Eta]*\[CapitalNu]^2 - 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*Ke*Kt*L*
            \[Eta]*\[CapitalNu]^2 + 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*L*R*\[Eta]*
            \[CapitalNu]^2 - b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*L*R*\[Eta]*
            \[CapitalNu]^2 - E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*J*R^2*
            \[Eta]*\[CapitalNu]^2 + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*J*R^2*
            \[Eta]*\[CapitalNu]^2 + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*R*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]27)*R*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2])/(2*(bout*R + Ke*Kt*\[Eta]*
             \[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2)*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
             (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]))*\[Tau]a22[t23 - \[Tau]27], 
       {\[Tau]27, 0, t23}]}
 
Attributes[ea$18365] = {Temporary}
 
ea$18365 = ea21
 
Attributes[\[Tau]a$18365] = {Temporary}
 
\[Tau]a$18365 = \[Tau]a22
 
Attributes[tActual$] = {Temporary}
 
Attributes[t$18365] = {Temporary}
 
t$18365 = t23
 
motorVelocity = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$19153 /. {ea$19153 -> eaActual$, 
          \[Tau]a$19153 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$19153 -> tActual$]]]
 
Attributes[expr$19153] = {Temporary}
 
expr$19153 = 
    {Integrate[-(((E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]33) - 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]33))*Kt*\[Eta]*\[CapitalNu]^2*
          ea28[t30 - \[Tau]33])/Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
              \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
       {\[Tau]33, 0, t30}] + Integrate[
       (\[CapitalNu]*(bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]34)*L - 
          bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]34)*L - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]34)*Jout*R + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]34)*Jout*R + 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]34)*L*\[Eta]*\[CapitalNu]^2 - 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]34)*L*\[Eta]*\[CapitalNu]^2 - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]34)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]34)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]34)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]34)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*
         \[Tau]a29[t30 - \[Tau]34])/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
           (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]34, 0, t30}]}
 
Attributes[ea$19153] = {Temporary}
 
ea$19153 = ea28
 
Attributes[\[Tau]a$19153] = {Temporary}
 
\[Tau]a$19153 = \[Tau]a29
 
Attributes[t$19153] = {Temporary}
 
t$19153 = t30
 
motorAcceleration = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$21069 /. {ea$21069 -> eaActual$, 
          \[Tau]a$21069 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$21069 -> tActual$]]]
 
Attributes[expr$21069] = {Temporary}
 
expr$21069 = {Integrate[(Kt*\[Eta]*\[CapitalNu]^2*
         (bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]40)*L - 
          bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]40)*L + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]40)*Jout*R - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]40)*Jout*R + 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]40)*L*\[Eta]*\[CapitalNu]^2 - 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]40)*L*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]40)*J*R*\[Eta]*\[CapitalNu]^2 - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]40)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]40)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]40)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*ea35[t37 - \[Tau]40])/
        (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
           (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]40, 0, t37}] + 
      Integrate[\[CapitalNu]*
        ((-(bout^2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                     2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                   J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                      \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                     b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                     b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^
                    2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*L) + 
           bout^2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*L + 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*Jout*R - 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*Jout*R + 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*Jout*Ke*Kt*
            \[Eta]*\[CapitalNu]^2 - 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*Jout*Ke*Kt*
            \[Eta]*\[CapitalNu]^2 - 2*b*bout*
            E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*L*\[Eta]*
            \[CapitalNu]^2 + 2*b*bout*E^((-bout/(2*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
                (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                 \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
               Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                    Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^
                      2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)))*\[Tau]41)*L*\[Eta]*\[CapitalNu]^2 + 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*J*R*\[Eta]*
            \[CapitalNu]^2 - bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*J*R*\[Eta]*
            \[CapitalNu]^2 + b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*Jout*R*
            \[Eta]*\[CapitalNu]^2 - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*Jout*R*
            \[Eta]*\[CapitalNu]^2 + 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*J*Ke*Kt*
            \[Eta]^2*\[CapitalNu]^4 - 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*J*Ke*Kt*
            \[Eta]^2*\[CapitalNu]^4 - 
           b^2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*L*\[Eta]^2*
            \[CapitalNu]^4 + b^2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*L*\[Eta]^2*
            \[CapitalNu]^4 + b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*J*R*
            \[Eta]^2*\[CapitalNu]^4 - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*J*R*
            \[Eta]^2*\[CapitalNu]^4 - 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] - 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*\[Eta]*
            \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                 \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2)^2] - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]41)*\[Eta]*
            \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                 \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2)^2])/
          (2*(Jout + J*\[Eta]*\[CapitalNu]^2)^2*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
             (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]) + DiracDelta[\[Tau]41]/
          (Jout + J*\[Eta]*\[CapitalNu]^2))*\[Tau]a36[t37 - \[Tau]41], 
       {\[Tau]41, 0, t37}]}
 
Attributes[ea$21069] = {Temporary}
 
ea$21069 = ea35
 
Attributes[\[Tau]a$21069] = {Temporary}
 
\[Tau]a$21069 = \[Tau]a36
 
Attributes[t$21069] = {Temporary}
 
t$21069 = t37
 
motorCurrent = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$105022 /. {ea$105022 -> eaActual$, 
          \[Tau]a$105022 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$105022 -> tActual$]]]
 
Attributes[expr$105022] = {Temporary}
 
expr$105022 = 
    {Integrate[((-(bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]55)*L) + 
          bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]55)*L + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]55)*Jout*R - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]55)*Jout*R - 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]55)*L*\[Eta]*\[CapitalNu]^2 + 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]55)*L*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]55)*J*R*\[Eta]*\[CapitalNu]^2 - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]55)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]55)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]55)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*ea50[t52 - \[Tau]55])/
        (2*L*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
              \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
       {\[Tau]55, 0, t52}] + Integrate[
       ((E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                   \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]56) - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                   \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]56))*Ke*
         \[CapitalNu]*\[Tau]a51[t52 - \[Tau]56])/
        Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
          (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2], {\[Tau]56, 0, t52}]}
 
Attributes[ea$105022] = {Temporary}
 
ea$105022 = ea50
 
Attributes[\[Tau]a$105022] = {Temporary}
 
\[Tau]a$105022 = \[Tau]a51
 
Attributes[t$105022] = {Temporary}
 
t$105022 = t52
 
motorEMF = Function[{eaActual$, \[Tau]aActual$}, Module[{exprT$}, 
      exprT$ = expr$106953 /. {ea$106953 -> eaActual$, \[Tau]a$106953 -> 
           \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$106953 -> tActual$]]]
 
Attributes[expr$106953] = {Temporary}
 
expr$106953 = 
    {Integrate[-(((E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]62) - 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]62))*Ke*Kt*\[Eta]*\[CapitalNu]^2*
          ea57[t59 - \[Tau]62])/Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
              \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
       {\[Tau]62, 0, t59}] + Integrate[
       (Ke*\[CapitalNu]*(bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]63)*L - 
          bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]63)*L - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]63)*Jout*R + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]63)*Jout*R + 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]63)*L*\[Eta]*\[CapitalNu]^2 - 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]63)*L*\[Eta]*\[CapitalNu]^2 - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]63)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]63)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]63)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]63)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*
         \[Tau]a58[t59 - \[Tau]63])/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
           (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]63, 0, t59}]}
 
Attributes[ea$106953] = {Temporary}
 
ea$106953 = ea57
 
Attributes[\[Tau]a$106953] = {Temporary}
 
\[Tau]a$106953 = \[Tau]a58
 
Attributes[t$106953] = {Temporary}
 
t$106953 = t59
 
motorTorque = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$108884 /. {ea$108884 -> eaActual$, 
          \[Tau]a$108884 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$108884 -> tActual$]]]
 
Attributes[expr$108884] = {Temporary}
 
expr$108884 = 
    {Integrate[(Kt*(-(bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]69)*L) + 
          bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]69)*L + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]69)*Jout*R - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]69)*Jout*R - 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]69)*L*\[Eta]*\[CapitalNu]^2 + 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]69)*L*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]69)*J*R*\[Eta]*\[CapitalNu]^2 - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]69)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]69)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]69)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*ea64[t66 - \[Tau]69])/
        (2*L*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
              \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
       {\[Tau]69, 0, t66}] + Integrate[
       ((E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                   \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]70) - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                   \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]70))*Ke*Kt*
         \[CapitalNu]*\[Tau]a65[t66 - \[Tau]70])/
        Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
          (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2], {\[Tau]70, 0, t66}]}
 
Attributes[ea$108884] = {Temporary}
 
ea$108884 = ea64
 
Attributes[\[Tau]a$108884] = {Temporary}
 
\[Tau]a$108884 = \[Tau]a65
 
Attributes[t$108884] = {Temporary}
 
t$108884 = t66
 
motorPositionOut = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$110815 /. {ea$110815 -> eaActual$, 
          \[Tau]a$110815 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$110815 -> tActual$]]]
 
Attributes[expr$110815] = {Temporary}
 
expr$110815 = {Integrate[Kt*\[Eta]*\[CapitalNu]*
        ((bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2)^
          (-1) - (-(bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                     2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                   J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                      \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                     b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                     b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^
                    2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*L) + 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*L - 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*Jout*R + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*Jout*R - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*L*\[Eta]*
            \[CapitalNu]^2 + b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*L*\[Eta]*
            \[CapitalNu]^2 - E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*J*R*\[Eta]*
            \[CapitalNu]^2 + E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*J*R*\[Eta]*
            \[CapitalNu]^2 + E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]76)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2])/(2*(bout*R + Ke*Kt*\[Eta]*
             \[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2)*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
             (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]))*ea71[t73 - \[Tau]76], 
       {\[Tau]76, 0, t73}] + Integrate[
       (R/(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
            \[CapitalNu]^2) - 
         (bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*L*R - 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*L*R - 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*Jout*R^2 + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*Jout*R^2 + 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*Ke*Kt*L*
            \[Eta]*\[CapitalNu]^2 - 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*Ke*Kt*L*
            \[Eta]*\[CapitalNu]^2 + 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*L*R*\[Eta]*
            \[CapitalNu]^2 - b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*L*R*\[Eta]*
            \[CapitalNu]^2 - E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*J*R^2*
            \[Eta]*\[CapitalNu]^2 + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*J*R^2*
            \[Eta]*\[CapitalNu]^2 + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*R*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] + 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]77)*R*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2])/(2*(bout*R + Ke*Kt*\[Eta]*
             \[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2)*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
             (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]))*\[Tau]a72[t73 - \[Tau]77], 
       {\[Tau]77, 0, t73}]}
 
Attributes[ea$110815] = {Temporary}
 
ea$110815 = ea71
 
Attributes[\[Tau]a$110815] = {Temporary}
 
\[Tau]a$110815 = \[Tau]a72
 
Attributes[t$110815] = {Temporary}
 
t$110815 = t73
 
motorVelocityOut = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$111599 /. {ea$111599 -> eaActual$, 
          \[Tau]a$111599 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$111599 -> tActual$]]]
 
Attributes[expr$111599] = {Temporary}
 
expr$111599 = 
    {Integrate[-(((E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]83) - 
           E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]83))*Kt*\[Eta]*\[CapitalNu]*
          ea78[t80 - \[Tau]83])/Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
              \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
       {\[Tau]83, 0, t80}] + Integrate[
       ((bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]84)*L - 
          bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]84)*L - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]84)*Jout*R + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]84)*Jout*R + 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]84)*L*\[Eta]*\[CapitalNu]^2 - 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]84)*L*\[Eta]*\[CapitalNu]^2 - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]84)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]84)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]84)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]84)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*
         \[Tau]a79[t80 - \[Tau]84])/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
           (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]84, 0, t80}]}
 
Attributes[ea$111599] = {Temporary}
 
ea$111599 = ea78
 
Attributes[\[Tau]a$111599] = {Temporary}
 
\[Tau]a$111599 = \[Tau]a79
 
Attributes[t$111599] = {Temporary}
 
t$111599 = t80
 
motorAccelerationOut = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$113530 /. {ea$113530 -> eaActual$, 
          \[Tau]a$113530 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$113530 -> tActual$]]]
 
Attributes[expr$113530] = {Temporary}
 
expr$113530 = {Integrate[(Kt*\[Eta]*\[CapitalNu]*
         (bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]90)*L - 
          bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                   b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]90)*L + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]90)*Jout*R - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]90)*Jout*R + 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]90)*L*\[Eta]*\[CapitalNu]^2 - 
          b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]90)*L*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]90)*J*R*\[Eta]*\[CapitalNu]^2 - 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]90)*J*R*\[Eta]*\[CapitalNu]^2 + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]90)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2] + 
          E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/(2*L*
                (Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
               (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
              Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                   Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
                 (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                    \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)))*\[Tau]90)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^
                 2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                 2 + J*R*\[Eta]*\[CapitalNu]^2)^2])*ea85[t87 - \[Tau]90])/
        (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
           (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]), {\[Tau]90, 0, t87}] + 
      Integrate[((-(bout^2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
                (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                     2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                   J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                      \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                     b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                     b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^
                    2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*L) + 
           bout^2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*L + 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*Jout*R - 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*Jout*R + 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*Jout*Ke*Kt*
            \[Eta]*\[CapitalNu]^2 - 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*Jout*Ke*Kt*
            \[Eta]*\[CapitalNu]^2 - 2*b*bout*
            E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*L*\[Eta]*
            \[CapitalNu]^2 + 2*b*bout*E^((-bout/(2*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)) - (b*\[Eta]*\[CapitalNu]^2)/
                (2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (J*R*\[Eta]*
                 \[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) + 
               Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                    Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^
                      2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + 
                    J*R*\[Eta]*\[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*
                   \[CapitalNu]^2)))*\[Tau]91)*L*\[Eta]*\[CapitalNu]^2 + 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*J*R*\[Eta]*
            \[CapitalNu]^2 - bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*J*R*\[Eta]*
            \[CapitalNu]^2 + b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*Jout*R*
            \[Eta]*\[CapitalNu]^2 - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*Jout*R*
            \[Eta]*\[CapitalNu]^2 + 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*J*Ke*Kt*
            \[Eta]^2*\[CapitalNu]^4 - 
           2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*J*Ke*Kt*
            \[Eta]^2*\[CapitalNu]^4 - 
           b^2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*L*\[Eta]^2*
            \[CapitalNu]^4 + b^2*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*L*\[Eta]^2*
            \[CapitalNu]^4 + b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (b*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                  J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                     \[CapitalNu]^2)*(bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                    b*R*\[Eta]*\[CapitalNu]^2) + (bout*L + Jout*R + 
                    b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*J*R*
            \[Eta]^2*\[CapitalNu]^4 - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*J*R*
            \[Eta]^2*\[CapitalNu]^4 - 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] - 
           bout*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*
            Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
              (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2] - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*\[Eta]*
            \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                 \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2)^2] - 
           b*E^((-bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (Jout*R)/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - (b*\[Eta]*
                 \[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
               (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                    2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                   (bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                     \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*
                     \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
                (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]91)*\[Eta]*
            \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*
                 \[CapitalNu]^2) + (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^
                  2 + J*R*\[Eta]*\[CapitalNu]^2)^2])/
          (2*(Jout + J*\[Eta]*\[CapitalNu]^2)^2*
           Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(bout*R + Ke*Kt*
                \[Eta]*\[CapitalNu]^2 + b*R*\[Eta]*\[CapitalNu]^2) + 
             (bout*L + Jout*R + b*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                \[CapitalNu]^2)^2]) + DiracDelta[\[Tau]91]/
          (Jout + J*\[Eta]*\[CapitalNu]^2))*\[Tau]a86[t87 - \[Tau]91], 
       {\[Tau]91, 0, t87}]}
 
Attributes[ea$113530] = {Temporary}
 
ea$113530 = ea85
 
Attributes[\[Tau]a$113530] = {Temporary}
 
\[Tau]a$113530 = \[Tau]a86
 
Attributes[t$113530] = {Temporary}
 
t$113530 = t87
 
makeMotorTimeDomainFunction[model_] := makeMotorTimeDomainFunction[model, 
     makeTimeDomainFunctionConvolve]
 
makeMotorTimeDomainFunction[model_, builder_] := 
    Module[{ea = Unique["ea"], \[Tau]a = Unique["\[Tau]a"], t = Unique["t"], 
      expr}, expr = builder[model, {ea[#1] & , \[Tau]a[#1] & }][t]; 
      Function[{eaActual, \[Tau]aActual}, Module[{exprT}, 
        exprT = expr /. {ea -> eaActual, \[Tau]a -> \[Tau]aActual}; 
         Function[{tActual}, exprT /. t -> tActual]]]]
 
makeTimeDomainFunctionConvolve[model_, tInputFunctions_] := 
    Module[{s = Unique["s"], \[Tau] = Unique["\[Tau]"], modelExpr}, 
     modelExpr = InverseLaplaceTransform[model[s], s, \[Tau]][[1]]; 
      Function[{t}, Module[{convolutions}, 
        convolutions = (convolve[modelExpr[[#1]], tInputFunctions[[#1]][
              \[Tau]], \[Tau], t] & ) /@ Range[1, Length[tInputFunctions]]; 
         {Total[convolutions]}]]]
 
convolve[fExpr_, gExpr_, exprVar_, t_] := Module[{\[Tau] = Unique["\[Tau]"]}, 
     Assuming[Union[{t >= 0}, parameterAssumptions], 
      Integrate[(fExpr /. exprVar -> \[Tau])*(gExpr /. 
         exprVar -> t - \[Tau]), {\[Tau], 0, t}]]]
 
parameterAssumptions = {Element[bout, Reals], Element[Jout, Reals], 
     Element[e[_], Reals], Element[ea[_], Reals], Element[i[_], Reals], 
     Element[\[Alpha][_], Reals], Element[\[Theta][_], Reals], 
     Element[\[Tau][_], Reals], Element[\[Tau]a[_], Reals], 
     Element[\[CapitalOmega][_], Reals], Ke > 0, Kt > 0, \[CapitalNu] > 0, 
     b >= 0, J >= 0, L >= 0, R >= 0, t >= 0, \[Eta] >= 0}
