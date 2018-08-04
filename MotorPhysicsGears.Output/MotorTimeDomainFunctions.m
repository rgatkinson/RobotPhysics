motorPosition = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$18317 /. {ea$18317 -> eaActual$, 
          \[Tau]a$18317 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$18317 -> tActual$]]]
 
Attributes[eaActual$] = {Temporary}
 
Attributes[\[Tau]aActual$] = {Temporary}
 
Attributes[exprT$] = {Temporary}
 
Attributes[expr$18317] = {Temporary}
 
expr$18317 = {Integrate[Kt*\[Eta]*\[CapitalNu]^2*
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
 
Attributes[ea$18317] = {Temporary}
 
ea$18317 = ea21
 
Attributes[\[Tau]a$18317] = {Temporary}
 
\[Tau]a$18317 = \[Tau]a22
 
Attributes[tActual$] = {Temporary}
 
Attributes[t$18317] = {Temporary}
 
t$18317 = t23
 
motorVelocity = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$19105 /. {ea$19105 -> eaActual$, 
          \[Tau]a$19105 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$19105 -> tActual$]]]
 
Attributes[expr$19105] = {Temporary}
 
expr$19105 = 
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
 
Attributes[ea$19105] = {Temporary}
 
ea$19105 = ea28
 
Attributes[\[Tau]a$19105] = {Temporary}
 
\[Tau]a$19105 = \[Tau]a29
 
Attributes[t$19105] = {Temporary}
 
t$19105 = t30
 
motorAcceleration = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$21019 /. {ea$21019 -> eaActual$, 
          \[Tau]a$21019 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$21019 -> tActual$]]]
 
Attributes[expr$21019] = {Temporary}
 
expr$21019 = {Integrate[(Kt*\[Eta]*\[CapitalNu]^2*
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
 
Attributes[ea$21019] = {Temporary}
 
ea$21019 = ea35
 
Attributes[\[Tau]a$21019] = {Temporary}
 
\[Tau]a$21019 = \[Tau]a36
 
Attributes[t$21019] = {Temporary}
 
t$21019 = t37
 
motorCurrent = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$100930 /. {ea$100930 -> eaActual$, 
          \[Tau]a$100930 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$100930 -> tActual$]]]
 
Attributes[expr$100930] = {Temporary}
 
expr$100930 = 
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
 
Attributes[ea$100930] = {Temporary}
 
ea$100930 = ea50
 
Attributes[\[Tau]a$100930] = {Temporary}
 
\[Tau]a$100930 = \[Tau]a51
 
Attributes[t$100930] = {Temporary}
 
t$100930 = t52
 
motorEMF = Function[{eaActual$, \[Tau]aActual$}, Module[{exprT$}, 
      exprT$ = expr$102861 /. {ea$102861 -> eaActual$, \[Tau]a$102861 -> 
           \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$102861 -> tActual$]]]
 
Attributes[expr$102861] = {Temporary}
 
expr$102861 = 
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
 
Attributes[ea$102861] = {Temporary}
 
ea$102861 = ea57
 
Attributes[\[Tau]a$102861] = {Temporary}
 
\[Tau]a$102861 = \[Tau]a58
 
Attributes[t$102861] = {Temporary}
 
t$102861 = t59
 
motorTorque = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$104792 /. {ea$104792 -> eaActual$, 
          \[Tau]a$104792 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$104792 -> tActual$]]]
 
Attributes[expr$104792] = {Temporary}
 
expr$104792 = 
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
 
Attributes[ea$104792] = {Temporary}
 
ea$104792 = ea64
 
Attributes[\[Tau]a$104792] = {Temporary}
 
\[Tau]a$104792 = \[Tau]a65
 
Attributes[t$104792] = {Temporary}
 
t$104792 = t66
 
motorPositionOut = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$106723 /. {ea$106723 -> eaActual$, 
          \[Tau]a$106723 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$106723 -> tActual$]]]
 
Attributes[expr$106723] = {Temporary}
 
expr$106723 = {Integrate[Kt*\[Eta]*\[CapitalNu]*
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
 
Attributes[ea$106723] = {Temporary}
 
ea$106723 = ea71
 
Attributes[\[Tau]a$106723] = {Temporary}
 
\[Tau]a$106723 = \[Tau]a72
 
Attributes[t$106723] = {Temporary}
 
t$106723 = t73
 
motorVelocityOut = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$107507 /. {ea$107507 -> eaActual$, 
          \[Tau]a$107507 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$107507 -> tActual$]]]
 
Attributes[expr$107507] = {Temporary}
 
expr$107507 = 
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
 
Attributes[ea$107507] = {Temporary}
 
ea$107507 = ea78
 
Attributes[\[Tau]a$107507] = {Temporary}
 
\[Tau]a$107507 = \[Tau]a79
 
Attributes[t$107507] = {Temporary}
 
t$107507 = t80
 
motorAccelerationOut = Function[{eaActual$, \[Tau]aActual$}, 
     Module[{exprT$}, exprT$ = expr$109438 /. {ea$109438 -> eaActual$, 
          \[Tau]a$109438 -> \[Tau]aActual$}; Function[{tActual$}, 
        exprT$ /. t$109438 -> tActual$]]]
 
Attributes[expr$109438] = {Temporary}
 
expr$109438 = {Integrate[(Kt*\[Eta]*\[CapitalNu]*
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
 
Attributes[ea$109438] = {Temporary}
 
ea$109438 = ea85
 
Attributes[\[Tau]a$109438] = {Temporary}
 
\[Tau]a$109438 = \[Tau]a86
 
Attributes[t$109438] = {Temporary}
 
t$109438 = t87
 
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
