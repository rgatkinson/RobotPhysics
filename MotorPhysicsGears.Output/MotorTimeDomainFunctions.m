motorPosition = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
           s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
               (B + J*s)*\[Eta]*\[CapitalNu]^2))}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Theta]", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
Attributes[vapp$] = {Temporary}
 
Attributes[\[Tau]appafter$] = {Temporary}
 
Attributes[fn$] = {Temporary}
 
makeTimeDomainFunctionConvolve[model_, tInputFunctions_] := 
    Module[{s, \[Tau], modelExpr}, 
     modelExpr = InverseLaplaceTransform[model[s], s, \[Tau]][[1]]; 
      Function[{t}, Module[{convolutions}, 
        convolutions = (convolve[modelExpr[[#1]], tInputFunctions[[#1]][
              \[Tau]], \[Tau], t] & ) /@ Range[1, Length[tInputFunctions]]; 
         {Total[convolutions]}]]]
 
convolve[fExpr_, gExpr_, exprVar_, t_] := Module[{\[Tau]}, 
     Assuming[Union[{t >= 0}, parameterAssumptions], 
      Integrate[(fExpr /. exprVar -> \[Tau])*(gExpr /. 
         exprVar -> t - \[Tau]), {\[Tau], 0, t}]]]
 
parameterAssumptions = {Element[Bafter, Reals], Element[constvapp, Reals], 
     Element[const\[Tau]appafter, Reals], Element[Jafter, Reals], 
     Element[i[_], Reals], Element[vapp[_], Reals], Element[vg[_], Reals], 
     Element[\[Alpha][_], Reals], Element[\[Theta][_], Reals], 
     Element[\[Tau][_], Reals], Element[\[Tau]appafter[_], Reals], 
     Element[\[Omega][_], Reals], Ke > 0, Kt > 0, L > 0, R > 0, \[Eta] > 0, 
     \[CapitalNu] > 0, B >= 0, J >= 0, t >= 0}
 
motorVelocity = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Omega]", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
motorAcceleration = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Alpha]", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
motorCurrent = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2, 
             -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, 
          s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "i", 
            Automatic}], {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
motorEMF = Function[{vapp$, \[Tau]appafter$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "vg", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
motorTorque = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
             -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, 
          s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "\[Tau]", 
            Automatic}], {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
motorPositionOut = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, 
           s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
               (B + J*s)*\[Eta]*\[CapitalNu]^2))}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Theta]after", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
motorVelocityOut = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, 
          s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, 
            "\[Omega]out", Automatic}], {vapp$[#1] & , 
          \[Tau]appafter$[#1] & }]; fn$]]
 
motorAccelerationOut = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Alpha]out", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
motorPositionConstvapp = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$18334[[#1]], 
           {(vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$18334], 
           \[Tau]$18334, t$] & ) /@ Range[1, Length[{(vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[t$] = {Temporary}
 
Attributes[convolutions$] = {Temporary}
 
Attributes[modelExpr$18334] = {Temporary}
 
modelExpr$18334 = {Kt*\[Eta]*\[CapitalNu]^2*
      ((Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)^
        (-1) - (-(Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*L) + 
         Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*L - 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*Jafter*
          R + E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*Jafter*
          R - B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*L*
          \[Eta]*\[CapitalNu]^2 + 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*L*
          \[Eta]*\[CapitalNu]^2 - 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*
          Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2] + 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*
          Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2])/
        (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
           \[CapitalNu]^2)*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])), 
     \[CapitalNu]*(R/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         B*R*\[Eta]*\[CapitalNu]^2) - 
       (Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*L*R - 
         Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*L*R - 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*Jafter*
          R^2 + E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*Jafter*
          R^2 + 2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*Ke*Kt*L*
          \[Eta]*\[CapitalNu]^2 - 
         2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*Ke*Kt*L*
          \[Eta]*\[CapitalNu]^2 + 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*L*R*
          \[Eta]*\[CapitalNu]^2 - 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*L*R*
          \[Eta]*\[CapitalNu]^2 - 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*J*R^2*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*J*R^2*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*R*
          Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2] + 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18334)*R*
          Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2])/
        (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
           \[CapitalNu]^2)*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]))}
 
Attributes[\[Tau]$18334] = {Temporary}
 
motorVelocityConstvapp = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$18416[[#1]], 
           {(vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$18416], 
           \[Tau]$18416, t$] & ) /@ Range[1, Length[{(vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$18416] = {Temporary}
 
modelExpr$18416 = 
    {-(((E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416) - 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416))*Kt*
        \[Eta]*\[CapitalNu]^2)/Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     (\[CapitalNu]*
       (Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*L - 
        Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*L - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*Jafter*
         R + E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*Jafter*
         R + B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*L*\[Eta]*
         \[CapitalNu]^2 - 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*L*\[Eta]*
         \[CapitalNu]^2 - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*J*R*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*J*R*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18416)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2])}
 
Attributes[\[Tau]$18416] = {Temporary}
 
motorAccelerationConstvapp = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$18486[[#1]], 
           {(vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$18486], 
           \[Tau]$18486, t$] & ) /@ Range[1, Length[{(vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$18486] = {Temporary}
 
modelExpr$18486 = {(Kt*\[Eta]*\[CapitalNu]^2*
       (Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L - 
        Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*Jafter*
         R - E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*Jafter*
         R + B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L*\[Eta]*
         \[CapitalNu]^2 - 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L*\[Eta]*
         \[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*J*R*
         \[Eta]*\[CapitalNu]^2 - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*J*R*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]), \[CapitalNu]*
      ((-(Bafter^2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L) + 
         Bafter^2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L + 
         Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*Jafter*
          R - Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*Jafter*
          R + 2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*Jafter*
          Ke*Kt*\[Eta]*\[CapitalNu]^2 - 
         2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*Jafter*
          Ke*Kt*\[Eta]*\[CapitalNu]^2 - 2*B*Bafter*
          E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L*
          \[Eta]*\[CapitalNu]^2 + 2*B*Bafter*
          E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L*
          \[Eta]*\[CapitalNu]^2 + Bafter*
          E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*J*R*
          \[Eta]*\[CapitalNu]^2 - Bafter*
          E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*Jafter*
          R*\[Eta]*\[CapitalNu]^2 - 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*Jafter*
          R*\[Eta]*\[CapitalNu]^2 + 
         2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*J*Ke*Kt*
          \[Eta]^2*\[CapitalNu]^4 - 
         2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*J*Ke*Kt*
          \[Eta]^2*\[CapitalNu]^4 - 
         B^2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L*
          \[Eta]^2*\[CapitalNu]^4 + 
         B^2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*L*
          \[Eta]^2*\[CapitalNu]^4 + 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*J*R*
          \[Eta]^2*\[CapitalNu]^4 - 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*J*R*
          \[Eta]^2*\[CapitalNu]^4 - Bafter*
          E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*
          Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2] - 
         Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*
          Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2] - 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*\[Eta]*
          \[CapitalNu]^2*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
             (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
               \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
               \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2] - 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18486)*\[Eta]*
          \[CapitalNu]^2*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
             (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
               \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
               \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])/
        (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)^2*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]) + DiracDelta[\[Tau]$18486]/
        (Jafter + J*\[Eta]*\[CapitalNu]^2))}
 
Attributes[\[Tau]$18486] = {Temporary}
 
motorCurrentConstvapp = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$18558[[#1]], 
           {(vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$18558], 
           \[Tau]$18558, t$] & ) /@ Range[1, Length[{(vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$18558] = {Temporary}
 
modelExpr$18558 = 
    {(-(Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*L) + 
       Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*L + 
       E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*Jafter*
        R - E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*Jafter*
        R - B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*L*\[Eta]*
        \[CapitalNu]^2 + 
       B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*L*\[Eta]*
        \[CapitalNu]^2 + E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*J*R*
        \[Eta]*\[CapitalNu]^2 - 
       E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*J*R*
        \[Eta]*\[CapitalNu]^2 + 
       E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2] + 
       E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2])/
      (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     ((E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558) - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18558))*Ke*
       \[CapitalNu])/Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
         (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
           \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
           \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]}
 
Attributes[\[Tau]$18558] = {Temporary}
 
motorEMFConstvapp = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$18628[[#1]], 
           {(vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$18628], 
           \[Tau]$18628, t$] & ) /@ Range[1, Length[{(vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$18628] = {Temporary}
 
modelExpr$18628 = 
    {-(((E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628) - 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628))*Ke*Kt*
        \[Eta]*\[CapitalNu]^2)/Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     (Ke*\[CapitalNu]*
       (Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*L - 
        Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*L - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*Jafter*
         R + E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*Jafter*
         R + B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*L*\[Eta]*
         \[CapitalNu]^2 - 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*L*\[Eta]*
         \[CapitalNu]^2 - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*J*R*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*J*R*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18628)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2])}
 
Attributes[\[Tau]$18628] = {Temporary}
 
motorTorqueConstvapp = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$18698[[#1]], 
           {(vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$18698], 
           \[Tau]$18698, t$] & ) /@ Range[1, Length[{(vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$18698] = {Temporary}
 
modelExpr$18698 = 
    {(Kt*(-(Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*L) + 
        Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*L + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*Jafter*
         R - E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*Jafter*
         R - B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*L*\[Eta]*
         \[CapitalNu]^2 + 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*L*\[Eta]*
         \[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*J*R*
         \[Eta]*\[CapitalNu]^2 - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*J*R*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*L*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     ((E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698) - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18698))*Ke*Kt*
       \[CapitalNu])/Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
         (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
           \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
           \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]}
 
Attributes[\[Tau]$18698] = {Temporary}
 
motorPositionOutConstvapp = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$18768[[#1]], 
           {(vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$18768], 
           \[Tau]$18768, t$] & ) /@ Range[1, Length[{(vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$18768] = {Temporary}
 
modelExpr$18768 = {Kt*\[Eta]*\[CapitalNu]*
      ((Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)^
        (-1) - (-(Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*L) + 
         Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*L - 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*Jafter*
          R + E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*Jafter*
          R - B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*L*
          \[Eta]*\[CapitalNu]^2 + 
         B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*L*
          \[Eta]*\[CapitalNu]^2 - 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*
          Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2] + 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*
          Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2])/
        (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
           \[CapitalNu]^2)*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])), 
     R/(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) - 
      (Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*L*R - 
        Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*L*R - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*Jafter*
         R^2 + E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*Jafter*
         R^2 + 2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*Ke*Kt*L*
         \[Eta]*\[CapitalNu]^2 - 
        2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*Ke*Kt*L*
         \[Eta]*\[CapitalNu]^2 + 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*L*R*
         \[Eta]*\[CapitalNu]^2 - 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*L*R*
         \[Eta]*\[CapitalNu]^2 - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*J*R^2*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*J*R^2*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*R*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18768)*R*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2])/
       (2*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         B*R*\[Eta]*\[CapitalNu]^2)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2])}
 
Attributes[\[Tau]$18768] = {Temporary}
 
motorVelocityOutConstvapp = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$18846[[#1]], 
           {(vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$18846], 
           \[Tau]$18846, t$] & ) /@ Range[1, Length[{(vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$18846] = {Temporary}
 
modelExpr$18846 = 
    {-(((E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846) - 
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846))*Kt*
        \[Eta]*\[CapitalNu])/Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
            \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     (Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*L - 
       Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*L - 
       E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*Jafter*
        R + E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*Jafter*
        R + B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*L*\[Eta]*
        \[CapitalNu]^2 - 
       B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*L*\[Eta]*
        \[CapitalNu]^2 - E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*J*R*
        \[Eta]*\[CapitalNu]^2 + 
       E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*J*R*
        \[Eta]*\[CapitalNu]^2 + 
       E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2] + 
       E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(
                Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                 \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
                 \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
            (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18846)*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2])/
      (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2])}
 
Attributes[\[Tau]$18846] = {Temporary}
 
motorAccelerationOutConstvapp = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$18916[[#1]], 
           {(vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$18916], 
           \[Tau]$18916, t$] & ) /@ Range[1, Length[{(vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$18916] = {Temporary}
 
modelExpr$18916 = {(Kt*\[Eta]*\[CapitalNu]*
       (Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L - 
        Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*Jafter*
         R - E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*Jafter*
         R + B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L*\[Eta]*
         \[CapitalNu]^2 - 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L*\[Eta]*
         \[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*J*R*
         \[Eta]*\[CapitalNu]^2 - 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*J*R*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
         (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     (-(Bafter^2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                  2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + 
                J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                   \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                  B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L) + 
        Bafter^2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L + 
        Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*Jafter*
         R - Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*Jafter*
         R + 2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*Jafter*
         Ke*Kt*\[Eta]*\[CapitalNu]^2 - 
        2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*Jafter*
         Ke*Kt*\[Eta]*\[CapitalNu]^2 - 2*B*Bafter*
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L*\[Eta]*
         \[CapitalNu]^2 + 2*B*Bafter*
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L*\[Eta]*
         \[CapitalNu]^2 + Bafter*
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*J*R*
         \[Eta]*\[CapitalNu]^2 - Bafter*
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*J*R*
         \[Eta]*\[CapitalNu]^2 + 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*Jafter*R*
         \[Eta]*\[CapitalNu]^2 - 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*Jafter*R*
         \[Eta]*\[CapitalNu]^2 + 
        2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*J*Ke*Kt*
         \[Eta]^2*\[CapitalNu]^4 - 
        2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*J*Ke*Kt*
         \[Eta]^2*\[CapitalNu]^4 - 
        B^2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L*
         \[Eta]^2*\[CapitalNu]^4 + 
        B^2*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*L*
         \[Eta]^2*\[CapitalNu]^4 + 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*J*R*
         \[Eta]^2*\[CapitalNu]^4 - 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*J*R*
         \[Eta]^2*\[CapitalNu]^4 - Bafter*
         E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] - 
        Bafter*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*
         Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] - 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) - Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*\[Eta]*
         \[CapitalNu]^2*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2] - 
        B*E^((-Bafter/(2*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jafter*R)/(2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jafter + J*\[Eta]*\[CapitalNu]^
                 2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jafter + J*\[Eta]*
                \[CapitalNu]^2)) + Sqrt[-4*(Jafter*L + J*L*\[Eta]*
                  \[CapitalNu]^2)*(Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 B*R*\[Eta]*\[CapitalNu]^2) + (Bafter*L + Jafter*R + 
                 B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jafter + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$18916)*\[Eta]*
         \[CapitalNu]^2*Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bafter*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bafter*L + Jafter*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])/
       (2*(Jafter + J*\[Eta]*\[CapitalNu]^2)^2*
        Sqrt[-4*(Jafter*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bafter*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bafter*L + Jafter*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2]) + DiracDelta[\[Tau]$18916]/
       (Jafter + J*\[Eta]*\[CapitalNu]^2)}
 
Attributes[\[Tau]$18916] = {Temporary}
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
      SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "\[Omega]out", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vapp$, \[Tau]appafter$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, 
          s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, 
            "\[Omega]out", Automatic}], {vapp$[#1] & , 
          \[Tau]appafter$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2))}, 
      s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "\[Theta]after", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vapp$, \[Tau]appafter$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, 
           s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
               (B + J*s)*\[Eta]*\[CapitalNu]^2))}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Theta]after", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, 
       Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
          (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
       {{"vapp", "\[Tau]appafter"}, "\[Alpha]out", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Alpha]out", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
       Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
          (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
       {{"vapp", "\[Tau]appafter"}, "\[Omega]", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Omega]", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
       s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
           (B + J*s)*\[Eta]*\[CapitalNu]^2))}, s, SystemsModelLabels -> 
       {{"vapp", "\[Tau]appafter"}, "\[Theta]", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
           s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
               (B + J*s)*\[Eta]*\[CapitalNu]^2))}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Theta]", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
       Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
          (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
       {{"vapp", "\[Tau]appafter"}, "vg", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "vg", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
       Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
          (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
       {{"vapp", "\[Tau]appafter"}, "\[Alpha]", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
           Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vapp", "\[Tau]appafter"}, "\[Alpha]", Automatic}], 
         {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2, 
         -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
      SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "i", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vapp$, \[Tau]appafter$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2, 
             -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, 
          s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "i", 
            Automatic}], {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
         -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
      SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "\[Tau]", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vapp$, \[Tau]appafter$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
             -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, 
          s, SystemsModelLabels -> {{"vapp", "\[Tau]appafter"}, "\[Tau]", 
            Automatic}], {vapp$[#1] & , \[Tau]appafter$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[model_] := makeMotorTimeDomainFunction[model, 
     makeTimeDomainFunctionConvolve]
 
makeMotorTimeDomainFunction[model_, builder_] := 
    makeMotorTimeDomainFunction[model, builder] = 
     Module[{t}, Function[{vapp, \[Tau]appafter}, 
       Module[{fn}, fn = builder[model, {vapp[#1] & , 
            \[Tau]appafter[#1] & }]; fn]]]
