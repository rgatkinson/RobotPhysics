motorPosition = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
           Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + s*(R + L*s)*(Bout + Jout*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Theta]", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
Attributes[vbat$] = {Temporary}
 
Attributes[\[Tau]auto$] = {Temporary}
 
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
 
parameterAssumptions = {Element[Bout, Reals], Element[constvbat, Reals], 
     Element[const\[Tau]auto, Reals], Element[Jout, Reals], 
     Element[i[_], Reals], Element[vbat[_], Reals], Element[vg[_], Reals], 
     Element[\[Alpha][_], Reals], Element[\[Theta][_], Reals], 
     Element[\[Tau][_], Reals], Element[\[Tau]auto[_], Reals], 
     Element[\[Omega][_], Reals], Ke > 0, Kt > 0, L > 0, R > 0, \[Eta] > 0, 
     \[CapitalNu] > 0, B >= 0, J >= 0, t >= 0}
 
motorVelocity = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
           Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
             \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Omega]", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
motorAcceleration = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
           Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
             \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Alpha]", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
motorCurrent = Function[{vbat$, \[Tau]auto$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2, 
             -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
          SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "i", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
motorEMF = Function[{vbat$, \[Tau]auto$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
           Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
             \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "vg", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
motorTorque = Function[{vbat$, \[Tau]auto$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
             -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
          SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "\[Tau]", 
            Automatic}], {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
motorPositionOut = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, 
           Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + s*(R + L*s)*(Bout + Jout*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Theta]out", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
motorVelocityOut = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Bout*(R + L*s) + 
            Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
             \[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Omega]out", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
motorAccelerationOut = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, Bout*(R + L*s) + 
            Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
             \[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Alpha]out", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
motorPositionConstVbat = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$10815[[#1]], 
           {(Vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$10815], 
           \[Tau]$10815, t$] & ) /@ Range[1, Length[{(Vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[t$] = {Temporary}
 
Attributes[convolutions$] = {Temporary}
 
Attributes[modelExpr$10815] = {Temporary}
 
modelExpr$10815 = {Kt*\[Eta]*\[CapitalNu]^2*
      ((Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)^
        (-1) - (-(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*L) + 
         Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*L - 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*Jout*R + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*Jout*R - 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*L*\[Eta]*
          \[CapitalNu]^2 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*L*\[Eta]*
          \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*
          Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
               \[CapitalNu]^2)^2] + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*
          Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
               \[CapitalNu]^2)^2])/(2*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          B*R*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2])), 
     \[CapitalNu]*(R/(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
         B*R*\[Eta]*\[CapitalNu]^2) - 
       (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*L*R - 
         Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*L*R - 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*Jout*
          R^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*Jout*
          R^2 + 2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*Ke*Kt*L*
          \[Eta]*\[CapitalNu]^2 - 
         2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*Ke*Kt*L*
          \[Eta]*\[CapitalNu]^2 + 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*L*R*
          \[Eta]*\[CapitalNu]^2 - 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*L*R*
          \[Eta]*\[CapitalNu]^2 - 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*J*R^2*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*J*R^2*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*R*
          Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
               \[CapitalNu]^2)^2] + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10815)*R*
          Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
               \[CapitalNu]^2)^2])/(2*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          B*R*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))}
 
Attributes[\[Tau]$10815] = {Temporary}
 
motorVelocityConstVbat = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$10897[[#1]], 
           {(Vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$10897], 
           \[Tau]$10897, t$] & ) /@ Range[1, Length[{(Vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$10897] = {Temporary}
 
modelExpr$10897 = 
    {-(((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897) - 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897))*Kt*\[Eta]*
        \[CapitalNu]^2)/Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     (\[CapitalNu]*(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*L - 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*L - 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*Jout*R + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*Jout*R + 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*L*\[Eta]*
         \[CapitalNu]^2 - B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*L*\[Eta]*
         \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*J*R*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*J*R*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10897)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*(Jout + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2])}
 
Attributes[\[Tau]$10897] = {Temporary}
 
motorAccelerationConstVbat = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$10967[[#1]], 
           {(Vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$10967], 
           \[Tau]$10967, t$] & ) /@ Range[1, Length[{(Vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$10967] = {Temporary}
 
modelExpr$10967 = {(Kt*\[Eta]*\[CapitalNu]^2*
       (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L - 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*Jout*R - 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*Jout*R + 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L*\[Eta]*
         \[CapitalNu]^2 - B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*J*R*\[Eta]*
         \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*J*R*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]), \[CapitalNu]*
      ((-(Bout^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L) + 
         Bout^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L + 
         Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*Jout*R - 
         Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*Jout*R + 
         2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*Jout*Ke*
          Kt*\[Eta]*\[CapitalNu]^2 - 
         2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*Jout*Ke*
          Kt*\[Eta]*\[CapitalNu]^2 - 2*B*Bout*
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L*\[Eta]*
          \[CapitalNu]^2 + 2*B*Bout*
          E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L*\[Eta]*
          \[CapitalNu]^2 + Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*J*R*
          \[Eta]*\[CapitalNu]^2 - 
         Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*Jout*R*
          \[Eta]*\[CapitalNu]^2 - 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*Jout*R*
          \[Eta]*\[CapitalNu]^2 + 
         2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*J*Ke*Kt*
          \[Eta]^2*\[CapitalNu]^4 - 
         2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*J*Ke*Kt*
          \[Eta]^2*\[CapitalNu]^4 - 
         B^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L*
          \[Eta]^2*\[CapitalNu]^4 + 
         B^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*L*
          \[Eta]^2*\[CapitalNu]^4 + 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*J*R*
          \[Eta]^2*\[CapitalNu]^4 - 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*J*R*
          \[Eta]^2*\[CapitalNu]^4 - 
         Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*
          Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
               \[CapitalNu]^2)^2] - 
         Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*
          Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
               \[CapitalNu]^2)^2] - 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*\[Eta]*
          \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
             (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^
                2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2] - 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$10967)*\[Eta]*
          \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
             (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^
                2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
              J*R*\[Eta]*\[CapitalNu]^2)^2])/
        (2*(Jout + J*\[Eta]*\[CapitalNu]^2)^2*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]) + DiracDelta[\[Tau]$10967]/
        (Jout + J*\[Eta]*\[CapitalNu]^2))}
 
Attributes[\[Tau]$10967] = {Temporary}
 
motorCurrentConstVbat = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$11039[[#1]], 
           {(Vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$11039], 
           \[Tau]$11039, t$] & ) /@ Range[1, Length[{(Vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$11039] = {Temporary}
 
modelExpr$11039 = 
    {(-(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11039)*L) + 
       Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039)*L + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039)*Jout*R - 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039)*Jout*R - 
       B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039)*L*\[Eta]*\[CapitalNu]^2 + 
       B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039)*L*\[Eta]*\[CapitalNu]^2 + 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039)*J*R*\[Eta]*\[CapitalNu]^2 - 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039)*J*R*\[Eta]*\[CapitalNu]^2 + 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
           (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
             \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2] + 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
           (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
             \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2])/
      (2*L*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     ((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039) - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11039))*Ke*\[CapitalNu])/
      Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
         (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
        (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2)^2]}
 
Attributes[\[Tau]$11039] = {Temporary}
 
motorEMFConstVbat = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$11109[[#1]], 
           {(Vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$11109], 
           \[Tau]$11109, t$] & ) /@ Range[1, Length[{(Vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$11109] = {Temporary}
 
modelExpr$11109 = 
    {-(((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109) - 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109))*Ke*Kt*
        \[Eta]*\[CapitalNu]^2)/Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     (Ke*\[CapitalNu]*(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*L - 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*L - 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*Jout*R + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*Jout*R + 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*L*\[Eta]*
         \[CapitalNu]^2 - B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*L*\[Eta]*
         \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*J*R*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*J*R*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11109)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*(Jout + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2])}
 
Attributes[\[Tau]$11109] = {Temporary}
 
motorTorqueConstVbat = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$11179[[#1]], 
           {(Vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$11179], 
           \[Tau]$11179, t$] & ) /@ Range[1, Length[{(Vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$11179] = {Temporary}
 
modelExpr$11179 = 
    {(Kt*(-(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*L) + 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*L + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*Jout*R - 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*Jout*R - 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*L*\[Eta]*
         \[CapitalNu]^2 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*L*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*J*R*\[Eta]*
         \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*J*R*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11179)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*L*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     ((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11179) - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11179))*Ke*Kt*\[CapitalNu])/
      Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
         (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
        (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
          J*R*\[Eta]*\[CapitalNu]^2)^2]}
 
Attributes[\[Tau]$11179] = {Temporary}
 
motorPositionOutConstVbat = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$11249[[#1]], 
           {(Vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$11249], 
           \[Tau]$11249, t$] & ) /@ Range[1, Length[{(Vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$11249] = {Temporary}
 
modelExpr$11249 = {Kt*\[Eta]*\[CapitalNu]*
      ((Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)^
        (-1) - (-(Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
              (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^
                   2)) - (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + 
                 J*\[Eta]*\[CapitalNu]^2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*
                    \[CapitalNu]^2)*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                   B*R*\[Eta]*\[CapitalNu]^2) + (Bout*L + Jout*R + 
                   B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/(
                2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*L) + 
         Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*L - 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*Jout*R + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*Jout*R - 
         B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*L*\[Eta]*
          \[CapitalNu]^2 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*L*\[Eta]*
          \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*J*R*
          \[Eta]*\[CapitalNu]^2 + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*
          Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
               \[CapitalNu]^2)^2] + 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*
          Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
              Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
            (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
               \[CapitalNu]^2)^2])/(2*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          B*R*\[Eta]*\[CapitalNu]^2)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2])), 
     R/(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) - 
      (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*L*R - 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*L*R - 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*Jout*R^2 + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*Jout*R^2 + 
        2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*Ke*Kt*L*
         \[Eta]*\[CapitalNu]^2 - 
        2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*Ke*Kt*L*
         \[Eta]*\[CapitalNu]^2 + 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*L*R*\[Eta]*
         \[CapitalNu]^2 - B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*L*R*\[Eta]*
         \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*J*R^2*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*J*R^2*
         \[Eta]*\[CapitalNu]^2 + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*R*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11249)*R*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2])/
       (2*(Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2)*
        Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2])}
 
Attributes[\[Tau]$11249] = {Temporary}
 
motorVelocityOutConstVbat = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$11327[[#1]], 
           {(Vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$11327], 
           \[Tau]$11327, t$] & ) /@ Range[1, Length[{(Vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$11327] = {Temporary}
 
modelExpr$11327 = 
    {-(((E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11327) - 
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11327))*Kt*\[Eta]*
        \[CapitalNu])/Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*L - 
       Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*L - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*Jout*R + 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*Jout*R + 
       B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*L*\[Eta]*\[CapitalNu]^2 - 
       B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*L*\[Eta]*\[CapitalNu]^2 - 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*J*R*\[Eta]*\[CapitalNu]^2 + 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*J*R*\[Eta]*\[CapitalNu]^2 + 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
           (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
             \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2] + 
       E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
           (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
              (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + J*R*\[Eta]*
                 \[CapitalNu]^2)^2]/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*
          \[Tau]$11327)*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
           (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
             \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2])/
      (2*(Jout + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2])}
 
Attributes[\[Tau]$11327] = {Temporary}
 
motorAccelerationOutConstVbat = Function[{t$}, Module[{convolutions$}, 
      convolutions$ = (convolve[modelExpr$11397[[#1]], 
           {(Vbat & )[#1] & , (0 & )[#1] & }[[#1]][\[Tau]$11397], 
           \[Tau]$11397, t$] & ) /@ Range[1, Length[{(Vbat & )[#1] & , 
            (0 & )[#1] & }]]; {Total[convolutions$]}]]
 
Attributes[modelExpr$11397] = {Temporary}
 
modelExpr$11397 = {(Kt*\[Eta]*\[CapitalNu]*
       (Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L - 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*Jout*R - 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*Jout*R + 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L*\[Eta]*
         \[CapitalNu]^2 - B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*J*R*\[Eta]*
         \[CapitalNu]^2 - E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*J*R*\[Eta]*
         \[CapitalNu]^2 + E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] + 
        E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2]))/
      (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)*
       Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
          (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
            \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
           J*R*\[Eta]*\[CapitalNu]^2)^2]), 
     (-(Bout^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
             (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                  2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                 (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                   \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                   \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
              (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L) + 
        Bout^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L + 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*Jout*R - 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*Jout*R + 
        2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*Jout*Ke*Kt*
         \[Eta]*\[CapitalNu]^2 - 
        2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*Jout*Ke*Kt*
         \[Eta]*\[CapitalNu]^2 - 2*B*Bout*
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L*\[Eta]*
         \[CapitalNu]^2 + 2*B*Bout*
         E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L*\[Eta]*
         \[CapitalNu]^2 + 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*J*R*\[Eta]*
         \[CapitalNu]^2 - 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*J*R*\[Eta]*
         \[CapitalNu]^2 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*Jout*R*
         \[Eta]*\[CapitalNu]^2 - 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*Jout*R*
         \[Eta]*\[CapitalNu]^2 + 
        2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*J*Ke*Kt*
         \[Eta]^2*\[CapitalNu]^4 - 
        2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*J*Ke*Kt*
         \[Eta]^2*\[CapitalNu]^4 - 
        B^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L*\[Eta]^2*
         \[CapitalNu]^4 + 
        B^2*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*L*\[Eta]^2*
         \[CapitalNu]^4 + B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*J*R*
         \[Eta]^2*\[CapitalNu]^4 - 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*J*R*
         \[Eta]^2*\[CapitalNu]^4 - 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] - 
        Bout*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*
         Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
             Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
           (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
             J*R*\[Eta]*\[CapitalNu]^2)^2] - 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) - Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*\[Eta]*
         \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2] - 
        B*E^((-Bout/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (Jout*R)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (B*\[Eta]*\[CapitalNu]^2)/(2*(Jout + J*\[Eta]*\[CapitalNu]^2)) - 
            (J*R*\[Eta]*\[CapitalNu]^2)/(2*L*(Jout + J*\[Eta]*\[CapitalNu]^
                 2)) + Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
                (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
                  \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
                  \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2]/
             (2*L*(Jout + J*\[Eta]*\[CapitalNu]^2)))*\[Tau]$11397)*\[Eta]*
         \[CapitalNu]^2*Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*
            (Bout*R + Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*
              \[CapitalNu]^2) + (Bout*L + Jout*R + B*L*\[Eta]*
              \[CapitalNu]^2 + J*R*\[Eta]*\[CapitalNu]^2)^2])/
       (2*(Jout + J*\[Eta]*\[CapitalNu]^2)^2*
        Sqrt[-4*(Jout*L + J*L*\[Eta]*\[CapitalNu]^2)*(Bout*R + 
            Ke*Kt*\[Eta]*\[CapitalNu]^2 + B*R*\[Eta]*\[CapitalNu]^2) + 
          (Bout*L + Jout*R + B*L*\[Eta]*\[CapitalNu]^2 + 
            J*R*\[Eta]*\[CapitalNu]^2)^2]) + DiracDelta[\[Tau]$11397]/
       (Jout + J*\[Eta]*\[CapitalNu]^2)}
 
Attributes[\[Tau]$11397] = {Temporary}
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Bout*(R + L*s) + 
        Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
         \[CapitalNu]^2}, s, SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, 
        "\[Omega]out", Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vbat$, \[Tau]auto$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Bout*(R + L*s) + 
            Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
             \[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Omega]out", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + 
        s*(R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
      SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "\[Theta]out", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vbat$, \[Tau]auto$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu], R + L*s}}, 
           Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + s*(R + L*s)*(Bout + Jout*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Theta]out", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, Bout*(R + L*s) + 
        Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
         \[CapitalNu]^2}, s, SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, 
        "\[Alpha]out", Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vbat$, \[Tau]auto$}, Module[{fn$}, 
      fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, Bout*(R + L*s) + 
            Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*\[Eta]*
             \[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Alpha]out", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
       Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
         \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
       {{"vbat", "\[Tau]auto"}, "\[Omega]", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
           Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
             \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Omega]", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
       Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + s*(R + L*s)*(Bout + Jout*s + 
          (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
       {{"vbat", "\[Tau]auto"}, "\[Theta]", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
           Ke*Kt*s*\[Eta]*\[CapitalNu]^2 + s*(R + L*s)*(Bout + Jout*s + 
              (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Theta]", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
       Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
         \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
       {{"vbat", "\[Tau]auto"}, "vg", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
           Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
             \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "vg", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
       Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
         \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
       {{"vbat", "\[Tau]auto"}, "\[Alpha]", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
           Bout*(R + L*s) + Jout*s*(R + L*s) + (Ke*Kt + (B + J*s)*(R + L*s))*
             \[Eta]*\[CapitalNu]^2}, s, SystemsModelLabels -> 
           {{"vbat", "\[Tau]auto"}, "\[Alpha]", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2, 
         -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
      SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "i", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2, 
             -(Ke*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
          SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "i", Automatic}], 
         {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
         -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
        (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
      SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "\[Tau]", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vbat$, \[Tau]auto$}, 
     Module[{fn$}, fn$ = makeTimeDomainFunctionConvolve[TransferFunctionModel[
          {{{Kt*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
             -(Ke*Kt*\[CapitalNu])}}, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
            (R + L*s)*(Bout + Jout*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}, s, 
          SystemsModelLabels -> {{"vbat", "\[Tau]auto"}, "\[Tau]", 
            Automatic}], {vbat$[#1] & , \[Tau]auto$[#1] & }]; fn$]]
 
makeMotorTimeDomainFunction[model_] := makeMotorTimeDomainFunction[model, 
     makeTimeDomainFunctionConvolve]
 
makeMotorTimeDomainFunction[model_, builder_] := 
    makeMotorTimeDomainFunction[model, builder] = 
     Module[{t}, Function[{vbat, \[Tau]auto}, Module[{fn}, 
        fn = builder[model, {vbat[#1] & , \[Tau]auto[#1] & }]; fn]]]
