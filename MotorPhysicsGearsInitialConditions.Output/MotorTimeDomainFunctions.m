motorPosition = Function[{vappfn$, \[Tau]appfn$}, 
     Module[{fn$}, fn$ = Function[{aTime$}, 
         makeTimeDomainFunctionConvolve[TransferFunctionModel[
             {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
              {{s*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
                s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                    Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)), 
                s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                    Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2))}}}, s, 
             SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Theta]", 
               Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
           aTime$][[1]]]; fn$]]
 
Attributes[vappfn$] = {Temporary}
 
Attributes[\[Tau]appfn$] = {Temporary}
 
Attributes[fn$] = {Temporary}
 
Attributes[aTime$] = {Temporary}
 
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
 
parameterAssumptions = {Element[Bafter, Reals], Element[Jafter, Reals], 
     Element[vapp0, Reals], Element[\[CapitalDelta]vappConst, Reals], 
     Element[\[CapitalDelta]\[Tau]appConst, Reals], 
     Element[\[Tau]app0, Reals], Element[i[_], Reals], Element[vg[_], Reals], 
     Element[\[Alpha][_], Reals], Element[\[CapitalDelta]vapp[_], Reals], 
     Element[\[CapitalDelta]\[Tau]app[_], Reals], Element[\[Theta][_], 
      Reals], Element[\[Tau][_], Reals], Element[\[Tau]after[_], Reals], 
     Element[\[Omega][_], Reals], Ke > 0, Kt > 0, L > 0, R > 0, \[Eta] > 0, 
     \[CapitalNu] > 0, B >= 0, J >= 0, t >= 0}
 
motorVelocity = Function[{vappfn$, \[Tau]appfn$}, 
     Module[{fn$}, fn$ = Function[{aTime$}, 
         makeTimeDomainFunctionConvolve[TransferFunctionModel[
             {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
              {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                  \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                    \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "\[Omega]", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
motorAcceleration = Function[{vappfn$, \[Tau]appfn$}, 
     Module[{fn$}, fn$ = Function[{aTime$}, 
         makeTimeDomainFunctionConvolve[TransferFunctionModel[
             {{{s*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
              {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                  \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                    \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "\[Alpha]", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
motorCurrent = Function[{vappfn$, \[Tau]appfn$}, 
     Module[{fn$}, fn$ = Function[{aTime$}, 
         makeTimeDomainFunctionConvolve[TransferFunctionModel[
             {{{Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
                   Ke*\[Tau]app0), Bafter + Jafter*s + (B + J*s)*\[Eta]*
                  \[CapitalNu]^2, -(Ke*\[CapitalNu])}}, 
              {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                  \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                    \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "i", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
motorEMF = Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{Ke*\[CapitalNu]*(Kt*vapp0*\[Eta]*
                   \[CapitalNu] + R*\[Tau]app0), Ke*Kt*\[Eta]*\[CapitalNu]^2, 
                Ke*(R + L*s)*\[CapitalNu]}}, {{Bafter*R + (Ke*Kt + B*R)*
                  \[Eta]*\[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^
                     2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                   Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
             SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "vg", 
               Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
           aTime$][[1]]]; fn$]]
 
motorTorque = Function[{vappfn$, \[Tau]appfn$}, 
     Module[{fn$}, fn$ = Function[{aTime$}, 
         makeTimeDomainFunctionConvolve[TransferFunctionModel[
             {{{Kt*(Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*
                     \[CapitalNu] - Ke*\[Tau]app0)), Kt*(Bafter + Jafter*s + 
                  (B + J*s)*\[Eta]*\[CapitalNu]^2), -(Ke*Kt*\[CapitalNu])}}, 
              {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                  \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                    \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "\[Tau]", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
motorPositionAfter = Function[{vappfn$, \[Tau]appfn$}, 
     Module[{fn$}, fn$ = Function[{aTime$}, 
         makeTimeDomainFunctionConvolve[TransferFunctionModel[
             {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*
                 \[CapitalNu], R + L*s}}, {{s*(Bafter*R + (Ke*Kt + B*R)*
                   \[Eta]*\[CapitalNu]^2), s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                     \[CapitalNu]^2)), s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                     \[CapitalNu]^2))}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "\[Theta]after", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
motorVelocityAfter = Function[{vappfn$, \[Tau]appfn$}, 
     Module[{fn$}, fn$ = Function[{aTime$}, 
         makeTimeDomainFunctionConvolve[TransferFunctionModel[
             {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*
                 \[CapitalNu], R + L*s}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                  (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
             SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, 
               "\[Omega]after", Automatic}], {1 & , vappfn$[#1] & , 
             \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
motorAccelerationAfter = Function[{vappfn$, \[Tau]appfn$}, 
     Module[{fn$}, fn$ = Function[{aTime$}, 
         makeTimeDomainFunctionConvolve[TransferFunctionModel[
             {{{s*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
                Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, 
              {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                  \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                    \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "\[Alpha]after", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*\[CapitalNu], 
         R + L*s}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
         Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
      s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Omega]after", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{Kt*vapp0*\[Eta]*\[CapitalNu] + 
                 R*\[Tau]app0, Kt*\[Eta]*\[CapitalNu], R + L*s}}, 
              {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                  \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                    \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "\[Omega]after", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0, Kt*\[Eta]*\[CapitalNu], 
         R + L*s}}, {{s*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
         s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
             (B + J*s)*\[Eta]*\[CapitalNu]^2)), 
         s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
             (B + J*s)*\[Eta]*\[CapitalNu]^2))}}}, s, 
      SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Theta]after", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{Kt*vapp0*\[Eta]*\[CapitalNu] + 
                 R*\[Tau]app0, Kt*\[Eta]*\[CapitalNu], R + L*s}}, 
              {{s*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
                s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                    Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)), 
                s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                    Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2))}}}, s, 
             SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, 
               "\[Theta]after", Automatic}], {1 & , vappfn$[#1] & , 
             \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{s*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
         Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, 
       {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
         Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
      s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Alpha]after", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{s*(Kt*vapp0*\[Eta]*\[CapitalNu] + 
                  R*\[Tau]app0), Kt*s*\[Eta]*\[CapitalNu], s*(R + L*s)}}, 
              {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                  \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                    \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "\[Alpha]after", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
         Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
       {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
         Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
      s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Omega]", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{\[CapitalNu]*(Kt*vapp0*\[Eta]*
                   \[CapitalNu] + R*\[Tau]app0), Kt*\[Eta]*\[CapitalNu]^2, 
                (R + L*s)*\[CapitalNu]}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                  (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
             SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Omega]", 
               Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
           aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
         Kt*\[Eta]*\[CapitalNu]^2, (R + L*s)*\[CapitalNu]}}, 
       {{s*(Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2), 
         s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
             (B + J*s)*\[Eta]*\[CapitalNu]^2)), 
         s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
             (B + J*s)*\[Eta]*\[CapitalNu]^2))}}}, s, 
      SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Theta]", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{\[CapitalNu]*(Kt*vapp0*\[Eta]*
                   \[CapitalNu] + R*\[Tau]app0), Kt*\[Eta]*\[CapitalNu]^2, 
                (R + L*s)*\[CapitalNu]}}, {{s*(Bafter*R + (Ke*Kt + B*R)*
                   \[Eta]*\[CapitalNu]^2), s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                     \[CapitalNu]^2)), s*(Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                  (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
                     \[CapitalNu]^2))}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "\[Theta]", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Ke*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
         Ke*Kt*\[Eta]*\[CapitalNu]^2, Ke*(R + L*s)*\[CapitalNu]}}, 
       {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
         Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
      s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "vg", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{Ke*\[CapitalNu]*(Kt*vapp0*\[Eta]*
                   \[CapitalNu] + R*\[Tau]app0), Ke*Kt*\[Eta]*\[CapitalNu]^2, 
                Ke*(R + L*s)*\[CapitalNu]}}, {{Bafter*R + (Ke*Kt + B*R)*
                  \[Eta]*\[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^
                     2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                   Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
             SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "vg", 
               Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
           aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{s*\[CapitalNu]*(Kt*vapp0*\[Eta]*\[CapitalNu] + R*\[Tau]app0), 
         Kt*s*\[Eta]*\[CapitalNu]^2, s*(R + L*s)*\[CapitalNu]}}, 
       {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
         Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
      s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Alpha]", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{s*\[CapitalNu]*(Kt*vapp0*\[Eta]*
                   \[CapitalNu] + R*\[Tau]app0), Kt*s*\[Eta]*\[CapitalNu]^2, 
                s*(R + L*s)*\[CapitalNu]}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                  (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
             SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Alpha]", 
               Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
           aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
            Ke*\[Tau]app0), Bafter + Jafter*s + (B + J*s)*\[Eta]*
           \[CapitalNu]^2, -(Ke*\[CapitalNu])}}, 
       {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
         Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
      s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "i", Automatic}], 
     makeTimeDomainFunctionConvolve] = Function[{vappfn$, \[Tau]appfn$}, 
     Module[{fn$}, fn$ = Function[{aTime$}, 
         makeTimeDomainFunctionConvolve[TransferFunctionModel[
             {{{Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
                   Ke*\[Tau]app0), Bafter + Jafter*s + (B + J*s)*\[Eta]*
                  \[CapitalNu]^2, -(Ke*\[CapitalNu])}}, 
              {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*
                  \[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + (B + J*s)*
                    \[Eta]*\[CapitalNu]^2)}}}, s, SystemsModelLabels -> 
              {{"1", "vapp", "\[Tau]app"}, "i", Automatic}], 
            {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*(Bafter*vapp0 + \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
             Ke*\[Tau]app0)), Kt*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
            \[CapitalNu]^2), -(Ke*Kt*\[CapitalNu])}}, 
       {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
         Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
      s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Tau]", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{Kt*(Bafter*vapp0 + \[CapitalNu]*
                   (B*vapp0*\[Eta]*\[CapitalNu] - Ke*\[Tau]app0)), 
                Kt*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                -(Ke*Kt*\[CapitalNu])}}, {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*
                  \[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*
                  (Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
                   (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
             SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Tau]", 
               Automatic}], {1 & , vappfn$[#1] & , \[Tau]appfn$[#1] & }][
           aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[TransferFunctionModel[
      {{{Kt*\[Eta]*\[CapitalNu]*(Bafter*vapp0 + \[CapitalNu]*
            (B*vapp0*\[Eta]*\[CapitalNu] - Ke*\[Tau]app0)), 
         Kt*\[Eta]*\[CapitalNu]*(Bafter + Jafter*s + (B + J*s)*\[Eta]*
            \[CapitalNu]^2), -(Ke*Kt*\[Eta]*\[CapitalNu]^2)}}, 
       {{Bafter*R + (Ke*Kt + B*R)*\[Eta]*\[CapitalNu]^2, 
         Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + Jafter*s + 
            (B + J*s)*\[Eta]*\[CapitalNu]^2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
          (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, 
      s, SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, "\[Tau]after", 
        Automatic}], makeTimeDomainFunctionConvolve] = 
    Function[{vappfn$, \[Tau]appfn$}, Module[{fn$}, 
      fn$ = Function[{aTime$}, makeTimeDomainFunctionConvolve[
            TransferFunctionModel[{{{Kt*\[Eta]*\[CapitalNu]*(Bafter*vapp0 + 
                  \[CapitalNu]*(B*vapp0*\[Eta]*\[CapitalNu] - 
                    Ke*\[Tau]app0)), Kt*\[Eta]*\[CapitalNu]*(Bafter + 
                  Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2), 
                -(Ke*Kt*\[Eta]*\[CapitalNu]^2)}}, {{Bafter*R + (Ke*Kt + B*R)*
                  \[Eta]*\[CapitalNu]^2, Ke*Kt*\[Eta]*\[CapitalNu]^2 + 
                 (R + L*s)*(Bafter + Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^
                     2), Ke*Kt*\[Eta]*\[CapitalNu]^2 + (R + L*s)*(Bafter + 
                   Jafter*s + (B + J*s)*\[Eta]*\[CapitalNu]^2)}}}, s, 
             SystemsModelLabels -> {{"1", "vapp", "\[Tau]app"}, 
               "\[Tau]after", Automatic}], {1 & , vappfn$[#1] & , 
             \[Tau]appfn$[#1] & }][aTime$][[1]]]; fn$]]
 
makeMotorTimeDomainFunction[model_] := makeMotorTimeDomainFunction[model, 
     makeTimeDomainFunctionConvolve]
 
makeMotorTimeDomainFunction[model_, builder_] := 
    makeMotorTimeDomainFunction[model, builder] = 
     Module[{t}, Function[{vappfn, \[Tau]appfn}, Module[{fn}, 
        fn = Function[{aTime}, builder[model, {1 & , vappfn[#1] & , 
               \[Tau]appfn[#1] & }][aTime][[1]]]; fn]]]
