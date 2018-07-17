(* Loess Regression *)

(*
 * We have two ways we can determine which points we use for fitting The first, which seems to be the traditional way, 
 * is to fit at each point at which we actually have data. The second is to space a number of points (possibly far fewer 
 * than the number of samples) equally across the domain of the data (we force the inclusion of the extremes so as to 
 * definitely cover the whole domain of the data). In practice, the two approaches seem to have very similar results 
 * if the data is dense.
 *)
deleteDuplicates[data_] := 
 DeleteDuplicates[data, Abs[#1 - #2] < 10^-9 &]
 
fitPoints[data_, interpolationFraction_] := 
 Block[{min = data[[All, 1]] // Min, max = data[[All, 1]] // Max}, 
  If [0 == interpolationFraction,
   data[[All, 1]] // deleteDuplicates // Sort,
   Union[
      Range[min, 
       max, (max - min)/Max[3, Length[data]* interpolationFraction]],  
      {min, max}] // deleteDuplicates // Sort
   ]]
   
(*
 * We define a function that computes the neighborhood of a given point using a particular fraction of the data.
 *)   
neighborhoodIndices[data_, xCenter_, \[Alpha]_] := Block[{n},
   n = Min[Length[data], Ceiling[\[Alpha] * Length[data]]];
   Ordering[Abs[#[[1]] - xCenter] & /@ data, n] // Sort];
   
dataNeighborhood[data_, xCenter_, \[Alpha]_] := 
 data[[neighborhoodIndices[data, xCenter, \[Alpha]]]]   
   
(*
 *The tricube function is the weighting function typically used in Loess smoothing, though other approaches are possible.
 *)   
tricube = Compile [{u}, If[Abs[u] < 1, (1 - Abs[u]^3)^3, 0]];
tricubeRange = 
  Compile[{u, uCenter, uHalfWidth}, 
   Block[{uMin = uCenter - uHalfWidth},  
    tricube[ (u - uMin) / (uHalfWidth * 2) * 2 - 1]]];

(*
 * We define a function that computes the weights for a range of data; in practice, we will apply this to subsets of 
 *data corresponding to fit neighborhoods.
*)
weights[data_, xCenter_, \[Alpha]_] := Block[{xMin, xMax, xHalfWidth},
   xMin = data[[1, 1]]; xMax = data[[Length[data], 1]];
  xHalfWidth = Max[xCenter - xMin, xMax - xCenter];
  tricubeRange[#[[1]], xCenter, xHalfWidth] & /@ data]
      

(*
 * We define a function that performs the regression at a particular fit point, then a second function which iterates over all such points.
 *)      
regressAtX[data_, x_, \[Alpha]_, degree_] := Block[{y, hood, fit},
  hood = dataNeighborhood[data, x, \[Alpha]];
  fit = LinearModelFit[hood, y^Range[0, degree], y, 
    Weights -> weights[hood, x, \[Alpha]]];
  fit["BestFit"] /. y -> x]
  
(*
 * The regress function returns the (Subscript[x, i],Subscript[y, i]) pairs of the smoothed fit. This is the main 
 * function one calls to carry out a regression. The first parameter is the data, the second is the fraction of the
 * data to use in generating each neighborhood, the third parameter is the degree of interpolation polynomials to use 
 * in each neighborhood, and the last parameter controls which fit points are used: zero indicates use the data 
 * points as fit points, while a non zero value indicates that that fraction of the number of data points are to be used 
 * as equally spaced fit points across the range of the data.
 *)  
regress[unsortedData_, \[Alpha]_, degree_, interpolationFraction_] := 
 Block[{sortedData, result, xx},
  sortedData = Sort[unsortedData, #1[[1]] < #2[[1]] &];
  result = { #, regressAtX[sortedData, #, \[Alpha], degree] } & /@ 
    fitPoints[sortedData, interpolationFraction];
  result]
  
sampleWithNoise[fn_, {xFirst_, xLast_}, n_, noiseScale_] := 
 Module[{xx, yy},
  xx = N[Range[xFirst, xLast, (xLast - xFirst) / n]];
  yy = fn[#] + noiseScale RandomVariate[NormalDistribution[]] & /@ xx;
  Transpose[{xx, yy}]
  ]     