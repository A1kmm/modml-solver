module ModML.Solver.ModelTransformations
where

import Data.List
import qualified ModML.Core.BasicDAEModel as B
import qualified Data.Map as M

data Transformation = DropConstraint String | FixVariable String Double deriving (Eq, Ord, Show)

applyTransformations = foldl' applyTransformation

findAnnotationByPredicateAndObject :: (Show a, Show b) => B.BasicDAEModel -> a -> b -> Maybe String
findAnnotationByPredicateAndObject model pred obj =
  let
    found = find (\((_,p), o) -> p == (show pred) && o == (show obj)) (M.toList $ B.annotations model)
  in
   case found
   of
     Nothing -> Nothing
     Just ((s, _), _) -> Just s

getVariableByName name model =
  do
    varName <- findAnnotationByPredicateAndObject model "nameIs" name
    find (\v -> show v == varName) (B.variables model)

applyTransformation :: B.BasicDAEModel -> Transformation -> B.BasicDAEModel
applyTransformation mod (DropConstraint varName) =
  case getVariableByName varName mod
  of
   Nothing -> mod
   Just v -> mod {B.equations = foldl' (dropConstraintFromEq v) [] (B.equations mod), 
                  B.boundaryEquations = foldl' (dropConstraintFromBEq v) [] (B.boundaryEquations mod) }
applyTransformation mod (FixVariable varName varVal) =
  case getVariableByName varName mod
  of
   Nothing -> mod
   Just v -> mod {B.equations = (B.RealEquation (B.Derivative $ B.RealVariableE v) (B.RealConstant 0)):(B.equations mod), 
                  B.boundaryEquations = (B.Equal (B.RealConstant 0) B.BoundVariableE, B.RealEquation (B.RealVariableE v) (B.RealConstant varVal)):(B.boundaryEquations mod)}
  
dropConstraintFromEq v l (B.RealEquation (B.Derivative (B.RealVariableE v')) _)
  | v' == v = l
dropConstraintFromEq v l (B.RealEquation _ (B.Derivative (B.RealVariableE v')))
  | v' == v = l
dropConstraintFromEq v l (B.RealEquation (B.RealVariableE v') _)
  | v' == v = l
dropConstraintFromEq v l (B.RealEquation _ (B.RealVariableE v'))
  | v' == v = l
dropConstraintFromEq _ l eqn = eqn:l

dropConstraintFromBEq v l (_, B.RealEquation (B.RealVariableE v') _)
  | v' == v = l
dropConstraintFromBEq v l (_, B.RealEquation _ (B.RealVariableE v'))
  | v' == v = l
dropConstraintFromBEq _ l eqn = eqn:l
