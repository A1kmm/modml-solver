import ModML.Units.UnitsDAEModel
import ModML.Units.SIUnits
import ModML.Core.BasicDAESolver
import qualified ModML.Core.BasicDAEModel

mymodelBuilder = do
  xcoord <- newNamedRealVariable uMetre "x"
  ycoord <- newNamedRealVariable uMetre "y"
  let gravity = realConstant (uMetre $*$ (uSecond $**$ (-2))) (-9.8)
  let initialPosition = (realConstant uMetre 0, realConstant uMetre 0)
  let initialPolarVelocity = (
                              realConstant (uMetre $*$ (uSecond $**$ (-1))) 100,
                              realConstant dimensionless (pi / 4)
                             )
  let time = boundVariable
  let time0 = realConstant uSecond 0
  newBoundaryEq (time .==. time0) xcoord {- == -} (fst initialPosition)
  newBoundaryEq (time .==. time0) ycoord {- == -} (snd initialPosition)
  newBoundaryEq (time .==. time0) (derivative ycoord) {- == -} (sinX (snd initialPolarVelocity) .*. fst initialPolarVelocity)
  derivative xcoord `newEq` (cosX (snd initialPolarVelocity) .*. fst initialPolarVelocity)
  (derivative (derivative ycoord)) `newEq` gravity

mymodel :: ModML.Core.BasicDAEModel.BasicDAEModel
mymodel = buildModel uSecond mymodelBuilder
main = print $ modelToResults mymodel defaultSolverParameters
