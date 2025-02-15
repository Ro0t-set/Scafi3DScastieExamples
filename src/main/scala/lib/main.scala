package lib

import SpatialIncarnation.*
import it.unibo.scafi.incarnations.BasicAbstractSpatialSimulationIncarnation
import it.unibo.scafi.incarnations.BasicSimulationIncarnation.Basic3DSpace
import it.unibo.scafi.space.Point3D

private object SpatialIncarnation
  extends BasicAbstractSpatialSimulationIncarnation:
  override type P = Point3D
  private trait CustomDistanceStrategy extends DistanceStrategy
  override def buildNewSpace[E](elems: Iterable[(E, P)]): SPACE[E] =
    new Basic3DSpace(elems.toMap) with CustomDistanceStrategy

private object MainProgram extends AggregateProgram with StandardSensors:
  def main(): MainResult = rep(0)(_ + 1)