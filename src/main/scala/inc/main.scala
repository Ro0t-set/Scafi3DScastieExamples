package inc

import it.unibo.scafi.config.Grid3DSettings
import it.unibo.scafi.incarnations.BasicAbstractSpatialSimulationIncarnation
import it.unibo.scafi.space.{Point3D, SpaceHelper}
import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.annotation.*
import scala.util.Random

type Id = Int
type Color = Int
type Label = String

final case class Position(x: Double, y: Double, z: Double)
final case class Node(id: Id, position: Position, label: Label, color: Color)

trait EngineApi:
  def executeIterations(): Unit
  def getNodes(): js.Array[js.Dynamic]
  def getEdges(): js.Array[js.Dynamic]

@JSExportTopLevel("EngineImpl")
case class EngineImpl(ncols: Int, nrows: Int, ndepth: Int)(
  stepx: Int,
  stepy: Int,
  stepz: Int
)(proximityThreshold: Int) extends EngineApi:

  private object BasicSpatialIncarnation
    extends BasicAbstractSpatialSimulationIncarnation:
    override type P = Point3D
    private trait MyDistanceStrategy extends DistanceStrategy
    override def buildNewSpace[E](elems: Iterable[(E, P)]): SPACE[E] =
      new Basic3DSpace(elems.toMap) with MyDistanceStrategy

  private val positions: List[Point3D] = SpaceHelper.grid3DLocations(
    Grid3DSettings(nrows, ncols, ndepth, stepx, stepy, stepz, tolerance = 0)
  )
  private val ids: IndexedSeq[Int] = 1 to ncols * nrows * ndepth
  private val devsToPos: Map[Int, Point3D] = ids.zip(positions).toMap

  import BasicSpatialIncarnation.*

  private object Spatial extends AggregateProgram with StandardSensors:
    def main()  = rep(0){_+1}


  private val net = new SpaceAwareSimulator(
    space =
      new Basic3DSpace(devsToPos, proximityThreshold = proximityThreshold),
    devs = devsToPos.map { case (d, p) =>
      d -> new DevInfo(d, p, lsns = Map.empty, nsns => nbr => null)
    },
    simulationSeed = System.currentTimeMillis(),
    randomSensorSeed = System.currentTimeMillis()
  )

  @JSExport
  override def executeIterations(): Unit =
    val nextIdToRun = ids(Random.nextInt(ids.size))
    net.exec(Spatial, Spatial.main(), nextIdToRun)

  @JSExport
  override def getNodes(): js.Array[js.Dynamic] =

    val nodes = net.devs.map { case (id, devInfo) =>
      literal(
        id = id,
        position = literal(
          x = devInfo.pos.x,
          y = devInfo.pos.y,
          z = devInfo.pos.z
        ),
        label = net.getExport(id).fold(".")(e => s"${e.root()}"),
        color = 0xFF0000
      )
    }.toArray

    js.Array(nodes: _*)

  @JSExport
  override def getEdges(): js.Array[js.Dynamic] =
    val edges = net.devs.keys.flatMap { id =>
      net.neighbourhood(id).collect {
        case nbr if id < nbr => literal("source" -> id, "target" -> nbr)
      }
    }.toSeq

    js.Array(edges: _*)