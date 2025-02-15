package examples.snake

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
  private val positions: List[Point3D] = SpaceHelper.grid3DLocations(
    Grid3DSettings(nrows, ncols, ndepth, stepx, stepy, stepz, tolerance = 0)
  )
  private val ids: IndexedSeq[Int] = 1 to ncols * nrows * ndepth
  private val devsToPos: Map[Int, Point3D] = ids.zip(positions).toMap
  private var colors: Map[Id, Color] = ids.map(_ -> 0xFF0000).toMap

  private object BasicSpatialIncarnation
    extends BasicAbstractSpatialSimulationIncarnation:
    override type P = Point3D

    private trait MyDistanceStrategy extends DistanceStrategy

    override def buildNewSpace[E](elems: Iterable[(E, P)]): SPACE[E] =
      new Basic3DSpace(elems.toMap) with MyDistanceStrategy

  import BasicSpatialIncarnation.*

  private object Spatial extends AggregateProgram with StandardSensors:
    override def main() =
      val snakePhase = rep(0)(p => p + 1)
      val distance = rep(Double.PositiveInfinity)(d =>
        mux(sense[Boolean]("sensor")) {
          0.0
        } {
          minHoodPlus(nbr(d) + 1)
        }
      )
      val candidate: Int = mux(distance == snakePhase) { mid() } { Int.MaxValue }
      val leader = foldhood(Int.MaxValue)(math.min)(nbr(candidate))
      val snakeActive = (mid() == leader) && (distance == snakePhase)
      colors += mid() -> (if (snakeActive) 0xFFFF else 0xFF0000)
    
      snakeActive


  private val net = new SpaceAwareSimulator(
    space = new Basic3DSpace(devsToPos, proximityThreshold = proximityThreshold),
    devs = devsToPos.map { case (d, p) =>
      d -> new DevInfo(d, p, lsns = Map.empty, nsns => nbr => null)
    },
    simulationSeed = System.currentTimeMillis(),
    randomSensorSeed = System.currentTimeMillis()

  )

  net.addSensor(name = "sensor", value = false)
  net.chgSensorValue(name = "sensor", ids = Set(1), value = true)


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
        color = colors.getOrElse(id, 0xFF0000)
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


def hslToRgb(h: Int, s: Int, l: Int): Int =
  val hue = (h % 360) / 60f
  val c = (1 - math.abs(2 * l/100f - 1)) * s/100f
  val x = c * (1 - math.abs(hue % 2 - 1))

  val (r, g, b) = hue match {
    case _ if hue < 1 => (c, x, 0f)
    case _ if hue < 2 => (x, c, 0f)
    case _ if hue < 3 => (0f, c, x)
    case _ if hue < 4 => (0f, x, c)
    case _ if hue < 5 => (x, 0f, c)
    case _            => (c, 0f, x)
  }

  val m = l/100f - c/2
  def clamp(v: Float) = (v * 255 + 0.5f).toInt.max(0).min(255)

  (clamp(r + m) << 16) | (clamp(g + m) << 8) | clamp(b + m)
