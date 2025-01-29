package examples.sphere

import it.unibo.scafi.config.Grid3DSettings
import it.unibo.scafi.incarnations.BasicAbstractSpatialSimulationIncarnation
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.SpaceHelper

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.annotation._
import scala.util.Random

/* ----- Cute Params -------
    x, y, z = 10, 10, 5
    edgeDist: 60
--------------------------*/

type Id    = Int
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
  private val ids: IndexedSeq[Int]         = 1 to ncols * nrows * ndepth
  private val devsToPos: Map[Int, Point3D] = ids.zip(positions).toMap
  private var colors: Map[Id, Color]       = ids.map(_ -> 0xff0000).toMap

  private object BasicSpatialIncarnation
      extends BasicAbstractSpatialSimulationIncarnation:
    override type P = Point3D

    private trait MyDistanceStrategy extends DistanceStrategy

    override def buildNewSpace[E](elems: Iterable[(E, P)]): SPACE[E] =
      new Basic3DSpace(elems.toMap) with MyDistanceStrategy

  import BasicSpatialIncarnation.*

  private object Spatial extends AggregateProgram with StandardSensors:
    def main(): MainResult =
      val center  = Point3D(450, 450, 300)
      val radius  = 300.0
      val id      = mid()
      val phi     = Math.acos(1 - 2 * (id.toDouble / net.ids.size))
      val time    = rep(0.0)(_ + 0.01)
      val theta   = Math.PI * (1 + Math.sqrt(5)) * id + time
      val targetX = center.x + radius * Math.sin(phi) * Math.cos(theta)
      val targetY = center.y + radius * Math.sin(phi) * Math.sin(theta)
      val targetZ = center.z + radius * Math.cos(phi)
      Point3D(targetX, targetY, targetZ)
      val currentPosition = net.devs(mid()).pos
      val speed           = 0.1
      val newX = currentPosition.x + (targetX - currentPosition.x) * speed
      val newY = currentPosition.y + (targetY - currentPosition.y) * speed
      val newZ = currentPosition.z + (targetZ - currentPosition.z) * speed
      net.setPosition(id, Point3D(newX, newY, newZ))
      val gradient = rep(Double.PositiveInfinity)(distance =>
        mux(sense[Boolean]("sensor")) {
          0.0
        } {
          minHoodPlus(nbr(distance) + nbrRange)
        }
      )
      val maxDist   = 900
      val hue       = (gradient / maxDist * 360).toInt
      val lightness = 50 + (gradient / maxDist * 20)
      val rgb       = hslToRgb(hue, 40, lightness.toInt)
      colors = colors + (mid() -> rgb)
      ""

  private val net = new SpaceAwareSimulator(
    space =
      new Basic3DSpace(devsToPos, proximityThreshold = proximityThreshold),
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
        color = colors.getOrElse(id, 0xff0000)
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
  val c   = (1 - math.abs(2 * l / 100f - 1)) * s / 100f
  val x   = c * (1 - math.abs(hue % 2 - 1))

  val (r, g, b) = hue match
    case _ if hue < 1 => (c, x, 0f)
    case _ if hue < 2 => (x, c, 0f)
    case _ if hue < 3 => (0f, c, x)
    case _ if hue < 4 => (0f, x, c)
    case _ if hue < 5 => (x, 0f, c)
    case _            => (c, 0f, x)

  val m               = l / 100f - c / 2
  def clamp(v: Float) = (v * 255 + 0.5f).toInt.max(0).min(255)
  (clamp(r + m) << 16) | (clamp(g + m) << 8) | clamp(b + m)
