package examples.channel

import it.unibo.scafi.config.Grid3DSettings
import it.unibo.scafi.incarnations.BasicAbstractSpatialSimulationIncarnation
import it.unibo.scafi.space.Point3D
import it.unibo.scafi.space.SpaceHelper

import scala.scalajs.js
import scala.scalajs.js.Dynamic.literal
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.util.Random

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
final case class EngineImpl(ncols: Int, nrows: Int, ndepth: Int)(
    stepx: Int,
    stepy: Int,
    stepz: Int
)(proximityThreshold: Int) extends EngineApi:
  import EngineImpl.*

  private val positions: List[Point3D] =
    SpaceHelper.grid3DLocations(Grid3DSettings(
      nrows,
      ncols,
      ndepth,
      stepx,
      stepy,
      stepz,
      tolerance = 0
    ))

  private val ids: IndexedSeq[Int]         = 1 to (ncols * nrows * ndepth)
  private val devsToPos: Map[Int, Point3D] = ids.zip(positions).toMap
  private var colors: Map[Id, Color]       = ids.map(_ -> DefaultColor).toMap

  private object SpatialIncarnation
      extends BasicAbstractSpatialSimulationIncarnation:
    override type P = Point3D
    private trait CustomDistanceStrategy extends DistanceStrategy

    override def buildNewSpace[E](elems: Iterable[(E, P)]): SPACE[E] =
      new Basic3DSpace(elems.toMap) with CustomDistanceStrategy

  import SpatialIncarnation.*

  private object GradientProgram extends AggregateProgram with StandardSensors:
    def main(): String =

      def broadcast[A](source: Boolean, input: A): A =
        val gradient = rep(Double.PositiveInfinity) { distance =>
          mux(source)(0.0) {
            minHoodPlus(nbr(distance) + nbrRange)
          }
        }
        colors += mid() -> calculateColor(gradient)
        input
      
      broadcast(sense[Boolean]("sensor"), "abc")
        
    private def calculateColor(gradient: Double): Int =
      val maxDistance = 1000
      val hue         = (gradient / maxDistance * 360).toInt
      val lightness   = 50 + (gradient / maxDistance * 20).toInt
      hslToRgb(hue, s = 40, lightness)

  private val net = SpaceAwareSimulator(
    space = Basic3DSpace(
      devsToPos,
      proximityThreshold = proximityThreshold
    ),
    devs = devsToPos.view.map { case (id, pos) =>
      id -> new DevInfo(
        id,
        pos,
        lsns = Map.empty,
        nsns => _ => null
      )
    }.toMap,
    simulationSeed = Random.nextLong(),
    randomSensorSeed = Random.nextLong()
  )

  net.addSensor("sensor", false)
  net.chgSensorValue("sensor", Set(1), true)

  @JSExport
  override def executeIterations(): Unit =
    val randomId = ids(Random.nextInt(ids.size))
    net.exec(GradientProgram, GradientProgram.main(), randomId)

  @JSExport
  override def getNodes(): js.Array[js.Dynamic] =
    js.Array(net.devs.map { case (id, devInfo) =>
      literal(
        id = id,
        position = literal(
          x = devInfo.pos.x,
          y = devInfo.pos.y,
          z = devInfo.pos.z
        ),
        label = net.getExport(id).fold(".")(_.root().toString),
        color = colors.getOrElse(id, DefaultColor)
      )
    }.toSeq*)

  @JSExport
  override def getEdges(): js.Array[js.Dynamic] =
    js.Array(net.devs.keys.flatMap { id =>
      net.neighbourhood(id).collect {
        case nbr if id < nbr => literal("source" -> id, "target" -> nbr)
      }
    }.toSeq*)

private object EngineImpl:
  private val DefaultColor: Color = 0xff0000

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
