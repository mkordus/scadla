import org.scalatest.flatspec.AnyFlatSpec
import scadla._
import scadla.EverythingIsIn.{millimeters, degrees}
import InlineOps._

class YakTest extends AnyFlatSpec {

  val keyBorderWidth: Double = 4.0

  sealed trait Item {
    def move(x: Double = 0.0, y: Double = 0.0): Item
    def renderUnion: Solid
  }

  case class Key(
      val x: Double = 0.0,
      val y: Double = 0.0,
      val keyWidth: Double = 14.0,
      val borderWidth: Double = 4.0,
      val wallWidth: Double = 4.0
  ) extends Item {

    override def move(x: Double = 0.0, y: Double = 0.0) = copy(
      x = this.x + x,
      y = this.y + y
    )

    override def renderUnion: Solid =
      drawKeyBorder(
        borderWidth,
        wallWidth,
        keyWidth
      ).move(x, y, 0)
  }

  object Key {
    def move(x: Double = 0.0, y: Double = 0.0): Key =
      Key(x = x, y = y)
  }

  case class Column(
      val keys: List[Key]
  ) extends Item {
    def move(x: Double, y: Double) = copy(
      keys = keys.map(_.move(x, y))
    )

    override def renderUnion: Solid = {
      val renderedkeys = keys.map(_.renderUnion)
      Union(renderedkeys: _*)
    }
  }

  case class Layout(
      val items: Item*
  ) {
    def render: Solid = {
      val renderedItems = items.map(_.renderUnion)
      Union(renderedItems: _*)
    }
  }

  object Column {
    def numberOfKeys(numberOfKeys: Int, keysSpacing: Double = 19.0): Column =
      Column(
        (0 to numberOfKeys - 1).map(i => Key(y = keysSpacing * i)).toList
      )
  }

  it should "render" in renderTest {
    val spacing = 19.0

    val layout = Layout(
      Key
        .move(x = spacing * -1, y = -6.0 - spacing),
      Column
        .numberOfKeys(5)
        .move(x = spacing * 0, y = -spacing),
      Column
        .numberOfKeys(5)
        .move(x = spacing * 1, y = 3.0 - spacing),
      Column
        .numberOfKeys(4)
        .move(x = spacing * 2, y = 5.0),
      Column
        .numberOfKeys(4)
        .move(x = spacing * 3, y = 3.0),
      Column
        .numberOfKeys(4)
        .move(x = spacing * 4, y = -2.0),
      Column
        .numberOfKeys(3)
        .move(x = spacing * 5, y = -4.0)
    )

    layout.render
  }

  def drawKeyBorder(
      borderWidth: Double = 2.5,
      wallWidth: Double = 4.0,
      keyWidth: Double = 14.0
  ) = {
    val borderCubeWidth = keyWidth + (borderWidth * 2)

    val borderPart =
      Cube(
        width = borderCubeWidth,
        depth = borderWidth,
        height = wallWidth
      )
        .moveX(-borderCubeWidth / 2)
        .moveY(-borderCubeWidth / 2)

    val gapDepth = 1.2
    val gapWidth = 5.0

    val gap =
      Cube(
        width = gapWidth,
        depth = gapDepth,
        height = wallWidth - 2.0
      )
        .moveX(-gapWidth / 2)
        .moveY((-keyWidth - gapDepth) / 2)

    val borders = Union(
      borderPart.rotateZ(0),
      borderPart.rotateZ(90),
      borderPart.rotateZ(180),
      borderPart.rotateZ(270)
    )

    val gaps = Union(
      gap.rotateZ(0),
      gap.rotateZ(180)
    )

    Difference(borders, gaps)
  }

  private def renderTest(obj: Solid): Unit = {
    val renderingOption = List("$fn=100;")
    val renderer = new backends.OpenSCAD(renderingOption)
    renderer.view(obj)
    // renderer.toSTL(obj, "v1.stl")
  }
}
