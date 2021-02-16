package yak

import scadla._
import scadla.EverythingIsIn.{millimeters, degrees}
import InlineOps._

sealed trait Item {
  def move(x: Double = 0.0, y: Double = 0.0): Item
  def renderUnion: Solid
  def renderDiff: Solid
}

case class Key(
    val x: Double = 0.0,
    val y: Double = 0.0,
    val keyWidth: Double = 14.0,
    val borderWidth: Double = 5.0,
    val wallWidth: Double = 5.0
) extends Item {

  override def move(x: Double = 0.0, y: Double = 0.0) = copy(
    x = this.x + x,
    y = this.y + y
  )

  override def renderUnion: Solid = {
    val borderCubeWidth = keyWidth + (borderWidth * 2)

    val borderPart =
      Cube(
        width = borderCubeWidth,
        depth = borderWidth,
        height = wallWidth
      )
        .moveX(-borderCubeWidth / 2)
        .moveY(-borderCubeWidth / 2)

    val borders = Union(
      borderPart.rotateZ(0),
      borderPart.rotateZ(90),
      borderPart.rotateZ(180),
      borderPart.rotateZ(270)
    )

    borders.move(x, y, 0)
  }

  override def renderDiff: Solid = {
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

    val gaps = Union(
      gap.rotateZ(0),
      gap.rotateZ(180)
    )

    gaps.move(x, y, 0)
  }
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

  override def renderUnion: Solid =
    Union(keys.map(_.renderUnion): _*)

  override def renderDiff: Solid =
    Union(keys.map(_.renderDiff): _*)
}

case class Layout(
    val items: Item*
) {
  def render: Solid =
    Difference(
      Union(items.map(_.renderUnion): _*),
      Union(items.map(_.renderDiff): _*)
    )
}

object Column {
  def numberOfKeys(numberOfKeys: Int, keysSpacing: Double = 19.0): Column =
    Column(
      (0 to numberOfKeys - 1).map(i => Key(y = keysSpacing * i)).toList
    )
}
