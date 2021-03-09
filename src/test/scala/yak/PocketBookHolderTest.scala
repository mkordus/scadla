package yak

import org.scalatest.flatspec.AnyFlatSpec
import scadla._
import scadla.EverythingIsIn.{millimeters, degrees}
import InlineOps._

class PocketBookHolderTest extends AnyFlatSpec {

  it should "render" in renderTest {
    val wallThicness = 2.2

    val readerY = 8.0
    val readerX = 137.0
    val readerZ = 195.0

    val holderZ = 27.0
    val holderSideMargin = 15.0
    val holderTopMargin = 12.0
    val holderExtension = 40.0

    val reader = Cube(
      width = readerX,
      depth = readerY,
      height = readerZ
    )
      .moveX(-readerX / 2)
      .moveY(-readerY / 2)
      .moveZ(wallThicness)

    val baseX = readerX
    val baseY = readerY + wallThicness * 2
    val baseZ = holderZ + wallThicness

    val base = Cube(
      width = baseX,
      depth = baseY,
      height = baseZ + holderExtension
    )
      .moveX(-baseX / 2)
      .moveY(-baseY / 2)
      .moveZ(-holderExtension)

    val baseCylinderRadius = baseY / 2
    val baseCylinder = Cylinder(
      radius = baseCylinderRadius,
      height = baseZ + holderExtension
    )
      .moveX(baseX / 2)
      .moveZ(-holderExtension)

    val insideBaseCylinder =
      Cylinder(
        radius = baseCylinderRadius,
        height = holderExtension + wallThicness
      )
        .moveX(baseX / 2 - holderSideMargin)
        .moveZ(-holderExtension)

    val windowX = baseX - holderSideMargin * 2
    val windowY = baseY
    val windowZ = baseZ - holderTopMargin + holderExtension
    val window = Cube(windowX, windowY, windowZ)
      .moveX(-windowX / 2)
      .moveY(-windowY / 2)
      .moveZ(-holderExtension)

    Union(
      Difference(
        Union(
          base,
          baseCylinder,
          baseCylinder.rotateZ(180)
        ),
        reader,
        window
      ),
      insideBaseCylinder,
      insideBaseCylinder.rotateZ(180)
    )
      .rotateY(180)
  }

  private def renderTest(obj: Solid): Unit = {
    val renderingOption = List("$fn=100;")
    val renderer = new backends.OpenSCAD(renderingOption)
    // renderer.view(obj)
    renderer.toSTL(obj, "holder.stl")
  }
}
