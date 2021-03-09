package yak

import org.scalatest.flatspec.AnyFlatSpec
import scadla._
import scadla.EverythingIsIn.{millimeters, degrees}
import InlineOps._

class PocketBookHolderTest extends AnyFlatSpec {

  it should "render" in renderTest {
    val wallThicness = 2.2

    val readerY = 8.0
    val readerX = 137.2
    val readerZ = 195.0
    val readerZScreen = 27.0
    val readerXScreen = 121.0

    val holderZ = 37.0
    val holderSideMargin = 15.0
    val holderTopMargin = 8.0

    val handleZ = 80.0

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
      height = baseZ
    )
      .moveX(-baseX / 2)
      .moveY(-baseY / 2)

    val baseCylinderRadius = baseY / 2
    val baseCylinder = Cylinder(
      radius = baseCylinderRadius,
      height = baseZ
    )
      .moveX(baseX / 2)

    val buttonsSpaceX = baseX - holderSideMargin * 2
    val buttonsSpaceY = baseY
    val buttonsSpaceZ = readerZScreen - holderTopMargin
    val buttonsSpace = Cube(buttonsSpaceX, buttonsSpaceY, buttonsSpaceZ)
      .moveX(-buttonsSpaceX / 2)
      .moveY(-buttonsSpaceY / 2 + wallThicness)

    val screenSpace = Cube(readerXScreen, baseY, holderZ - readerZScreen)
      .moveX(-readerXScreen / 2)
      .moveY(-baseY / 2 + wallThicness)
      .moveZ(readerZScreen + wallThicness)

    val handle = Hull(
      Union(
        Cylinder(
          radius = baseCylinderRadius,
          height = handleZ
        )
          .moveX(baseX / 2),
        Sphere(
          radius = baseCylinderRadius
        )
          .moveX(baseX / 2),
        Cylinder(
          radius = baseCylinderRadius,
          height = handleZ
        )
          .moveX(baseX / 2 - holderSideMargin),
        Sphere(
          radius = baseCylinderRadius
        )
          .moveX(baseX / 2 - holderSideMargin)
      )
    ).moveZ(-handleZ + wallThicness)

    Union(
      Difference(
        Union(
          base,
          baseCylinder,
          baseCylinder.rotateZ(180)
        ),
        reader,
        buttonsSpace,
        screenSpace
      ),
      handle,
      handle.rotateZ(180)
    )
      .rotateY(180)
  }

  private def renderTest(obj: Solid): Unit = {
    val renderingOption = List("$fn=100;")
    val renderer = new backends.OpenSCAD(renderingOption)
    renderer.view(obj)
    // renderer.toSTL(obj, "holder.stl")
  }
}
