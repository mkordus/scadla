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

    val buttonsSpaceX = readerXScreen
    val buttonsSpaceY = baseY
    val buttonsSpaceZ = readerZScreen - holderTopMargin
    val buttonsSpace = Cube(buttonsSpaceX, buttonsSpaceY, buttonsSpaceZ)
      .moveX(-readerXScreen / 2)
      .moveY(-buttonsSpaceY / 2 + wallThicness)
      .moveZ(wallThicness)

    val screenSpace = Cube(readerXScreen, baseY, holderZ - readerZScreen)
      .moveX(-readerXScreen / 2)
      .moveY(-baseY / 2 + wallThicness)
      .moveZ(readerZScreen + wallThicness)

    val bottomSpace = Cube(80, baseY, wallThicness)
      .moveX(-80 / 2)
      .moveY(-baseY / 2)

    val handle = Hull(
      Union(
        Cylinder(
          radius = baseCylinderRadius,
          height = handleZ
        ),
        Sphere(
          radius = baseCylinderRadius
        ),
        Cylinder(
          radius = baseCylinderRadius,
          height = handleZ
        )
          .moveX(-holderSideMargin),
        Sphere(
          radius = baseCylinderRadius
        )
          .moveX(-holderSideMargin)
      )
    ).moveZ(-handleZ)

    val pinY = baseY
    // val pinY = 6.0

    val pin = Union(
      Cube(9.0, pinY, 3.0)
        .moveX(-3.0)
        .moveZ(3.0),
      Cube(3.0, pinY, 3.0)
    )

    val pinHolder = Difference(
      Cube(15.0, pinY, 9.0)
        .moveX(-6.0),
      Union(
        Cube(9.2, pinY, 3.2)
          .moveX(-3.1)
          .moveZ(2.9),
        Cube(3.2, pinY, 3.2)
          .moveX(-0.1)
      )
    )

    val pinHandle = Difference(
      handle,
      Union(
        Cube(9.2, pinY + 2, 3.2)
          .moveX(-3.1)
          .moveZ(2.9),
        Cube(3.2, pinY + 2, 3.2)
          .moveX(-0.1)
      )
        .moveX(9.0)
        .moveY(-baseY / 2)
        .rotateY(180)
    )

    val all = Union(
      Difference(
        Union(
          base,
          baseCylinder,
          baseCylinder.rotateZ(180)
        ),
        reader,
        buttonsSpace,
        screenSpace,
        bottomSpace
      ),
      pin.moveX(baseX / 2 - 9.0).moveY(-baseY / 2).rotateY(180),
      pin.moveX(baseX / 2 - 9.0).moveY(-baseY / 2).rotateY(180).rotateZ(180)
      // handle.moveZ(wallThicness).moveX(baseX / 2),
      // handle.moveZ(wallThicness).moveX(baseX / 2).rotateZ(180)
    )
      .rotateY(180)

    // Union(
    //   pinHolder,
    //   pin.moveX(20)
    // )
    //   .rotateX(90)

    // pinHandle
    all
  }

  private def renderTest(obj: Solid): Unit = {
    val renderingOption = List("$fn=100;")
    val renderer = new backends.OpenSCAD(renderingOption)
    renderer.view(obj)
    // renderer.toSTL(obj, "handle.stl")
  }
}
