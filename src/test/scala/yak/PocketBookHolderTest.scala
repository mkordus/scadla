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

    val baseX = readerX + wallThicness * 2
    val baseY = readerY + wallThicness * 2
    val baseZ = holderZ + wallThicness

    val base = Cube(
      width = baseX,
      depth = baseY,
      height = baseZ
    )
      .moveX(-baseX / 2)
      .moveY(-baseY / 2)

    val screenSpace = Cube(readerXScreen, baseY, readerZ)
      .moveX(-readerXScreen / 2)
      .moveY(-baseY / 2 + wallThicness)
      .moveZ(wallThicness * 2)

    val bottomSpaceX = 30.0

    val weightReductionX = (baseX - bottomSpaceX) / 2
    val weightReductionZ = baseZ - 20.0
    val weightReduction = Cube(weightReductionX, baseY, weightReductionZ)
      .moveX(bottomSpaceX / 2)
      .moveY(-baseY / 2)

    val handleRadius = 20 / 2
    val handleMount = Hull(
      Cube(bottomSpaceX, baseY, wallThicness)
        .moveX(-bottomSpaceX / 2)
        .moveY(-baseY / 2),
      Cylinder(
        radius = handleRadius,
        height = wallThicness
      ),
      // Sphere(
      //   radius = handleRadius
      // ),
      Cylinder(
        radius = handleRadius,
        height = wallThicness
      ).moveY(handleRadius)
    )

    val handle = Difference(
      Hull(
        Union(
          Cylinder(
            radius = handleRadius,
            height = handleZ + 20
          ),
          Sphere(
            radius = handleRadius
          ),
          Cylinder(
            radius = handleRadius,
            height = handleZ + 20
          ).moveY(handleRadius),
          Sphere(
            radius = handleRadius
          ).moveY(handleRadius)
        )
      ).moveZ(-handleZ)
        .rotateX(15),
      Cube(200, 200, 200)
        .moveX(-100)
        .moveY(-100)
    )

    val all = Union(
      Difference(
        base,
        reader,
        screenSpace,
        weightReduction,
        weightReduction.rotateZ(180)
      ),
      handle,
      handleMount
    )
      .rotateY(180)

    all
  }

  private def renderTest(obj: Solid): Unit = {
    val renderingOption = List("$fn=90;")
    val renderer = new backends.OpenSCAD(renderingOption)
    // renderer.view(obj)
    renderer.toSTL(obj, "holder5.stl")
  }
}
