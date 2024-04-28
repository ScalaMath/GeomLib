package io.github.scalamath.geomlib

import io.github.scalamath.vecmatlib.Vec3f
import org.scalatest.funsuite.AnyFunSuite

class PlaneSuite extends AnyFunSuite {

  test("Temp") {
    val plane0 = Plane(0.0f, 2.0f, 0.0f, 0.0f)
    val point = Vec3f(0.0f, -1.0f, 0.0f)
    println(plane0.distanceTo(point))
  }
}
