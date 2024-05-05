package io.github.scalamath.geomlib

import io.github.scalamath.vecmatlib.Vec3f
import org.scalatest.funsuite.AnyFunSuite

class AABBSuite extends AnyFunSuite {

  test("Check if a plane intersects an AABB") {
    val aabb1 = AABB(-1.0f, -1.0f, -1.0f, 3.0f, 3.0f, 2.0f)
    val plane = Plane(1.0f, 0.0f, 1.0f, 2.0f)
    assert(aabb1.intersects(plane))
    val aabb2 = AABB(-3.0f, -2.0f, -1.0f, 3.0f, 3.0f, 2.0f)
    assert(!aabb2.intersects(plane))
  }

  test("Check if a line intersects an AABB") {
    val aabb = AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f)
    assert(aabb.intersectsLine(Vec3f(-2.0f, 0.0f, 0.0f), Vec3f(1.0f, 0.0f, 0.0f)))
    assert(aabb.intersectsLine(Vec3f(-2.0f, 0.0f, 0.0f), Vec3f(-1.0f, 0.0f, 0.0f)))
    assert(!aabb.intersectsLine(Vec3f(-2.0f, -10.0f, 0.0f), Vec3f(1.0f, 1.0f, 0.0f)))
  }
}
