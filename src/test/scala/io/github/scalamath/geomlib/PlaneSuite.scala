package io.github.scalamath.geomlib

import io.github.scalamath
import io.github.scalamath.vecmatlib.{Mat3f, Mat3x4f, Mat4f, Vec3f}
import org.scalatest.funsuite.AnyFunSuite

class PlaneSuite extends AnyFunSuite {

  test("Plane with a point and a normal") {
    val normal = Vec3f(1.0f, 1.0f, 1.0f)
    val point = Vec3f(0.5f, 0.5f, 1.0f)
    val plane = Plane(normal, point)
    val res = Plane(1.0f, 1.0f, 1.0f, 2.0f)
    assert(plane == res)
  }

  test("Plane through three points") {
    val p1 = Vec3f(-1.0f, -2.0f, 0.0f)
    val p2 = Vec3f(-2.0f, 1.0f, 2.0f)
    val p3 = Vec3f(1.5f, 1.5f, 1.0f)
    val plane = Plane(p1, p2, p3)
    val res = Plane(-4.0f, 6.0f, -11.0f, -8.0f)
    assert(plane == res)
  }

  test("Get plane equation coefficients") {
    val plane = Plane(1.0f, 2.0f, 3.0f, 4.0f)
    assert(plane.a == 1.0f)
    assert(plane.b == 2.0f)
    assert(plane.c == 3.0f)
    assert(plane.d == 4.0f)
  }

  test("Distance from point to plane") {
    val plane = Plane(1.0f, 2.0f, 3.0f, -4.0f)
    val point = Vec3f(3.0f, 4.0f, 5.0f)
    assert(scalamath.equalsApprox(plane.distanceTo(point), 11.0 / 7.0 * math.sqrt(14.0)))
  }

  test("Flip plane") {
    val plane = Plane(1.0f, 0.0f, 1.0f, -2.0f)
    val res = Plane(-1.0f, 0.0f, -1.0f, 2.0f)
    assert(-plane == res)
  }

  test("Check if plane contains points") {
    val p1 = Vec3f(-1.0f, -2.0f, 0.0f)
    val p2 = Vec3f(-2.0f, 1.0f, 2.0f)
    val p3 = Vec3f(1.5f, 1.5f, 1.0f)
    val plane = Plane(-4.0f, 6.0f, -11.0f, -8.0f)
    assert(plane.containsPoint(p1))
    assert(plane.containsPoint(p2))
    assert(plane.containsPoint(p3))
    assert(!plane.containsPoint(3.5f, -1.0f, 2.9f))
  }

  ignore("Check if plane intersects line") {
    // TODO: Line plane intersection
  }

  ignore("Get line-plane intersection") {
    // TODO: Line plane intersection
  }

  test("Check if two planes are parallel") {
    val plane1 = Plane(0.0f, 1.0f, 0.0f)
    val plane2 = Plane(0.0f, 2.0f, 0.0f, 5.0f)
    assert(plane1.isParallelTo(plane2))
    val plane3 = Plane(1.0f, 0.0f, 1.0f, 2.0f)
    assert(!plane1.isParallelTo(plane3))
    assert(plane1.isParallelTo(plane1))
    val plane4 = Plane(2.0f, 0.0f, 2.0f, 4.0f)
    assert(plane3.isParallelTo(plane4))
  }

  test("Check if two planes are congruent") {
    val plane1 = Plane(0.0f, 1.0f, 0.0f, 3.0f)
    val plane2 = Plane(0.0f, 1.0f, 0.0f, 3.0f)
    val plane3 = Plane(0.0f, 2.0f, 0.0f, 6.0f)
    assert(plane1.isCongruentTo(plane2))
    assert(plane1.isCongruentTo(plane3))
    val plane4 = Plane(0.0f, 2.0f, 0.0f, 5.0f)
    assert(!plane1.isCongruentTo(plane4))
    val plane5 = Plane(2.0f, 0.0f, 2.0f, 4.0f)
    assert(!plane1.isCongruentTo(plane5))
    val plane6 = Plane(0.2f, 0.0f, 0.2f, 0.4f)
    assert(plane5.isCongruentTo(plane6))
  }

  test("Check if two planes intersect") {
    val plane1 = Plane(0.0f, 1.0f, 0.0f, 3.0f)
    val plane2 = Plane(1.0f, 0.0f, 1.0f, 2.0f)
    assert(plane1.intersects(plane2))
    val plane3 = Plane(0.0f, 1.0f, 0.0f, 5.0f)
    assert(!plane1.intersects(plane3))
    val plane4 = Plane(3.0f, 0.0f, 3.0f, 6.0f)
    assert(plane2.intersects(plane4))
  }

  test("Check if three planes intersect in a single point") {
    val plane1 = Plane(1.0f, 2.0f, 1.0f, 1.0f)
    val plane2 = Plane(2.0f, 0.0f, 1.0f, 1.0f)
    val plane3 = Plane(1.0f, 1.0f, 2.0f, 1.0f)
    assert(plane1.intersectsInPoint(plane2, plane3))
    val plane4 = Plane(0.0f, 1.0f, 0.0f, 3.0f)
    val plane5 = Plane(0.0f, 1.0f, 0.0f, 7.0f)
    val plane6 = Plane(1.0f, 0.0f, 0.0f, 2.0f)
    assert(!plane4.intersectsInPoint(plane5, plane6))
  }

  test("Get the point of intersection between three planes") {
    val plane1 = Plane(1.0f, 2.0f, 1.0f, 1.0f)
    val plane2 = Plane(2.0f, 1.0f, 1.0f, 1.0f)
    val plane3 = Plane(1.0f, 1.0f, 2.0f, 1.0f)
    val res = Vec3f(0.25f, 0.25f, 0.25f)
    assert(plane1.intersection(plane2, plane3) == res)
  }

  ignore("Project point onto plane") {
    // TODO: Project point onto plane
  }

  test("Check if point is above plane") {
    val plane1 = Plane(0.0f, 1.0f, 0.0f, 2.0f)
    val p1 = Vec3f(1.0f, 4.0f, 1.0f)
    val p2 = Vec3f(2.0f, -3.0f, 3.0f)
    assert(plane1.isPointOver(p1))
    assert(!plane1.isPointOver(p2))
    val plane2 = Plane(0.0f, -1.0f, 0.0f, -2.0f)
    assert(!plane2.isPointOver(p1))
    assert(plane2.isPointOver(p2))
    val plane3 = Plane(1.0f, 0.0f, 1.0f)
    assert(plane3.isPointOver(1.0f, 0.0f, 1.0f))
    assert(!plane3.isPointOver(-1.0f, 0.0f, -1.0f))
  }

  test("Rotate a plane with a 3x3 matrix") {
    val transform = Mat3f.rotation(math.Pi / 2.0, 0.0, math.Pi / 4.0)
    val plane = Plane(1.0f, 1.0f, 0.0f, 1.0f)
    val res = Plane(0.0f, 0.0f, 1.0f, math.sqrt(2.0).toFloat)
    assert((transform * plane).isCongruentTo(res))
  }

  test("Transform a plane with a 3x4 matrix") {
    val transform = Mat3x4f.translation(0.0f, 0.0f, 1.0f) * Mat4f.rotation(math.Pi / 2.0, 0.0, math.Pi / 4.0)
    val plane = Plane(1.0f, 1.0f, 0.0f, 1.0f)
    val res = Plane(0.0f, 0.0f, 1.0f, math.sqrt(2.0).toFloat + 1.0f)
    assert((transform * plane).isCongruentTo(res))
  }

  test("Transform a plane with a 4x4 matrix") {
    val transform = Mat4f.translation(0.0f, 0.0f, 1.0f) * Mat4f.rotation(math.Pi / 2.0, 0.0, math.Pi / 4.0)
    val plane = Plane(1.0f, 1.0f, 0.0f, 1.0f)
    val res = Plane(0.0f, 0.0f, 1.0f, math.sqrt(2.0).toFloat + 1.0f)
    assert((transform * plane).isCongruentTo(res))
  }

  test("Inverse transform a plane with a 3x3 matrix") {
    val transform = Mat3f.rotation(math.Pi / 2.0, 0.0, math.Pi / 4.0)
    val plane = Plane(1.0f, 1.0f, 0.0f, 1.0f)
    val res = Plane(0.5f, -0.5f, -math.sqrt(2.0).toFloat / 2.0f, math.sqrt(2.0).toFloat)
    assert((plane * transform).isCongruentTo(res))
  }

  test("Inverse transform a plane with a 4x4 matrix") {
    val transform = Mat4f.translation(0.0f, 1.0f, 0.0f) * Mat4f.rotation(math.Pi / 2.0, 0.0, math.Pi / 4.0)
    val plane = Plane(1.0f, 1.0f, 0.0f, 1.0f)
    val res = Plane(0.5f, -0.5f, -math.sqrt(2.0).toFloat / 2.0f, math.sqrt(2.0).toFloat / 2.0f)
    assert((plane * transform).isCongruentTo(res))
  }

  test("Check if two planes are approximately equal") {
    val plane1 = Plane(1.0f, 2.0f, 3.0f, 4.0f)
    val plane2 = Plane(0.9999999f, 1.9999999f, 3.0000001f, 4.0000001f)
    val plane3 = -plane1
    assert(plane1 != plane2)
    assert(plane1 ~= plane2)
    assert(plane3.isCongruentTo(plane1))
    assert(!(plane3 ~= plane1))
  }
}
