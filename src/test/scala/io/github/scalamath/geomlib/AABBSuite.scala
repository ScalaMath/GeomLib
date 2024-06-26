package io.github.scalamath.geomlib

import io.github.scalamath.vecmatlib.{Mat3f, Mat3x4f, Mat4f, Vec3f}
import org.scalatest.funsuite.AnyFunSuite

class AABBSuite extends AnyFunSuite {

  test("Get AABB position") {
    val aabb = AABB(-1.0f, -2.0f, -3.0f, 3.0f, 2.0f, 1.0f)
    assert(aabb.position == Vec3f(-1.0f, -2.0f, -3.0f))
  }

  test("Get AABB size") {
    val aabb = AABB(-1.0f, -2.0f, -3.0f, 3.0f, 2.0f, 1.0f)
    assert(aabb.size == Vec3f(3.0f, 2.0f, 1.0f))
  }

  test("Get AABB end point") {
    val aabb = AABB(-1.0f, -2.0f, -3.0f, 3.0f, 2.0f, 1.0f)
    assert(aabb.end == Vec3f(2.0f, 0.0f, -2.0f))
  }

  test("Get center of AABB") {
    val aabb = AABB(-1.0f, -2.0f, -3.0f, 3.0f, 2.0f, 1.0f)
    assert(aabb.center == Vec3f(0.5f, -1.0f, -2.5f))
  }

  test("Volume of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, 2.0f, 3.0f, 4.0f)
    assert(aabb.volume == 24.0f)
  }

  test("Left extent of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, -2.0f, -3.0f, -4.0f)
    assert(aabb.x == 0.0f)
    assert(aabb.left == -2.0f)
  }

  test("Left plane of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, -2.0f, -3.0f, -4.0f)
    val plane = Plane(-1.0f, 0.0f, 0.0f, -2.0f)
    assert(aabb.leftPlane == plane)
  }

  test("Right extent of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, 2.0f, 3.0f, 4.0f)
    assert(aabb.x == 0.0f)
    assert(aabb.right == 2.0f)
  }

  test("Right plane of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, 2.0f, 3.0f, 4.0f)
    val plane = Plane(1.0f, 0.0f, 0.0f, 2.0f)
    assert(aabb.rightPlane == plane)
  }

  test("Top extent of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, 2.0f, 3.0f, 4.0f)
    assert(aabb.y == 0.0f)
    assert(aabb.top == 3.0f)
  }

  test("Top plane of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, 2.0f, 3.0f, 4.0f)
    val plane = Plane(0.0f, 1.0f, 0.0f, 3.0f)
    assert(aabb.topPlane == plane)
  }

  test("Bottom extent of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, -2.0f, -3.0f, -4.0f)
    assert(aabb.y == 0.0f)
    assert(aabb.bottom == -3.0f)
  }

  test("Bottom plane of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, -2.0f, -3.0f, -4.0f)
    val plane = Plane(0.0f, -1.0f, 0.0f, -3.0f)
    assert(aabb.bottomPlane == plane)
  }

  test("Font extent of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, -2.0f, -3.0f, -4.0f)
    assert(aabb.z == 0.0f)
    assert(aabb.front == -4.0f)
  }

  test("Font plane of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, -2.0f, -3.0f, -4.0f)
    val plane = Plane(0.0f, 0.0f, 1.0f, -4.0f)
    assert(aabb.frontPlane == plane)
  }

  test("Back extent of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, 2.0f, 3.0f, 4.0f)
    assert(aabb.z == 0.0f)
    assert(aabb.back == 4.0f)
  }

  test("Back plane of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, 2.0f, 3.0f, 4.0f)
    val plane = Plane(0.0f, 0.0f, -1.0f, 4.0f)
    assert(aabb.backPlane == plane)
  }

  test("Check if an AABB contains a point") {
    val aabb = AABB(-1.0f, -1.0f, -1.0f, 3.0f, 2.0f, 3.0f)
    val p1 = Vec3f(1.0f, 0.5f, 1.0f)
    assert(aabb.containsPoint(p1))
    val p2 = Vec3f(2.0f, 1.0f, -1.0f)
    assert(aabb.containsPoint(p2, includeFaces = true))
    assert(!aabb.containsPoint(p2, includeFaces = false))
    val p3 = Vec3f(3.0f, 2.0f, 0.5f)
    assert(!aabb.containsPoint(p3, includeFaces = true))
    assert(!aabb.containsPoint(p3, includeFaces = false))
  }

  test("Check if two AABBs intersect") {
    val aabb1 = AABB(-1.0f, -1.0f, -1.0f, 3.0f, 2.0f, 3.0f)
    val aabb2 = AABB(0.0f, 0.0f, 0.0f, 3.0f, 3.0f, 2.0f)
    assert(aabb1.intersects(aabb2))
    val aabb3 = AABB(2.0f, -1.0f, 0.0f, 2.0f, 3.0f, 3.0f)
    assert(!aabb1.intersects(aabb3, includeFaces = false))
    assert(aabb1.intersects(aabb3, includeFaces = true))
    val aabb4 = AABB(4.0f, 5.0f, 2.0f, 1.0f, 1.0f, 1.0f)
    assert(!aabb1.intersects(aabb4, includeFaces = false))
    assert(!aabb1.intersects(aabb4, includeFaces = true))
  }

  test("Get the intersection between two AABBs") {
    val aabb1 = AABB(-1.0f, -1.0f, -1.0f, 3.0f, 2.0f, 3.0f)
    val aabb2 = AABB(0.0f, 0.0f, 0.0f, 3.0f, 3.0f, 2.0f)
    val res = AABB(0.0f, 0.0f, 0.0f, 2.0f, 1.0f, 2.0f)
    assert(aabb1.intersection(aabb2) == res)
    val aabb3 = AABB(4.0f, 5.0f, 2.0f, 1.0f, 1.0f, 1.0f)
    assert(aabb1.intersection(aabb3) == AABB())
  }

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

  test("Get the intersection between an AABB and a line") {
    val aabb = AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f)
    val p1 = Vec3f(-1.0f, 0.0f, 0.0f)
    val p2 = Vec3f(1.0f, 0.0f, 0.0f)
    assert(aabb.lineIntersection(Vec3f(-2.0f, 0.0f, 0.0f), Vec3f(1.0f, 0.0f, 0.0f)) == p1)
    assert(aabb.lineIntersection(Vec3f(-2.0f, 0.0f, 0.0f), Vec3f(-1.0f, 0.0f, 0.0f)) == p2)
    val p3 = aabb.lineIntersection(Vec3f(-2.0f, -10.0f, 0.0f), Vec3f(1.0f, 1.0f, 0.0f))
    assert(p3.x.isNaN && p3.y.isNaN && p3.z.isNaN)
  }

  test("AABB absolute value") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, -2.0f, -3.0f, -4.0f)
    val res = AABB(-2.0f, -3.0f, -4.0f, 2.0f, 3.0f, 4.0f)
    assert(aabb.abs == res)
  }

  test("Check if an AABB completely encloses another") {
    val aabb1 = AABB(-2.0f, -2.0f, -2.0f, 4.0f, 4.0f, 4.0f)
    val aabb2 = AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f)
    val aabb3 = AABB(-0.5f, -0.5f, -0.5f, 3.0f, 3.0f, 3.0f)
    assert(aabb1.encloses(aabb2))
    assert(!aabb1.encloses(aabb3))
  }

  test("Grow AABB") {
    val aabb = AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f)
    val res = AABB(-2.0f, -2.0f, -2.0f, 4.0f, 4.0f, 4.0f)
    assert(aabb.grow(1.0f) == res)
  }

  test("Expand AABB to point") {
    val aabb = AABB(-0.5f, -0.5f, -0.5f, 1.0f, 1.0f, 1.0f)
    val p1 = Vec3f(2.0f, 0.0f, 0.0f)
    val res1 = AABB(-0.5f, -0.5f, -0.5f, 2.5f, 1.0f, 1.0f)
    assert(aabb.expandTo(p1) == res1)
    val p2 = Vec3f(0.0f, -1.0f, 0.0f)
    val res2 = AABB(-0.5f, -1.0f, -0.5f, 1.0f, 1.5f, 1.0f)
    assert(aabb.expandTo(p2) == res2)
  }

  test("Merge two AABBs") {
    val aabb1 = AABB(-1.0f, -1.0f, -2.0f, 2.0f, 3.0f, 4.0f)
    val aabb2 = AABB(0.0f, 0.0f, 1.0f, 2.0f, 4.0f, 3.0f)
    val res = AABB(-1.0f, -1.0f, -2.0f, 3.0f, 5.0f, 6.0f)
    assert(aabb1.merge(aabb2) == res)
  }

  test("Bounding sphere of AABB") {
    val aabb = AABB(0.0f, 0.0f, 0.0f, 2.0f, 2.0f, 2.0f)
    val sphere = Sphere(1.0f, 1.0f, 1.0f, math.sqrt(3.0).toFloat)
    assert(aabb.boundingSphere == sphere)
  }

  test("Transform an AABB with a 3x3 matrix") {
    val aabb = AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f)
    val transform = Mat3f.scaling(2.0f, 3.0f, 2.0f)
    val res = AABB(-2.0f, -3.0f, -2.0f, 4.0f, 6.0f, 4.0f)
    assert(transform * aabb == res)
  }

  test("Transform an AABB with a 3x4 matrix") {
    val aabb = AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f)
    val transform = Mat3x4f.translation(2.0f, 3.0f, 2.0f)
    val res = AABB(1.0f, 2.0f, 1.0f, 2.0f, 2.0f, 2.0f)
    assert(transform * aabb == res)
  }

  test("Transform an AABB with a 4x4 matrix") {
    val aabb = AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f)
    val transform = Mat4f.translation(2.0f, 3.0f, 2.0f) * Mat4f.scaling(2.0f, 3.0f, 2.0f)
    val res = AABB(0.0f, 0.0f, 0.0f, 4.0f, 6.0f, 4.0f)
    assert(transform * aabb == res)
  }

  // TODO: Inverse transform

  // TODO: Support

  test("Check if two AABBs are congruent") {
    val aabb1 = AABB(-1.0f, -2.0f, -3.0f, 3.0f, 2.0f, 1.0f)
    val aabb2 = AABB(2.0f, 0.0f, -2.0f, -3.0f, -2.0f, -1.0f)
    assert(aabb1.isCongruentTo(aabb2))
  }

  test("Check if two AABBs are approximately equal") {
    val aabb1 = AABB(-1.0f, -2.0f, -3.0f, 3.0f, 2.0f, 1.0f)
    val aabb2 = AABB(-1.0000001f, -1.9999999f, -2.9999999f, 3.0000001f, 2.0000001f, 0.99999999f)
    assert(aabb1 != aabb2)
    assert(aabb1 ~= aabb2)
  }

  test("Construct an AABB from two points") {
    val p1 = Vec3f(-1.0f, -2.0f, -3.0f)
    val p2 = Vec3f(2.0f, 0.0f, -2.0f)
    val aabb = AABB(-1.0f, -2.0f, -3.0f, 3.0f, 2.0f, 1.0f)
    assert(AABB.fromPoints(p1, p2) == aabb)
  }
}
