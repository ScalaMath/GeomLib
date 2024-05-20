package io.github.scalamath.geomlib

import io.github.scalamath.vecmatlib.Vec3f
import org.scalatest.funsuite.AnyFunSuite

class SphereSuite extends AnyFunSuite {

  test("Construct sphere from center and point") {
    val res = Sphere(1.0f, 1.0f, 1.0f, math.sqrt(3.0).toFloat)
    assert(Sphere(Vec3f.One, Vec3f(2.0f, 2.0f, 2.0f)) == res)
    assert(Sphere(1.0f, 1.0f, 1.0f, Vec3f(2.0f, 2.0f, 2.0f)) == res)
    assert(Sphere(Vec3f.One, 2.0f, 2.0f, 2.0f) == res)
  }

  test("Get the center of a sphere") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 2.0f)
    assert(sphere.center == Vec3f.One)
  }

  test("Get the diameter of a sphere") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 2.0f)
    assert(sphere.diameter == 4.0f)
  }

  test("Get the squared radius of a sphere") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 3.0f)
    assert(sphere.radiusSquared == 9.0f)
  }

  test("Get the surface area of a sphere") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 3.0f)
    assert(sphere.surface == 36.0f * math.Pi.toFloat)
  }

  test("Get the cubed radius of a sphere") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 3.0f)
    assert(sphere.radiusCubed == 27.0f)
  }

  test("Get the volume of a sphere") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 3.0f)
    assert(sphere.volume == 36.0f * math.Pi.toFloat)
  }

  test("Check if a sphere contains a point using coordinates") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 3.0f)
    assert(sphere.containsPoint(1.0f, 2.0f, 1.0f))
    assert(!sphere.containsPoint(1.0f, 4.0f, 1.0f, includeSurface = false))
    assert(sphere.containsPoint(1.0f, 4.0f, 1.0f, includeSurface = true))
    assert(!sphere.containsPoint(-1.0f, -5.0f, 1.0f, includeSurface = false))
    assert(!sphere.containsPoint(-1.0f, -5.0f, 1.0f, includeSurface = true))
  }

  test("Check if a sphere contains a point using a vector") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 3.0f)
    val p1 = Vec3f(1.0f, 2.0f, 1.0f)
    val p2 = Vec3f(1.0f, 4.0f, 1.0f)
    val p3 = Vec3f(-1.0f, -5.0f, 1.0f)
    assert(sphere.containsPoint(p1))
    assert(!sphere.containsPoint(p2, includeSurface = false))
    assert(sphere.containsPoint(p2, includeSurface = true))
    assert(!sphere.containsPoint(p3, includeSurface = false))
    assert(!sphere.containsPoint(p3, includeSurface = true))
  }

  test("Check if a point is on the surface of a sphere using coordinates") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 3.0f)
    assert(!sphere.isPointOnSurface(1.0f, 2.0f, 1.0f))
    assert(sphere.isPointOnSurface(1.0f, 4.0f, 1.0f))
    assert(!sphere.isPointOnSurface(-1.0f, -5.0f, 1.0f))
  }

  test("Check if a point is on the surface of a sphere using a vector") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 3.0f)
    val p1 = Vec3f(1.0f, 2.0f, 1.0f)
    val p2 = Vec3f(1.0f, 4.0f, 1.0f)
    val p3 = Vec3f(-1.0f, -5.0f, 1.0f)
    assert(!sphere.isPointOnSurface(p1))
    assert(sphere.isPointOnSurface(p2))
    assert(!sphere.isPointOnSurface(p3))
  }
  
  test("Check if two spheres intersect") {
    val s1 = Sphere(1.0f, 1.0f, 1.0f, 2.0f)
    val s2 = Sphere(1.0f, 1.5f, 1.0f, 2.5f)
    val s3 = Sphere(5.0f, 1.0f, 1.0f, 2.0f)
    val s4 = Sphere(-1.0f, -5.0f, 1.0f, 1.0f)
    assert(s1.intersects(s2))
    assert(!s1.intersects(s3, includeSurface = false))
    assert(s1.intersects(s3, includeSurface = true))
    assert(!s1.intersects(s4, includeSurface = false))
    assert(!s1.intersects(s4, includeSurface = true))
  }

  test("Check if a sphere completely encloses another") {
    val s1 = Sphere(3.0f)
    val s2 = Sphere(1.3f)
    val s3 = Sphere(0.0f, 0.1f, 0.1f, 3.0f)
    val s4 = Sphere(0.0f, 2.5f, 0.0f, 0.3f)
    assert(s1.encloses(s2))
    assert(!s1.encloses(s3))
    assert(s1.encloses(s4))
  }

  test("Merge two spheres") {
    val s1 = Sphere(0.0f, 0.0f, 2.0f, 2.0f)
    val s2 = Sphere(3.0f, 0.0f, 2.0f, 2.0f)
    val res = Sphere(1.5f, 0.0f, 2.0f, 3.5f)
    assert(s1.merge(s2) == res)
  }

  test("Get the bounding box of a sphere") {
    val sphere = Sphere(1.0f, 1.0f, 1.0f, 2.0f)
    val aabb = AABB(-1.0f, -1.0f, -1.0f, 4.0f, 4.0f, 4.0f)
    assert(sphere.boundingBox == aabb)
  }

  test("Check if two spheres are approximately equal") {
    val s1 = Sphere(1.0f, 1.0f, 2.0f, 3.0f)
    val s2 = Sphere(1.0000001f, 0.9999999f, 2.0000001f, 2.9999999f)
    assert(s1 != s2)
    assert(s1 ~= s2)
  }
}
