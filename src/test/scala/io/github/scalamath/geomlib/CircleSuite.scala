package io.github.scalamath.geomlib

import io.github.scalamath.vecmatlib.Vec2f
import org.scalatest.funsuite.AnyFunSuite

class CircleSuite extends AnyFunSuite {

  test("Construct circle from center and point") {
    val res = Circle(1.0f, 1.0f, math.sqrt(2.0).toFloat)
    assert(Circle(Vec2f.One, Vec2f(2.0f, 2.0f)) == res)
    assert(Circle(1.0f, 1.0f, Vec2f(2.0f, 2.0f)) == res)
    assert(Circle(Vec2f.One, 2.0f, 2.0f) == res)
  }

  test("Construct circle from three points") {
    val p1 = Vec2f(-1.0f, 2.0f)
    val p2 = Vec2f(2.0f, 3.0f)
    val p3 = Vec2f(4.0f, 2.0f)
    val circle = Circle(1.5f, -0.5f, math.sqrt(12.5).toFloat)
    assert(Circle(p1, p2, p3) == circle)
  }

  test("Get the center of a circle") {
    val circle = Circle(1.0f, 1.0f, 2.0f)
    assert(circle.center == Vec2f.One)
  }

  test("Get the diameter of a circle") {
    val circle = Circle(1.0f, 1.0f, 2.0f)
    assert(circle.diameter == 4.0f)
  }

  test("Get the circumference of a circle") {
    val circle = Circle(1.0f, 1.0f, 2.0f)
    assert(circle.circumference == 4.0f * math.Pi.toFloat)
  }

  test("Get the squared radius of a circle") {
    val circle = Circle(1.0f, 1.0f, 3.0f)
    assert(circle.radiusSquared == 9.0f)
  }

  test("Get the area of a circle") {
    val circle = Circle(1.0f, 1.0f, 3.0f)
    assert(circle.area == 9.0f * math.Pi.toFloat)
  }

  test("Check if a circle contains a point using coordinates") {
    val circle = Circle(1.0f, 1.0f, 3.0f)
    assert(circle.containsPoint(1.0f, 2.0f))
    assert(!circle.containsPoint(1.0f, 4.0f, includeCircumference = false))
    assert(circle.containsPoint(1.0f, 4.0f, includeCircumference = true))
    assert(!circle.containsPoint(-1.0f, -5.0f, includeCircumference = false))
    assert(!circle.containsPoint(-1.0f, -5.0f, includeCircumference = true))
  }

  test("Check if a circle contains a point using a vector") {
    val circle = Circle(1.0f, 1.0f, 3.0f)
    val p1 = Vec2f(1.0f, 2.0f)
    val p2 = Vec2f(1.0f, 4.0f)
    val p3 = Vec2f(-1.0f, -5.0f)
    assert(circle.containsPoint(p1))
    assert(!circle.containsPoint(p2, includeCircumference = false))
    assert(circle.containsPoint(p2, includeCircumference = true))
    assert(!circle.containsPoint(p3, includeCircumference = false))
    assert(!circle.containsPoint(p3, includeCircumference = true))
  }

  test("Check if a point is on the circumference using coordinates") {
    val circle = Circle(1.0f, 1.0f, 3.0f)
    assert(!circle.isPointOnCircumference(1.0f, 2.0f))
    assert(circle.isPointOnCircumference(1.0f, 4.0f))
    assert(!circle.isPointOnCircumference(-1.0f, -5.0f))
  }

  test("Check if a point is on the circumference using a vector") {
    val circle = Circle(1.0f, 1.0f, 3.0f)
    val p1 = Vec2f(1.0f, 2.0f)
    val p2 = Vec2f(1.0f, 4.0f)
    val p3 = Vec2f(-1.0f, -5.0f)
    assert(!circle.isPointOnCircumference(p1))
    assert(circle.isPointOnCircumference(p2))
    assert(!circle.isPointOnCircumference(p3))
  }

  test("Check if two circles intersect") {
    val c1 = Circle(1.0f, 1.0f, 2.0f)
    val c2 = Circle(1.0f, 1.5f, 2.5f)
    val c3 = Circle(5.0f, 1.0f, 2.0f)
    val c4 = Circle(-1.0f, -5.0f, 1.0f)
    assert(c1.intersects(c2))
    assert(!c1.intersects(c3, includeCircumference = false))
    assert(c1.intersects(c3, includeCircumference = true))
    assert(!c1.intersects(c4, includeCircumference = false))
    assert(!c1.intersects(c4, includeCircumference = true))
  }

  test("Check if a circle completely encloses another") {
    val c1 = Circle(3.0f)
    val c2 = Circle(1.3f)
    val c3 = Circle(0.0f, 0.1f, 3.0f)
    val c4 = Circle(0.0f, 2.5f, 0.3f)
    assert(c1.encloses(c2))
    assert(!c1.encloses(c3))
    assert(c1.encloses(c4))
  }

  test("Merge two circles") {
    val c1 = Circle(2.0f)
    val c2 = Circle(3.0f, 0.0f, 2.0f)
    val res = Circle(1.5f, 0.0f, 3.5f)
    assert(c1.merge(c2) == res)
  }

  test("Get the bounding rectangle of a circle") {
    val circle = Circle(1.0f, 1.0f, 2.0f)
    val rectangle = Rect2(-1.0f, -1.0f, 4.0f, 4.0f)
    assert(circle.boundingRect == rectangle)
  }

  test("Check if two circles are approximately equal") {
    val c1 = Circle(1.0f, 1.0f, 2.0f)
    val c2 = Circle(1.0000001f, 0.9999999f, 2.0000001f)
    assert(c1 != c2)
    assert(c1 ~= c2)
  }
}
