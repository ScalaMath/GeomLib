package io.github.scalamath.geomlib

import io.github.scalamath.vecmatlib.{Mat2f, Mat2x3f, Mat3f, Vec2f}
import org.scalatest.funsuite.AnyFunSuite

class Rect2Suite extends AnyFunSuite {

  test("Get rectangle position") {
    val rect = Rect2(1.0f, 2.0f, 3.0f, 4.0f)
    val res = Vec2f(1.0f, 2.0f)
    assert(rect.position == res)
  }

  test("Get rectangle size") {
    val rect = Rect2(1.0f, 2.0f, 3.0f, 4.0f)
    val res = Vec2f(3.0f, 4.0f)
    assert(rect.size == res)
  }

  test("Get rectangle end") {
    val rect1 = Rect2(1.0f, 2.0f, 3.0f, 4.0f)
    val res1 = Vec2f(4.0f, 6.0f)
    assert(rect1.end == res1)
    val rect2 = Rect2(1.0f, 2.0f, -3.0f, -4.0f)
    val res2 = Vec2f(-2.0f, -2.0f)
    assert(rect2.end == res2)
  }

  test("Get rectangle center") {
    val rect = Rect2(1.0f, 2.0f, 3.0f, 4.0f)
    val res = Vec2f(2.5f, 4.0f)
    assert(rect.center == res)
  }

  test("Get rectangle area") {
    val rect = Rect2(4.0f, 3.0f)
    assert(rect.area == 12.0f)
  }

  test("Get rectangle extents") {
    val rect = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    assert(rect.left == 2.0f)
    assert(rect.right == 6.0f)
    assert(rect.top == 4.0f)
    assert(rect.bottom == 1.0f)
  }

  test("Get rectangle extents with negative size") {
    val rect = Rect2(6.0f, 4.0f, -4.0f, -3.0f)
    assert(rect.left == 2.0f)
    assert(rect.right == 6.0f)
    assert(rect.top == 4.0f)
    assert(rect.bottom == 1.0f)
  }

  test("Get rectangle corners") {
    val rect = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val bottomLeft = Vec2f(2.0f, 1.0f)
    val topLeft = Vec2f(2.0f, 4.0f)
    val bottomRight = Vec2f(6.0f, 1.0f)
    val topRight = Vec2f(6.0f, 4.0f)
    assert(rect.bottomLeft == bottomLeft)
    assert(rect.topLeft == topLeft)
    assert(rect.bottomRight == bottomRight)
    assert(rect.topRight == topRight)
  }

  test("Get rectangle corners with negative size") {
    val rect = Rect2(6.0f, 4.0f, -4.0f, -3.0f)
    val bottomLeft = Vec2f(2.0f, 1.0f)
    val topLeft = Vec2f(2.0f, 4.0f)
    val bottomRight = Vec2f(6.0f, 1.0f)
    val topRight = Vec2f(6.0f, 4.0f)
    assert(rect.bottomLeft == bottomLeft)
    assert(rect.topLeft == topLeft)
    assert(rect.bottomRight == bottomRight)
    assert(rect.topRight == topRight)
  }

  test("Check if rectangle contains point") {
    val rect = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    assert(rect.containsPoint(3.0f, 2.0f))
    assert(!rect.containsPoint(-1.0f, -1.0f))
    assert(!rect.containsPoint(2.0f, 1.0f))
    assert(rect.containsPoint(2.0f, 1.0f, includeBorders = true))
  }

  test("Check if rectangle with negative size contains point") {
    val rect = Rect2(6.0f, 4.0f, -4.0f, -3.0f)
    assert(rect.containsPoint(3.0f, 2.0f))
    assert(!rect.containsPoint(-1.0f, -1.0f))
    assert(!rect.containsPoint(2.0f, 1.0f))
    assert(rect.containsPoint(2.0f, 1.0f, includeBorders = true))
  }

  test("Check if rectangle contains point using vector") {
    val rect = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val p1 = Vec2f(3.0f, 2.0f)
    val p2 = Vec2f(-1.0f, -1.0f)
    val p3 = Vec2f(2.0f, 1.0f)
    assert(rect.containsPoint(p1))
    assert(!rect.containsPoint(p2))
    assert(!rect.containsPoint(p3))
    assert(rect.containsPoint(p3, includeBorders = true))
  }

  test("Check if two rectangles intersect") {
    val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val rect2 = Rect2(3.0f, 2.0f, 4.0f, 3.0f)
    assert(rect1.intersects(rect2))
    val rect3 = Rect2(8.0f, 1.0f, 3.0f, 2.0f)
    assert(!rect1.intersects(rect3))
    val rect4 = Rect2(6.0f, 2.0f, 4.0f, 3.0f)
    assert(!rect1.intersects(rect4))
    assert(rect1.intersects(rect4, includeBorders = true))
    val rect5 = Rect2(7.0f, 5.0f, -4.0f, -3.0f)
    assert(rect1.intersects(rect5))
  }

  test("Intersection between two rectangles") {
    val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val rect2 = Rect2(3.0f, 2.0f, 4.0f, 3.0f)
    val res = Rect2(3.0f, 2.0f, 3.0f, 2.0f)
    assert(rect1.intersection(rect2) == res)
    val rect3 = Rect2(7.0f, 5.0f, -4.0f, -3.0f)
    assert(rect1.intersection(rect3) == res)
  }

  test("Intersection between two non intersecting rectangles") {
    val rect1 = Rect2(1.0f, 1.0f, 2.0f, 2.0f)
    val rect2 = Rect2(-1.0f, -1.0f, -2.0f, -2.0f)
    val res = Rect2()
    assert(rect1.intersection(rect2) == res)
  }

  test("Check if rectangle intersects line by point and direction") {
    val rect = Rect2(1.0f, 1.0f, 4.0f, 3.0f)
    val p1 = Vec2f(0.0f, 0.5f)
    val d1 = Vec2f(1.0f, 0.3f)
    assert(rect.intersectsLine(p1, d1))
    val p2 = Vec2f(6.0f, 5.0f)
    val d2 = Vec2f(-0.1f, 1.0f)
    assert(!rect.intersectsLine(p2, d2))
  }

  test("Check if rectangle intersects line by slope and intercept") {
    val rect = Rect2(1.0f, 1.0f, 4.0f, 3.0f)
    assert(rect.intersectsLine(0.3f, 0.5f))
    assert(!rect.intersectsLine(1.0f, 7.0f))
  }

  test("Intersection between a rectangle and a line by point and direction") {
    val rect = Rect2(1.0f, 1.0f, 4.0f, 3.0f)
    val p1 = Vec2f(0.0f, 0.5f)
    val d1 = Vec2f(1.0f, 1.0f)
    val res1 = Vec2f(1.0f, 1.5f)
    assert(rect.lineIntersection(p1, d1) == res1)
    val p2 = Vec2f(6.0f, 5.0f)
    val d2 = Vec2f(-0.1f, 1.0f)
    val res2 = rect.lineIntersection(p2, d2)
    assert(res2.x.isNaN && res2.y.isNaN)
  }

  test("Intersection between a rectangle and a line by slope and intercept") {
    val rect = Rect2(1.0f, 1.0f, 4.0f, 3.0f)
    val res1 = Vec2f(1.0f, 1.5f)
    assert(rect.lineIntersection(1.0f, 0.5f) == res1)
    val res2 = rect.lineIntersection(1.0f, 7.0f)
    assert(res2.x.isNaN && res2.y.isNaN)
  }

  test("Rectangle absolute value") {
    val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val rect2 = Rect2(6.0f, 4.0f, -4.0f, -3.0f)
    assert(rect2.abs == rect1)
  }

  test("Check if rectangle encloses another") {
    val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val rect2 = Rect2(3.0f, 2.0f, 4.0f, 3.0f)
    assert(!rect1.encloses(rect2))
    val rect3 = Rect2(2.5f, 1.5f, 3.5f, 2.5f)
    assert(rect1.encloses(rect3))
    val rect4 = Rect2(6.0f, 4.0f, -3.5f, -2.5f)
    assert(rect1.encloses(rect4))
  }

  test("Grow rectangle") {
    val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val res = Rect2(1.5f, 1.5f, 5.5f, 2.0f)
    assert(rect1.grow(0.5f, -0.5f, 1.0f, -0.5f) == res)
  }

  test("Grow rectangle on all sides") {
    val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val res1 = Rect2(0.5f, -0.5f, 7.0f, 6.0f)
    assert(rect1.grow(1.5f) == res1)
    val res2 = Rect2(3.0f, 2.0f, 2.0f, 1.0f)
    assert(rect1.grow(-1.0f) == res2)
    val rect2 = Rect2(6.0f, 4.0f, -4.0f, -3.0f)
    val res3 = Rect2(5.0f, 3.0f, -2.0f, -1.0f)
    assert(rect2.grow(1.0f) == res3)
    val res4 = Rect2(7.0f, 5.0f, -6.0f, -5.0f)
    assert(rect2.grow(-1.0f) == res4)
  }

  test("Expand rectangle to a point") {
    val rect1 = Rect2(0.0f, 0.0f, 5.0f, 2.0f)
    val res1 = Rect2(0.0f, 0.0f, 10.0f, 2.0f)
    assert(rect1.expandTo(10.0f, 0.0f) == res1)
    val res2 = Rect2(-5.0f, 0.0f, 10.0f, 5.0f)
    assert(rect1.expandTo(-5.0f, 5.0f) == res2)
    val rect2 = Rect2(5.0f, 2.0f, -5.0f, -2.0f)
    assert(rect2.expandTo(10.0f, 0.0f) == res1)
  }

  test("Expand rectangle to a point using a vector") {
    val rect1 = Rect2(0.0f, 0.0f, 5.0f, 2.0f)
    val p1 = Vec2f(10.0f, 0.0f)
    val p2 = Vec2f(-5.0f, 5.0f)
    val res1 = Rect2(0.0f, 0.0f, 10.0f, 2.0f)
    assert(rect1.expandTo(p1) == res1)
    val res2 = Rect2(-5.0f, 0.0f, 10.0f, 5.0f)
    assert(rect1.expandTo(p2) == res2)
    val rect2 = Rect2(5.0f, 2.0f, -5.0f, -2.0f)
    assert(rect2.expandTo(p1) == res1)
  }

  test("Merge two rectangles") {
    val rect1 = Rect2(1.0f, 1.0f, 3.0f, 2.0f)
    val rect2 = Rect2(3.0f, 2.0f, 4.0f, 2.0f)
    val res = Rect2(1.0f, 1.0f, 6.0f, 3.0f)
    assert(rect1.merge(rect2) == res)
    val rect3 = Rect2(7.0f, 4.0f, -4.0f, -2.0f)
    assert(rect1.merge(rect3) == res)
  }

  test("Get the bounding circle of a rectangle") {
    val rect = Rect2(1.0f, 1.0f, 3.0f, 2.0f)
    val circle = Circle(2.5f, 2.0f, math.sqrt(3.25).toFloat)
    assert(rect.boundingCircle == circle)
  }

  test("Transform rectangle with a 2x2 matrix") {
    val rotation = Mat2f.rotation(math.Pi / 2.0)
    val rect = Rect2(4.0f, 3.0f)
    val res1 = Rect2(-3.0f, 0.0f, 3.0f, 4.0f)
    assert(rotation * rect == res1)
    val scaling = Mat2f.scaling(2.0f, 3.0f)
    val res2 = Rect2(8.0f, 9.0f)
    assert(scaling * rect == res2)
  }

  test("Transform rectangle with a 2x3 matrix") {
    val translation = Mat2x3f.translation(2.0f, 3.0f)
    val rect = Rect2(2.0f, 1.0f, 4.0f, 2.0f)
    val res1 = Rect2(4.0f, 4.0f, 4.0f, 2.0f)
    assert(translation * rect == res1)
    val transform = Mat2f.rotation(math.Pi / 2.0) * translation
    val res2 = Rect2(-6.0f, 4.0f, 2.0f, 4.0f)
    assert(transform * rect == res2)
  }

  test("Transform a rectangle with a 3x3 matrix") {
    val translation = Mat3f.translation(2.0f, 3.0f)
    val rect = Rect2(2.0f, 1.0f, 4.0f, 2.0f)
    val res1 = Rect2(4.0f, 4.0f, 4.0f, 2.0f)
    assert(translation * rect == res1)
    val transform = Mat3f.rotationZ(math.Pi / 2.0) * translation
    val res2 = Rect2(-6.0f, 4.0f, 2.0f, 4.0f)
    assert(transform * rect == res2)
  }

  // TODO: Inverse transform

  test("Check if two rectangles are congruent") {
    val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val rect2 = Rect2(6.0f, 4.0f, -4.0f, -3.0f)
    assert(rect1.isCongruentTo(rect2))
    assert(rect1.isCongruentTo(rect1))
  }

  test("Rectangle equals approx") {
    val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    val rect2 = Rect2(1.9999999f, 1.0000001f, 4.0000001f, 2.9999999f)
    val rect3 = Rect2(6.0f, 4.0f, -4.0f, -3.0f)
    assert(rect1 != rect2)
    assert(rect1 ~= rect2)
    assert(!(rect1 ~= rect3))
  }

  test("Cast rectangle to int") {
    val rect1 = Rect2(2.1f, 1.9f, 4.4f, 3.2f)
    val rect2 = Rect2i(2, 1, 4, 3)
    assert(rect1.toInt == rect2)
  }

  test("Construct rectangle from points") {
    val p1 = Vec2f(2.0f, 1.0f)
    val p2 = Vec2f(6.0f, 4.0f)
    val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    assert(Rect2.fromPoints(p1, p2) == rect1)
    val p3 = Vec2f(0.0f, 5.0f)
    val rect2 = Rect2(0.0f, 1.0f, 2.0f, 4.0f)
    assert(Rect2.fromPoints(p1, p3) == rect2)
  }
}
