package io.github.scalamath.geomlib

import io.github.scalamath.vecmatlib.Vec2i
import org.scalatest.funsuite.AnyFunSuite

class Rect2iSuite extends AnyFunSuite {

  test("Get rectangle position") {
    val rect = Rect2i(1, 2, 3, 4)
    val res = Vec2i(1, 2)
    assert(rect.position == res)
  }

  test("Get rectangle size") {
    val rect = Rect2i(1, 2, 3, 4)
    val res = Vec2i(3, 4)
    assert(rect.size == res)
  }

  test("Get rectangle end") {
    val rect1 = Rect2i(1, 2, 3, 4)
    val res1 = Vec2i(4, 6)
    assert(rect1.end == res1)
    val rect2 = Rect2i(1, 2, -3, -4)
    val res2 = Vec2i(-2, -2)
    assert(rect2.end == res2)
  }

  test("Get rectangle center") {
    val rect = Rect2i(1, 2, 3, 4)
    val res = Vec2i(2, 4)
    assert(rect.center == res)
  }

  test("Get rectangle area") {
    val rect = Rect2i(4, 3)
    assert(rect.area == 12)
  }

  test("Get rectangle extents") {
    val rect = Rect2i(2, 1, 4, 3)
    assert(rect.left == 2)
    assert(rect.right == 6)
    assert(rect.top == 4)
    assert(rect.bottom == 1)
  }

  test("Get rectangle extents with negative size") {
    val rect = Rect2i(6, 4, -4, -3)
    assert(rect.left == 2)
    assert(rect.right == 6)
    assert(rect.top == 4)
    assert(rect.bottom == 1)
  }

  test("Get rectangle corners") {
    val rect = Rect2i(2, 1, 4, 3)
    val bottomLeft = Vec2i(2, 1)
    val topLeft = Vec2i(2, 4)
    val bottomRight = Vec2i(6, 1)
    val topRight = Vec2i(6, 4)
    assert(rect.bottomLeft == bottomLeft)
    assert(rect.topLeft == topLeft)
    assert(rect.bottomRight == bottomRight)
    assert(rect.topRight == topRight)
  }

  test("Get rectangle corners with negative size") {
    val rect = Rect2i(6, 4, -4, -3)
    val bottomLeft = Vec2i(2, 1)
    val topLeft = Vec2i(2, 4)
    val bottomRight = Vec2i(6, 1)
    val topRight = Vec2i(6, 4)
    assert(rect.bottomLeft == bottomLeft)
    assert(rect.topLeft == topLeft)
    assert(rect.bottomRight == bottomRight)
    assert(rect.topRight == topRight)
  }

  test("Check if rectangle contains point") {
    val rect = Rect2i(2, 1, 4, 3)
    assert(rect.containsPoint(3, 2))
    assert(!rect.containsPoint(-1, -1))
    assert(!rect.containsPoint(2, 1))
    assert(rect.containsPoint(2, 1, includeBorders = true))
  }

  test("Check if rectangle with negative size contains point") {
    val rect = Rect2i(6, 4, -4, -3)
    assert(rect.containsPoint(3, 2))
    assert(!rect.containsPoint(-1, -1))
    assert(!rect.containsPoint(2, 1))
    assert(rect.containsPoint(2, 1, includeBorders = true))
  }

  test("Check if rectangle contains point using vector") {
    val rect = Rect2i(2, 1, 4, 3)
    val p1 = Vec2i(3, 2)
    val p2 = Vec2i(-1, -1)
    val p3 = Vec2i(2, 1)
    assert(rect.containsPoint(p1))
    assert(!rect.containsPoint(p2))
    assert(!rect.containsPoint(p3))
    assert(rect.containsPoint(p3, includeBorders = true))
  }

  test("Check if two rectangles intersect") {
    val rect1 = Rect2i(2, 1, 4, 3)
    val rect2 = Rect2i(3, 2, 4, 3)
    assert(rect1.intersects(rect2))
    val rect3 = Rect2i(8, 1, 3, 2)
    assert(!rect1.intersects(rect3))
    val rect4 = Rect2i(6, 2, 4, 3)
    assert(!rect1.intersects(rect4))
    assert(rect1.intersects(rect4, includeBorders = true))
    val rect5 = Rect2i(7, 5, -4, -3)
    assert(rect1.intersects(rect5))
  }

  test("Intersection between two rectangles") {
    val rect1 = Rect2i(2, 1, 4, 3)
    val rect2 = Rect2i(3, 2, 4, 3)
    val res = Rect2i(3, 2, 3, 2)
    assert(rect1.intersection(rect2) == res)
    val rect3 = Rect2i(7, 5, -4, -3)
    assert(rect1.intersection(rect3) == res)
  }

  test("Intersection between two non intersecting rectangles") {
    val rect1 = Rect2i(1, 1, 2, 2)
    val rect2 = Rect2i(-1, -1, -2, -2)
    val res = Rect2i()
    assert(rect1.intersection(rect2) == res)
  }

  test("Rectangle absolute value") {
    val rect1 = Rect2i(2, 1, 4, 3)
    val rect2 = Rect2i(6, 4, -4, -3)
    assert(rect2.abs == rect1)
  }

  test("Check if rectangle encloses another") {
    val rect1 = Rect2i(2, 1, 4, 3)
    val rect2 = Rect2i(3, 2, 4, 3)
    assert(!rect1.encloses(rect2))
    val rect3 = Rect2i(2, 1, 3, 2)
    assert(rect1.encloses(rect3))
    val rect4 = Rect2i(6, 4, -3, -2)
    assert(rect1.encloses(rect4))
  }

  test("Grow rectangle") {
    val rect1 = Rect2i(2, 1, 4, 3)
    val res = Rect2i(1, 2, 6, 4)
    assert(rect1.grow(1, 2, 1, -1) == res)
  }

  test("Grow rectangle on all sides") {
    val rect1 = Rect2i(2, 1, 4, 3)
    val res1 = Rect2i(0, -1, 8, 7)
    assert(rect1.grow(2) == res1)
    val res2 = Rect2i(3, 2, 2, 1)
    assert(rect1.grow(-1) == res2)
    val rect2 = Rect2i(6, 4, -4, -3)
    val res3 = Rect2i(5, 3, -2, -1)
    assert(rect2.grow(1) == res3)
    val res4 = Rect2i(7, 5, -6, -5)
    assert(rect2.grow(-1) == res4)
  }

  test("Expand rectangle to a point") {
    val rect1 = Rect2i(0, 0, 5, 2)
    val res1 = Rect2i(0, 0, 10, 2)
    assert(rect1.expandTo(10, 0) == res1)
    val res2 = Rect2i(-5, 0, 10, 5)
    assert(rect1.expandTo(-5, 5) == res2)
    val rect2 = Rect2i(5, 2, -5, -2)
    assert(rect2.expandTo(10, 0) == res1)
  }

  test("Expand rectangle to a point using a vector") {
    val rect1 = Rect2i(0, 0, 5, 2)
    val p1 = Vec2i(10, 0)
    val p2 = Vec2i(-5, 5)
    val res1 = Rect2i(0, 0, 10, 2)
    assert(rect1.expandTo(p1) == res1)
    val res2 = Rect2i(-5, 0, 10, 5)
    assert(rect1.expandTo(p2) == res2)
    val rect2 = Rect2i(5, 2, -5, -2)
    assert(rect2.expandTo(p1) == res1)
  }

  test("Merge two rectangles") {
    val rect1 = Rect2i(1, 1, 3, 2)
    val rect2 = Rect2i(3, 2, 4, 2)
    val res = Rect2i(1, 1, 6, 3)
    assert(rect1.merge(rect2) == res)
    val rect3 = Rect2i(7, 4, -4, -2)
    assert(rect1.merge(rect3) == res)
  }

  test("Get the bounding circle of a rectangle") {
    val rect = Rect2i(1, 1, 3, 2)
    val circle = Circle(2.5f, 2, math.sqrt(3.25).toFloat)
    assert(rect.boundingCircle == circle)
  }

  test("Check if two rectangles are congruent") {
    val rect1 = Rect2i(2, 1, 4, 3)
    val rect2 = Rect2i(6, 4, -4, -3)
    assert(rect1.isCongruentTo(rect2))
    assert(rect1.isCongruentTo(rect1))
  }

  test("Rectangle as float") {
    val rect1 = Rect2i(2, 1, 4, 3)
    val rect2 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
    assert(rect1.toFloat == rect2)
  }

  test("Construct rectangle from points") {
    val p1 = Vec2i(2, 1)
    val p2 = Vec2i(6, 4)
    val rect1 = Rect2i(2, 1, 4, 3)
    assert(Rect2i.fromPoints(p1, p2) == rect1)
    val p3 = Vec2i(0, 5)
    val rect2 = Rect2i(0, 1, 2, 4)
    assert(Rect2i.fromPoints(p1, p3) == rect2)
  }
}
