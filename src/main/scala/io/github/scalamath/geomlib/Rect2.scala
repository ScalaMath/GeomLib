package io.github.scalamath.geomlib

import io.github.scalamath.FloatEqualsApprox
import io.github.scalamath.vecmatlib.{Mat2f, Mat2x3f, Vec2f}

/**
 * A 2D axis-aligned bounding box using floating point coordinates.
 * It is defined by two 2D vectors: a [[position]], usually the bottom left corner, and a [[size]], generally positive.
 *
 * The size of the rectangle can be negative.
 * Rectangles with a negative size are considered valid and do not have their origin at the bottom left corner.
 * The size is considered to be the vector from the rectangle's origin to its [[end]] point.
 * Use [[abs]] to get an equivalent rectangle with a non-negative size.
 *
 * For integer coordinates, use [[Rect2i]].
 * The 3D equivalent is [[AABB]].
 *
 * @constructor Constructs a rectangle at the given x and y coordinates with the given width and height.
 * @param x The x coordinate of the rectangle's position.
 * @param y The y coordinate of the rectangle's position.
 * @param width The width of the rectangle.
 * @param height The height of the rectangle.
 */
case class Rect2(x: Float, y: Float, width: Float, height: Float) {

  /**
   * Constructs a rectangle at the given position with the given size.
   *
   * @param position The rectangle's position.
   * @param size The size of the rectangle.
   */
  def this(position: Vec2f, size: Vec2f) = this(position.x, position.y, size.x, size.y)

  /**
   * Constructs a rectangle at the given position with the given width and height.
   *
   * @param position The rectangle's position.
   * @param width The width of the rectangle.
   * @param height The height of the rectangle.
   */
  def this(position: Vec2f, width: Float, height: Float) = this(position.x, position.y, width, height)

  /**
   * Constructs a rectangle at the given x and y coordinates with the given size.
   *
   * @param x The x coordinate of the rectangle's position.
   * @param y The y coordinate of the rectangle's position.
   * @param size The size of the rectangle.
   */
  def this(x: Float, y: Float, size: Vec2f) = this(x, y, size.x, size.y)

  /**
   * Constructs a rectangle at position `(0, 0)` with the given size.
   *
   * @param width The width of the rectangle.
   * @param height The height of the rectangle.
   */
  def this(width: Float, height: Float) = this(0.0f, 0.0f, width, height)

  /**
   * Constructs an empty rectangle, whose position and size are both `(0, 0)`.
   */
  def this() = this(0.0f, 0.0f)

  /**
   * Returns the origin position of this rectangle.
   * Corresponds to the rectangle's [[bottomLeft]] corner if the [[size]] of the rectangle is positive.
   *
   * @return The origin position of this rectangle.
   */
  def position: Vec2f = Vec2f(this.x, this.y)

  /**
   * Returns the size of this rectangle.
   * Can be negative.
   *
   * The size of the rectangle is the vector from the rectangle's [[position]] to its [[end]].
   *
   * @return The size of this rectangle.
   */
  def size: Vec2f = Vec2f(this.width, this.height)

  /**
   * Returns this rectangle's end point.
   * Corresponds to the rectangle's [[topRight]] corner if the [[size]] of the rectangle is positive.
   *
   * Equivalent to [[position]] + [[size]].
   *
   * @return The end point of this rectangle.
   */
  def end: Vec2f = Vec2f(this.x + this.width, this.y + this.height)

  /**
   * Returns the center point of this rectangle.
   *
   * Equivalent to [[position]] + ([[size]] / 2).
   *
   * @return The center point of this rectangle.
   */
  def center: Vec2f = Vec2f(this.x + this.width / 2.0f, this.y + this.height / 2.0f)

  /**
   * Returns the area of this rectangle.
   *
   * Equivalent to `width * height`.
   *
   * @return The area of this rectangle.
   */
  def area: Float = this.width * this.height

  /**
   * Returns the x coordinate of the leftmost point of the rectangle.
   * This method is guaranteed to return the left extent of the rectangle regardless of the sign of its [[size]].
   *
   * @return The x coordinate of the leftmost point of the rectangle.
   * @see [[bottomLeft]] and [[topLeft]].
   */
  def left: Float = math.min(this.x, this.x + this.width)

  /**
   * Returns the x coordinate of the rightmost point of the rectangle.
   * This method is guaranteed to return the right extent of the rectangle regardless of the sign of its [[size]].
   *
   * @return The x coordinate of the rightmost point of the rectangle.
   * @see [[bottomRight]] and [[topRight]].
   */
  def right: Float = math.max(this.x, this.x + this.width)

  /**
   * Returns the y coordinate of the highest point of the rectangle.
   * This method is guaranteed to return the top extent of the rectangle regardless of the sign of its [[size]].
   *
   * @return The y coordinate of the highest point of the rectangle.
   * @see [[topLeft]] and [[topRight]].
   */
  def top: Float = math.max(this.y, this.y + this.height)

  /**
   * Returns the y coordinate of the lowest point of the rectangle.
   * This method is guaranteed to return the bottom extent of the rectangle regardless of the sign of its [[size]].
   *
   * @return The y coordinate of the lowest point of the rectangle.
   * @see [[bottomLeft]] and [[bottomRight]].
   */
  def bottom: Float = math.min(this.y, this.y + this.height)

  /**
   * Returns the position of the bottom left corner of the rectangle.
   * This method is guaranteed to return the bottom left corner of the rectangle regardless of the sign of its [[size]].
   *
   * @return The position of the bottom left corner of the rectangle.
   * @see [[bottom]] and [[left]].
   */
  def bottomLeft: Vec2f = Vec2f(this.left, this.bottom)

  /**
   * Returns the position of the top left corner of the rectangle.
   * This method is guaranteed to return the top left corner of the rectangle regardless of the sign of its [[size]].
   *
   * @return The position of the top left corner of the rectangle.
   * @see [[top]] and [[left]].
   */
  def topLeft: Vec2f = Vec2f(this.left, this.top)

  /**
   * Returns the position of the bottom right corner of the rectangle.
   * This method is guaranteed to return the bottom right corner of the rectangle regardless of the sign of its [[size]].
   *
   * @return The position of the bottom right corner of the rectangle.
   * @see [[bottom]] and [[right]].
   */
  def bottomRight: Vec2f = Vec2f(this.right, this.bottom)

  /**
   * Returns the position of the top right corner of the rectangle.
   * This method is guaranteed to return the top right corner of the rectangle regardless of the sign of its [[size]].
   *
   * @return The position of the top right corner of the rectangle.
   * @see [[top]] and [[right]].
   */
  def topRight: Vec2f = Vec2f(this.right, this.top)

  /**
   * Checks if this rectangle contains the point at the given coordinates.
   * If `includeBorders` is true, points laying on the borders of the rectangle will be considered inside the rectangle.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @param includeBorders If true, points laying on the borders of the rectangle will be considered inside the rectangle.
   * @return True if the point at the given coordinates is inside this rectangle, otherwise false.
   */
  def containsPoint(x: Float, y: Float, includeBorders: Boolean): Boolean = {
    if(includeBorders) {
      x >= this.left && x <= this.right && y >= this.bottom && y <= this.top
    } else {
      x > this.left && x < this.right && y > this.bottom && y < this.top
    }
  }

  /**
   * Checks if the given point is inside this rectangle.
   * If `includeBorders` is true, points laying on the borders of the rectangle will be considered inside the rectangle.
   *
   * @param point The point.
   * @param includeBorders If true, points laying on the borders of the rectangle will be considered inside the rectangle.
   * @return True if the given point is inside this rectangle, otherwise false.
   */
  def containsPoint(point: Vec2f, includeBorders: Boolean): Boolean = this.containsPoint(point.x, point.y, includeBorders)

  /**
   * Checks if this rectangle contains the point at the given coordinates.
   * Points laying on the borders of the rectangle are not considered to be inside the rectangle.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @return True if the point at the given coordinates is inside this rectangle, otherwise false.
   */
  def containsPoint(x: Float, y: Float): Boolean = this.containsPoint(x, y, includeBorders = false)

  /**
   * Checks if the given point is inside this rectangle.
   * Points laying on the borders of the rectangle are not considered to be inside the rectangle.
   *
   * @param point The point.
   * @return True if the given point is inside this rectangle, otherwise false.
   */
  def containsPoint(point: Vec2f): Boolean = this.containsPoint(point.x, point.y)

  /**
   * Checks if this rectangle overlaps with the given one.
   * If `includeBorders` is true, rectangles touching each other by their edge are considered as overlapping.
   *
   * @param rect The other rectangle.
   * @param includeBorders If true, rectangles touching each other by their edge are considered as overlapping.
   * @return True if this rectangle overlaps with the given one, otherwise false.
   * @see [[encloses]]
   */
  def intersects(rect: Rect2, includeBorders: Boolean): Boolean = {
    if(includeBorders) {
      !(this.right < rect.left || this.left > rect.right || this.top < rect.bottom || this.bottom > rect.top)
    } else {
      !(this.right <= rect.left || this.left >= rect.right || this.top <= rect.bottom || this.bottom >= rect.top)
    }
  }

  /**
   * Checks if this rectangle overlaps with the given one.
   * The edges of both rectangles are excluded.
   *
   * @param rect The other rectangle.
   * @return True if this rectangle overlaps with the given one, otherwise false.
   * @see [[encloses]]
   */
  def intersects(rect: Rect2): Boolean = this.intersects(rect, includeBorders = false)

  /**
   * Returns the intersection between this rectangle and the given one or an empty rectangle if the two do not intersect.
   * This method always returns a rectangle with positive [[size]].
   *
   * @param rect The other rectangle.
   * @return The intersection between this rectangle and the given one.
   * @see [[intersects]]
   */
  def intersection(rect: Rect2): Rect2 = {
    // TODO: Avoid doing this twice
    if(!this.intersects(rect, includeBorders = true)) {
      Rect2()
    } else {
      Rect2.fromPoints(math.max(this.left, rect.left), math.max(this.bottom, rect.bottom), math.min(this.right, rect.right), math.min(this.top, rect.top))
    }
  }

  /**
   * Returns a rectangle equivalent to this one with non-negative [[size]] and its [[position]] being the bottom left corner.
   *
   * @return A rectangle equivalent to this one with non-negative size and its position being the bottom left corner.
   */
  def abs: Rect2 = Rect2(this.left, this.bottom, this.width.abs, this.height.abs)

  /**
   * Checks if this rectangle completely encloses the given one.
   *
   * @param rect The other rectangle.
   * @return True if this rectangle completely encloses the given one, otherwise false.
   * @see [[intersects]]
   */
  def encloses(rect: Rect2): Boolean = this.left <= rect.left && this.right >= rect.right && this.top >= rect.top && this.bottom <= rect.bottom

  /**
   * Returns a copy of this rectangle with its [[left]], [[top]], [[right]], and [[bottom]] sides extended by the given amount.
   * A negative amount shrinks the rectangle.
   *
   * @param left Amount by which the left side should grow.
   * @param top Amount by which the top side should grow.
   * @param right Amount by which the right side should grow.
   * @param bottom Amount by which the bottom side should grow.
   * @return A copy of this rectangle with all sides extended by the given amounts.
   */
  def grow(left: Float, top: Float, right: Float, bottom: Float): Rect2 = Rect2(this.x - left, this.y - top, this.width + left + right, this.height + top + bottom)

  /**
   * Returns a copy of this rectangle with all sides extended by the given amount.
   * A negative amount shrinks the rectangle.
   *
   * @param amount Amount by which the rectangle should grow.
   * @return A copy of this rectangle with all sides extended by the given amount.
   */
  def grow(amount: Float): Rect2 = Rect2(this.x - amount, this.y - amount, this.width + amount * 2.0f, this.height + amount * 2.0f)

  /**
   * Returns a copy of this rectangle expanded to align the edges with the given point.
   *
   * If the given point is outside the rectangle, the resulting rectangle will have the given point on one of its edges.
   * The result will be an equivalent rectangle if the given point is inside this rectangle.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @return A copy of this rectangle expanded to align the edges with the given point.
   */
  def expandTo(x: Float, y: Float): Rect2 = Rect2.fromPoints(math.min(this.left, x), math.min(this.bottom, y), math.max(this.right, x), math.max(this.top, y))

  /**
   * Returns a copy of this rectangle expanded to align the edges with the given point.
   *
   * If the given point is outside the rectangle, the resulting rectangle will have the given point on one of its edges.
   * The result will be an equivalent rectangle if the given point is inside this rectangle.
   *
   * @param point The point.
   * @return A copy of this rectangle expanded to align the edges with the given point.
   */
  def expandTo(point: Vec2f): Rect2 = this.expandTo(point.x, point.y)

  /**
   * Returns a rectangle that encloses both this one and the given one around the edges.
   *
   * @param rect The other rectangle.
   * @return A rectangle that encloses both this one and the given one around the edges.
   * @see [[encloses]]
   */
  def merge(rect: Rect2): Rect2 = Rect2.fromPoints(math.min(this.left, rect.left), math.min(this.bottom, rect.bottom), math.max(this.right, rect.right), math.max(this.top, rect.top))

  def transform(m: Mat2f): Rect2 = {
    val x = m.col0 * this.width
    val y = m.col1 * this.height
    val pos = m * this.position
    // TODO: Creates too much garbage
    Rect2(pos, 0.0f, 0.0f).expandTo(pos + x).expandTo(pos + y).expandTo(pos + x + y)
  }

  def transform(m: Mat2x3f): Rect2 = {
    val x = m.col0 * this.width
    val y = m.col1 * this.height
    val pos = m * (this.position, 1.0f)
    // TODO: Creates too much garbage
    Rect2(pos, 0.0f, 0.0f).expandTo(pos + x).expandTo(pos + y).expandTo(pos + x + y)
  }

  def inverseTransform(m: Mat2f): Rect2 = this.transform(m.inverse)

  def *(m: Mat2f): Rect2 = this.inverseTransform(m)

  /**
   * Checks if this rectangle is approximately equivalent to the given one using an internal epsilon.
   *
   * Unlike the `==` operator, this method will treat equivalent rectangles with a different origin [[position]] and a [[size]] of a different sign to be equal.
   *
   * @param rect The other rectangle.
   * @return True if this rectangle is approximately equivalent to the given one, otherwise false.
   */
  def ~=(rect: Rect2): Boolean = (this.left ~= rect.left) && (this.right ~= rect.right) && (this.top ~= rect.top) && (this.bottom ~= rect.bottom)

  /**
   * Checks if this rectangle is approximately equivalent to the given one using an internal epsilon.
   *
   * Unlike the `equals` method, this method will treat equivalent rectangles with a different origin [[position]] and a [[size]] of a different sign to be equal.
   *
   * This method can be used in place of the `~=` operator for better interoperability with Java.
   *
   * @param rect The other rectangle.
   * @return True if this rectangle is approximately equivalent to the given one, otherwise false.
   */
  def equalsApprox(rect: Rect2): Boolean = this ~= rect
}

/**
 * Factory methods and implicits for rectangles.
 */
object Rect2 {

  /**
   * Constructs a rectangle at the given position with the given size.
   *
   * Allows to construct a rectangle without using the `new` keyword in Scala.
   *
   * @param position The rectangle's position.
   * @param size The size of the rectangle.
   * @return The newly instantiated rectangle.
   */
  def apply(position: Vec2f, size: Vec2f): Rect2 = new Rect2(position, size)

  /**
   * Constructs a rectangle at the given position with the given width and height.
   *
   * Allows to construct a rectangle without using the `new` keyword in Scala.
   *
   * @param position The rectangle's position.
   * @param width The width of the rectangle.
   * @param height The height of the rectangle.
   * @return The newly instantiated rectangle.
   */
  def apply(position: Vec2f, width: Float, height: Float): Rect2 = new Rect2(position, width, height)

  /**
   * Constructs a rectangle at the given x and y coordinates with the given size.
   *
   * Allows to construct a rectangle without using the `new` keyword in Scala.
   *
   * @param x The x coordinate of the rectangle's position.
   * @param y The y coordinate of the rectangle's position.
   * @param size The size of the rectangle.
   * @return The newly instantiated rectangle.
   */
  def apply(x: Float, y: Float, size: Vec2f): Rect2 = new Rect2(x, y, size)

  /**
   * Constructs a rectangle at position `(0, 0)` with the given size.
   *
   * Allows to construct a rectangle without using the `new` keyword in Scala.
   *
   * @param width The width of the rectangle.
   * @param height The height of the rectangle.
   * @return The newly instantiated rectangle.
   */
  def apply(width: Float, height: Float): Rect2 = new Rect2(width, height)

  /**
   * Constructs an empty rectangle, whose position and size are both `(0, 0)`.
   *
   * Allows to construct a rectangle without using the `new` keyword in Scala.
   *
   * @return The newly instantiated rectangle.
   */
  def apply(): Rect2 = new Rect2()

  /**
   * Constructs a rectangle using the two given points as the bounds of the rectangle.
   * The given points will be two of the corners of the resulting rectangle.
   *
   * The resulting rectangle is guaranteed to have a positive size.
   *
   * @param x1 The x coordinate of the first point.
   * @param y1 The y coordinate of the first point.
   * @param x2 The x coordinate of the second point.
   * @param y2 The y coordinate of the second point.
   * @return The newly instantiated rectangle.
   */
  def fromPoints(x1: Float, y1: Float, x2: Float, y2: Float): Rect2 = Rect2(math.min(x1, x2), math.min(y1, y2), (x1 - x2).abs, (y1 - y2).abs)

  /**
   * Constructs a rectangle using the two given points as the bounds of the rectangle.
   * The given points will be two of the corners of the resulting rectangle.
   *
   * The resulting rectangle is guaranteed to have a positive size.
   *
   * @param p1 The first point.
   * @param p2 The second point.
   * @return The newly instantiated rectangle.
   */
  def fromPoints(p1: Vec2f, p2: Vec2f): Rect2 = this.fromPoints(p1.x, p1.y, p2.x, p2.y)

  implicit class Transform2x2(val self: Mat2f) extends AnyVal {

    def *(rect: Rect2): Rect2 = rect.transform(self)
  }

  implicit class Transform2x3(val self: Mat2x3f) extends AnyVal {

    def *(rect: Rect2): Rect2 = rect.transform(self)
  }
}