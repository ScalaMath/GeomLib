package io.github.scalamath.geomlib

import io.github.scalamath.{FloatEqualsApprox, geomlib}
import io.github.scalamath.vecmatlib.Vec2f

/**
 * A 2D circle, defined by a [[center]] and a radius.
 *
 * The radius of a circle is expected to be positive.
 * Many methods in this class will return incorrect results if the radius is negative.
 *
 * The 3D equivalent is a [[Sphere]].
 *
 * @constructor Constructs a circle with the given center and radius.
 * @param x The x coordinate of the center of the circle.
 * @param y The y coordinate of the center of the circle.
 * @param radius The radius of the circle. Should be positive.
 */
case class Circle(x: Float, y: Float, radius: Float) {

  /**
   * Constructs a circle with the given center and radius.
   *
   * @param center The center of the circle.
   * @param radius The radius of the circle. Should be positive.
   */
  def this(center: Vec2f, radius: Float) = this(center.x, center.y, radius)

  /**
   * Constructs a circle centered in the origin with the given radius.
   *
   * @param radius The radius of the circle. Should be positive.
   */
  def this(radius: Float) = this(0.0f, 0.0f, radius)

  /**
   * Constructs an empty circle centered in the origin with radius `0`.
   */
  def this() = this(0.0f)

  /**
   * Constructs a circle with the given center and the given point.
   * The radius of the circle will be the distance from the center to the given point.
   *
   * @param center The center of the circle.
   * @param point A point on the circumference.
   */
  def this(center: Vec2f, point: Vec2f) = this(center, center.distanceTo(point))

  /**
   * Constructs a circle with the given center and the given point.
   * The radius of the circle will be the distance from the center to the given point.
   *
   * @param x The x coordinate of the center of the circle.
   * @param y The y coordinate of the center of the circle.
   * @param point A point on the circumference.
   */
  def this(x: Float, y: Float, point: Vec2f) = this(x, y, point.distanceTo(x, y))

  /**
   * Constructs a circle with the given center and the given point.
   * The radius of the circle will be the distance from the center to the given point.
   *
   * @param center The center of the circle.
   * @param x The x coordinate of a point on the circumference.
   * @param y The y coordinate of a point on the circumference.
   */
  def this(center: Vec2f, x: Float, y: Float) = this(center, center.distanceTo(x, y))

  /**
   * Constructs a circle passing through the three given points.
   *
   * @param p1 The first point.
   * @param p2 The second point.
   * @param p3 The third point.
   */
  def this(p1: Vec2f, p2: Vec2f, p3: Vec2f) = this(geomlib.lineIntersection((p1 + p2) / 2.0f, (p2 - p1).aspect, (p2 + p3) / 2.0f, (p3 - p2).aspect), p1)

  /**
   * Returns the center of the circle.
   *
   * @return The center of the circle.
   */
  def center: Vec2f = Vec2f(this.x, this.y)

  /**
   * Returns the diameter of the circle.
   * Equivalent to `radius * 2`.
   *
   * @return The diameter of the circle.
   */
  def diameter: Float = this.radius * 2.0f

  /**
   * Returns the length of the circumference of the circle.
   *
   * @return The length of the circumference of the circle.
   */
  def circumference: Float = (this.diameter * math.Pi).toFloat

  /**
   * Returns the squared radius of this circle.
   * Equivalent to `radius * radius`.
   *
   * @return The squared radius of this circle.
   */
  def radiusSquared: Float = this.radius * this.radius

  /**
   * Returns the area of this circle.
   *
   * @return The area of this circle.
   */
  def area: Float = (this.radiusSquared * math.Pi).toFloat

  /**
   * Checks if this circle contains the point at the given coordinates.
   * If `includeCircumference` is true, points laying on the circumference will be considered to be inside the circle.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @param includeCircumference If true, points laying on the circumference will be considered to be inside the circle.
   * @return True if the point at the given coordinates is inside this circle, otherwise false.
   */
  def containsPoint(x: Float, y: Float, includeCircumference: Boolean): Boolean = {
    if(includeCircumference) {
      this.center.distanceSquaredTo(x, y) <= this.radiusSquared
    } else {
      this.center.distanceSquaredTo(x, y) < this.radiusSquared
    }
  }

  /**
   * Checks if this circle contains the point at the given coordinates.
   * Points laying on the circumference are not considered to be inside the circle.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @return True if the point at the given coordinates is inside this circle, otherwise false.
   */
  def containsPoint(x: Float, y: Float): Boolean = this.containsPoint(x, y, includeCircumference = false)

  /**
   * Checks if this circle contains the given point.
   * If `includeCircumference` is true, points laying on the circumference will be considered inside the circle.
   *
   * @param point The point.
   * @param includeCircumference If true, points laying on the circumference will be considered inside the circle.
   * @return True if the given point is inside this circle, otherwise false.
   */
  def containsPoint(point: Vec2f, includeCircumference: Boolean): Boolean = {
    if(includeCircumference) {
      this.center.distanceSquaredTo(point) <= this.radiusSquared
    } else {
      this.center.distanceSquaredTo(point) < this.radiusSquared
    }
  }

  /**
   * Checks if this circle contains the given point.
   * Points laying on the circumference are not considered to be inside the circle.
   *
   * @param point The point.
   * @return True if the given point is inside this circle, otherwise false.
   */
  def containsPoint(point: Vec2f): Boolean = this.containsPoint(point, includeCircumference = false)

  /**
   * Checks if the point at the given coordinates lays on the circumference of this circle by checking if its distance from the center is approximately equal to the radius.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @return True if the point at the given coordinates lays on the circumference of this circle, otherwise false.
   */
  def isPointOnCircumference(x: Float, y: Float): Boolean = this.center.distanceSquaredTo(x, y) ~= this.radiusSquared

  /**
   * Checks if the given point lays on the circumference of this circle by checking if its distance from the center is approximately equal to the radius.
   *
   * @param point The point.
   * @return True if the given point lays on the circumference of this circle, otherwise false.
   */
  def isPointOnCircumference(point: Vec2f): Boolean = this.center.distanceSquaredTo(point) ~= this.radiusSquared

  /**
   * Checks if this circle intersects the given one.
   * If `includeCircumference` is true, circles touching each other by a single point will be considered to be intersecting.
   *
   * @param circle The other circle.
   * @param includeCircumference If true, circles touching each other by a single point will be considered to be intersecting.
   * @return True if this circle intersects the given one, otherwise false.
   */
  def intersects(circle: Circle, includeCircumference: Boolean): Boolean = {
    if(includeCircumference) {
      this.center.distanceTo(circle.x, circle.y) <= this.radius + circle.radius
    } else {
      this.center.distanceTo(circle.x, circle.y) < this.radius + circle.radius
    }
  }

  /**
   * Checks if this circle intersects the given one.
   * Circles touching each other by a single point are not considered to be intersecting.
   *
   * @param circle The other circle.
   * @return True if this circle intersects the given one, otherwise false.
   */
  def intersects(circle: Circle): Boolean = this.intersects(circle, includeCircumference = false)

  /**
   * Checks if this circle completely encloses the given one.
   *
   * @param circle The other circle.
   * @return True if this circle completely encloses the given one, otherwise false.
   */
  def encloses(circle: Circle): Boolean = this.containsPoint(circle.x, circle.y) && this.radius - this.center.distanceTo(circle.center) >= circle.radius

  /**
   * Returns a circle that encloses both this one and the given one.
   *
   * @param circle The other circle.
   * @return A circle that encloses both this one and the given one.
   */
  def merge(circle: Circle): Circle = Circle((this.x + circle.x) / 2.0f, (this.y + circle.y) / 2.0f, (this.center.distanceTo(circle.center) + this.radius + circle.radius) / 2.0f)

  /**
   * Returns a rectangle that completely encloses this circle.
   *
   * @return The bounding rectangle of this circle.
   */
  def boundingRect: Rect2 = Rect2(this.x - this.radius, this.y - this.radius, this.diameter, this.diameter)

  /**
   * Checks if this circle is approximately equal to the given one by checking if positions and radii are approximately equal using an internal epsilon.
   *
   * @param circle The other circle.
   * @return True if this circle is approximately equal to the given one, otherwise false.
   */
  def ~=(circle: Circle): Boolean = (this.x ~= circle.x) && (this.y ~= circle.y) && (this.radius ~= circle.radius)

  /**
   * Checks if this circle is approximately equal to the given one by checking if positions and radii are approximately equal using an internal epsilon.
   *
   * This method can be used in place of the `~=` operator for better interoperability with Java.
   *
   * @param circle The other circle.
   * @return True if this circle is approximately equal to the given one, otherwise false.
   */
  def equalsApprox(circle: Circle): Boolean = this ~= circle
}

/**
 * Factory methods for circles.
 */
object Circle {

  /**
   * Constructs a circle with the given center and radius.
   *
   * Allows to construct a circle without using the `new` keyword in Scala.
   *
   * @param center The center of the circle.
   * @param radius The radius of the circle. Should be positive.
   * @return The newly instantiated circle.
   */
  def apply(center: Vec2f, radius: Float): Circle = new Circle(center, radius)

  /**
   * Constructs a circle centered in the origin with the given radius.
   *
   * Allows to construct a circle without using the `new` keyword in Scala.
   *
   * @param radius The radius of the circle. Should be positive.
   * @return The newly instantiated circle.
   */
  def apply(radius: Float): Circle = new Circle(radius)

  /**
   * Constructs an empty circle centered in the origin with radius `0`.
   *
   * Allows to construct a circle without using the `new` keyword in Scala.
   *
   * @return The newly instantiated circle.
   */
  def apply(): Circle = new Circle()

  /**
   * Constructs a circle with the given center and the given point.
   * The radius of the circle will be the distance from the center to the given point.
   *
   * Allows to construct a circle without using the `new` keyword in Scala.
   *
   * @param center The center of the circle.
   * @param point A point on the circumference.
   * @return The newly instantiated circle.
   */
  def apply(center: Vec2f, point: Vec2f): Circle = new Circle(center, point)

  /**
   * Constructs a circle with the given center and the given point.
   * The radius of the circle will be the distance from the center to the given point.
   *
   * Allows to construct a circle without using the `new` keyword in Scala.
   *
   * @param x The x coordinate of the center of the circle.
   * @param y The y coordinate of the center of the circle.
   * @param point A point on the circumference.
   * @return The newly instantiated circle.
   */
  def apply(x: Float, y: Float, point: Vec2f): Circle = new Circle(x, y, point)

  /**
   * Constructs a circle with the given center and the given point.
   * The radius of the circle will be the distance from the center to the given point.
   *
   * Allows to construct a circle without using the `new` keyword in Scala.
   *
   * @param center The center of the circle.
   * @param x The x coordinate of a point on the circumference.
   * @param y The y coordinate of a point on the circumference.
   * @return The newly instantiated circle.
   */
  def apply(center: Vec2f, x: Float, y: Float): Circle = new Circle(center, x, y)

  /**
   * Constructs a circle passing through the three given points.
   *
   * Allows to construct a circle without using the `new` keyword in Scala.
   *
   * @param p1 The first point.
   * @param p2 The second point.
   * @param p3 The third point.
   * @return The newly instantiated circle.
   */
  def apply(p1: Vec2f, p2: Vec2f, p3: Vec2f): Circle = new Circle(p1, p2, p3)
}