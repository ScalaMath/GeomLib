package io.github.scalamath.geomlib

import io.github.scalamath.FloatEqualsApprox
import io.github.scalamath.vecmatlib.Vec3f

/**
 * A 3D sphere, defined by a [[center]] and a radius.
 *
 * The radius of a sphere is expected to be positive.
 * Many methods in this class will return incorrect results if the radius is negative.
 *
 * The 2D equivalent is a [[Circle]].
 *
 * @constructor Constructs a sphere with the given center and radius.
 * @param x The x coordinate of the center of the sphere.
 * @param y The y coordinate of the center of the sphere.
 * @param z The z coordinate of the center of the sphere.
 * @param radius The radius of the sphere. Should be positive.
 */
case class Sphere(x: Float, y: Float, z: Float, radius: Float) {

  /**
   * Constructs a sphere with the given center and radius.
   *
   * @param center The center of the sphere.
   * @param radius The radius of the sphere. Should be positive.
   */
  def this(center: Vec3f, radius: Float) = this(center.x, center.y, center.z, radius)

  /**
   * Constructs a sphere centered in the origin with the given radius.
   *
   * @param radius The radius of the sphere. Should be positive.
   */
  def this(radius: Float) = this(0.0f, 0.0f, 0.0f, radius)

  /**
   * Constructs an empty sphere centered in the origin with radius `0`.
   */
  def this() = this(0.0f)

  /**
   * Constructs a sphere with the given center and the given point.
   * The radius of the sphere will be the distance from the center to the given point.
   *
   * @param center The center of the sphere.
   * @param point A point on the surface of the sphere.
   */
  def this(center: Vec3f, point: Vec3f) = this(center, center.distanceTo(point))

  /**
   * Constructs a sphere with the given center and the given point.
   * The radius of the sphere will be the distance from the center to the given point.
   *
   * @param x The x coordinate of the center of the sphere.
   * @param y The y coordinate of the center of the sphere.
   * @param z The z coordinate of the center of the sphere.
   * @param point A point on the surface of the sphere.
   */
  def this(x: Float, y: Float, z: Float, point: Vec3f) = this(x, y, z, point.distanceTo(x, y, z))

  /**
   * Constructs a sphere with the given center and the given point.
   * The radius of the sphere will be the distance from the center to the given point.
   *
   * @param center The center of the sphere.
   * @param x The x coordinate of a point on the surface of the sphere.
   * @param y The y coordinate of a point on the surface of the sphere.
   * @param z The z coordinate of a point on the surface of the sphere
   */
  def this(center: Vec3f, x: Float, y: Float, z: Float) = this(center, center.distanceTo(x, y, z))

  /**
   * Returns the center of the sphere.
   *
   * @return The center of the sphere.
   */
  def center: Vec3f = Vec3f(this.x, this.y, this.z)

  /**
   * Returns the diameter of the sphere.
   * Equivalent to `radius * 2`.
   *
   * @return The diameter of the sphere.
   */
  def diameter: Float = this.radius * 2.0f

  /**
   * Returns the squared radius of the sphere.
   * Equivalent to `radius * radius`.
   *
   * @return The squared radius of the sphere.
   */
  def radiusSquared: Float = this.radius * this.radius

  /**
   * Returns the surface area of the sphere.
   *
   * @return The surface area of the sphere.
   */
  def surface: Float = (4.0 * math.Pi * this.radiusSquared).toFloat

  /**
   * Returns the cubed radius of the sphere.
   * Equivalent to `radius * radius * radius`.
   *
   * @return The cubed radius of the sphere.
   */
  def radiusCubed: Float = this.radiusSquared * this.radius

  /**
   * Returns the volume of the sphere.
   *
   * @return The volume of the sphere.
   */
  def volume: Float = (4.0 / 3.0 * math.Pi * this.radiusCubed).toFloat

  /**
   * Checks if this sphere contains the point at the given coordinates.
   * If `includeSurface` is true, points laying on the surface of the sphere will be considered to be inside the sphere.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @param z The z coordinate of the point.
   * @param includeSurface If true, points laying on the surface of the sphere will be considered to be inside the sphere.
   * @return True if the point at the given coordinates is inside this sphere, otherwise false.
   */
  def containsPoint(x: Float, y: Float, z: Float, includeSurface: Boolean): Boolean = {
    if(includeSurface) {
      this.center.distanceSquaredTo(x, y, z) <= this.radiusSquared
    } else {
      this.center.distanceSquaredTo(x, y, z) < this.radiusSquared
    }
  }

  /**
   * Checks if this sphere contains the point at the given coordinates.
   * Points laying on the surface of the sphere are not considered to be inside the sphere.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @param z The z coordinate of the point.
   * @return True if the point at the given coordinates is inside this sphere, otherwise false.
   */
  def containsPoint(x: Float, y: Float, z: Float): Boolean = this.containsPoint(x, y, z, includeSurface = false)

  /**
   * Checks if this sphere contains the given point.
   * If `includeSurface` is true, points laying on the surface of the sphere will be considered to be inside the sphere.
   *
   * @param point The point.
   * @param includeSurface If true, points laying on the surface of the sphere will be considered to be inside the sphere.
   * @return True if the given point is inside this sphere, otherwise false.
   */
  def containsPoint(point: Vec3f, includeSurface: Boolean): Boolean = {
    if(includeSurface) {
      this.center.distanceSquaredTo(point) <= this.radiusSquared
    } else {
      this.center.distanceSquaredTo(point) < this.radiusSquared
    }
  }

  /**
   * Checks if this sphere contains the given point.
   * Points laying on the surface of the sphere are not considered to be inside the sphere.
   *
   * @param point The point.
   * @return True if the given point is inside this sphere, otherwise false.
   */
  def containsPoint(point: Vec3f): Boolean = this.containsPoint(point, includeSurface = false)

  /**
   * Checks if the point at the given coordinates is on the surface of the sphere by checking if its distance from the center is approximately equal to the radius.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @param z The z coordinate of the point.
   * @return True if the point at the given coordinates is on the surface of the sphere, otherwise false.
   */
  def isPointOnSurface(x: Float, y: Float, z: Float): Boolean = this.center.distanceSquaredTo(x, y, z) ~= this.radiusSquared

  /**
   * Checks if the given point is on the surface of the sphere by checking if its distance from the center is approximately equal to the radius.
   *
   * @param point The point.
   * @return True if the given point is on the surface of the sphere, otherwise false.
   */
  def isPointOnSurface(point: Vec3f): Boolean = this.center.distanceSquaredTo(point) ~= this.radiusSquared

  /**
   * Checks if this sphere intersects the given one.
   * If `includeSurface` is true, spheres touching each other by a single point will be considered to be intersecting.
   *
   * @param sphere The other sphere.
   * @param includeSurface If true, spheres touching each other by a single point will be considered to be intersecting.
   * @return True if this sphere intersects the given one, otherwise false.
   */
  def intersects(sphere: Sphere, includeSurface: Boolean): Boolean = {
    if(includeSurface) {
      this.center.distanceTo(sphere.x, sphere.y, sphere.z) <= this.radius + sphere.radius
    } else {
      this.center.distanceTo(sphere.x, sphere.y, sphere.z) < this.radius + sphere.radius
    }
  }

  /**
   * Checks if this sphere intersects the given one.
   * Spheres touching each other by a single point are not considered to be intersecting.
   *
   * @param sphere The other sphere.
   * @return True if this sphere intersects the given one, otherwise false.
   */
  def intersects(sphere: Sphere): Boolean = this.intersects(sphere, includeSurface = false)

  /**
   * Checks if this sphere completely encloses the given one.
   *
   * @param sphere The other sphere.
   * @return True if this sphere completely encloses the given one, otherwise false.
   */
  def encloses(sphere: Sphere): Boolean = this.containsPoint(sphere.x, sphere.y, sphere.z) && this.radius - this.center.distanceTo(sphere.center) >= sphere.radius

  /**
   * Returns a sphere that encloses both this one and the given one.
   *
   * @param sphere The other sphere.
   * @return A sphere that encloses both this one and the given one.
   */
  def merge(sphere: Sphere): Sphere = Sphere((this.x + sphere.x) / 2.0f, (this.y + sphere.y) / 2.0f, (this.z + sphere.z) / 2.0f, (this.center.distanceTo(sphere.center) + this.radius + sphere.radius) / 2.0f)

  /**
   * Returns an axis-aligned bounding box that completely encloses this sphere.
   *
   * @return The bounding box of this sphere.
   */
  def boundingBox: AABB = AABB(this.x - this.radius, this.y - this.radius, this.z - this.radius, this.diameter, this.diameter, this.diameter)

  /**
   * Checks if this sphere is approximately equal to the given one by checking if potions and radii are approximately equal using an internal epsilon.
   *
   * @param sphere The other sphere.
   * @return True if this sphere is approximately equal to the given one, otherwise false.
   */
  def ~=(sphere: Sphere): Boolean = (this.x ~= sphere.x) && (this.y ~= sphere.y) && (this.z ~= sphere.z) && (this.radius ~= sphere.radius)

  /**
   * Checks if this sphere is approximately equal to the given one by checking if potions and radii are approximately equal using an internal epsilon.
   *
   * This method can be used in place of the `~=` operator for better interoperability with Java.
   *
   * @param sphere The other sphere.
   * @return True if this sphere is approximately equal to the given one, otherwise false.
   */
  def equalsApprox(sphere: Sphere): Boolean = this ~= sphere
}

/**
 * Factory methods for spheres.
 */
object Sphere {

  /**
   * Constructs a sphere with the given center and radius.
   *
   * Allows to construct a sphere without using the `new` keyword in Scala.
   *
   * @param center The center of the sphere.
   * @param radius The radius of the sphere. Should be positive.
   * @return The newly instantiated sphere.
   */
  def apply(center: Vec3f, radius: Float): Sphere = new Sphere(center, radius)

  /**
   * Constructs a sphere centered in the origin with the given radius.
   *
   * Allows to construct a sphere without using the `new` keyword in Scala.
   *
   * @param radius The radius of the sphere. Should be positive.
   * @return The newly instantiated sphere.
   */
  def apply(radius: Float): Sphere = new Sphere(radius)

  /**
   * Constructs an empty sphere centered in the origin with radius `0`.
   *
   * Allows to construct a sphere without using the `new` keyword in Scala.
   *
   * @return The newly instantiated sphere.
   */
  def apply(): Sphere = new Sphere()

  /**
   * Constructs a sphere with the given center and the given point.
   * The radius of the sphere will be the distance from the center to the given point.
   *
   * Allows to construct a sphere without using the `new` keyword in Scala.
   *
   * @param center The center of the sphere.
   * @param point A point on the surface of the sphere.
   * @return The newly instantiated sphere.
   */
  def apply(center: Vec3f, point: Vec3f): Sphere = new Sphere(center, point)

  /**
   * Constructs a sphere with the given center and the given point.
   * The radius of the sphere will be the distance from the center to the given point.
   *
   * Allows to construct a sphere without using the `new` keyword in Scala.
   *
   * @param x The x coordinate of the center of the sphere.
   * @param y The y coordinate of the center of the sphere.
   * @param z The z coordinate of the center of the sphere.
   * @param point A point on the surface of the sphere.
   * @return The newly instantiated sphere.
   */
  def apply(x: Float, y: Float, z: Float, point: Vec3f): Sphere = new Sphere(x, y, z, point)

  /**
   * Constructs a sphere with the given center and the given point.
   * The radius of the sphere will be the distance from the center to the given point.
   *
   * Allows to construct a sphere without using the `new` keyword in Scala.
   *
   * @param center The center of the sphere.
   * @param x The x coordinate of a point on the surface of the sphere.
   * @param y The y coordinate of a point on the surface of the sphere.
   * @param y The z coordinate of a point on the surface of the sphere.
   * @return The newly instantiated sphere.
   */
  def apply(center: Vec3f, x: Float, y: Float, z: Float): Sphere = new Sphere(center, x, y, z)
}