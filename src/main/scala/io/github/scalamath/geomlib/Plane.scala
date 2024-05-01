package io.github.scalamath.geomlib

import io.github.scalamath.FloatEqualsApprox
import io.github.scalamath.vecmatlib.{Mat3f, Mat3x4f, Mat4f, Vec3f}

/**
 * A plane defined by a normal vector and its distance from the origin.
 *
 * The equation of a plane is `ax + by + cz = d`, where the vector `(a, b, c)` is the normal of the plane and `d` is the distance from the origin to the plane.
 *
 * @constructor Constructs a plane with the given normal and distance from the origin.
 * @param normal The normal of the plane. Not required to be a unit vector. Should not be the zero vector.
 * @param d The distance from the origin to the plane.
 */
case class Plane(normal: Vec3f, d: Float) {

  /**
   * Constructs a plane passing through the origin with the given normal.
   *
   * @param normal The normal of the plane. Not required to be a unit vector. Should not be the zero vector.
   */
  def this(normal: Vec3f) = this(normal, 0.0f)

  /**
   * Constructs a plane with the equation `ax + by + cz = d`.
   *
   * @param a The x component of the plane's normal.
   * @param b The y component of the plane's normal.
   * @param c The z component of the plane's normal.
   * @param d The distance from the origin to the plane.
   */
  def this(a: Float, b: Float, c: Float, d: Float) = this(Vec3f(a, b, c), d)

  /**
   * Constructs a plane passing through the origin with the equation `ax + by + cz = 0`.
   *
   * @param x The x component of the plane's normal.
   * @param y The y component of the plane's normal.
   * @param z The z component of the plane's normal.
   */
  def this(x: Float, y: Float, z: Float) = this(x, y, z, 0.0f)

  /**
   * Constructs a plane passing through the given point with the given normal.
   *
   * @param normal The normal of the plane. Not required to be a unit vector. Should not be the zero vector.
   * @param point A point the plane is passing through.
   */
  def this(normal: Vec3f, point: Vec3f) = this(normal, normal.dot(point))

  /**
   * Constructs a plane passing through the three given points.
   *
   * @param p1 The first point.
   * @param p2 The second point.
   * @param p3 The third point.
   */
  def this(p1: Vec3f, p2: Vec3f, p3: Vec3f) = this((p1 - p2).cross(p1 - p3), p1)

  /**
   * Returns the x component of the plane's normal vector.
   * Usually denoted as `a` in the plane's equation.
   *
   * @return The x component of the plane's normal vector.
   */
  def a: Float = this.normal.x

  /**
   * Returns the y component of the plane's normal vector.
   * Usually denoted as `b` in the plane's equation.
   *
   * @return The y component of the plane's normal vector.
   */
  def b: Float = this.normal.y

  /**
   * Returns the z component of the plane's normal vector.
   * Usually denoted as `c` in the plane's equation.
   *
   * @return The z component of the plane's normal vector.
   */
  def c: Float = this.normal.z

  /**
   * Returns the signed distance from the plane to the point at the given coordinates.
   *
   * If the point is "above" the plane, the distance will be positive.
   * If "below", the distance will be negative.
   *
   * Which side of the plane is above and which is below is determined by the direction of its normal.
   *
   * @param x The x coordinate of the point.
   * @param y The z coordinate of the point.
   * @param z The z coordinate of the point.
   * @return The signed distance from the plane to the point at the given coordinates.
   */
  def distanceTo(x: Float, y: Float, z: Float): Float = (this.normal.dot(x, y, z) + this.d) / this.normal.length

  /**
   * Returns the signed distance from the plane to the given point.
   *
   * If the point is "above" the plane, the distance will be positive.
   * If "below", the distance will be negative.
   *
   * Which side of the plane is above and which is below is determined by the direction of its normal.
   *
   * @param point The point.
   * @return The signed distance from the plane to the given point.
   */
  def distanceTo(point: Vec3f): Float = this.distanceTo(point.x, point.y, point.z)

  /**
   * Flips the direction of this plane's normal and its distance value and returns the result.
   * The resulting plane is congruent to the original one, but faces the opposite direction.
   *
   * This method can be used in place of the unary `-` operator for better interoperability with Java.
   *
   * @return A plane that is congruent to the original one, but faces the opposite direction.
   */
  def flip: Plane = Plane(-this.normal, -this.d)

  /**
   * Flips the direction of this plane's normal and its distance value and returns the result.
   * The resulting plane is congruent to the original one, but faces the opposite direction.
   *
   * This operator is equivalent to the [[flip]] method.
   *
   * @return A plane that is congruent to the original one, but faces the opposite direction.
   */
  def unary_- : Plane = this.flip

  /**
   * Checks if the point at the given coordinates belongs to this plane.
   *
   * @param x The x coordinate of the point.
   * @param y The z coordinate of the point.
   * @param z The z coordinate of the point.
   * @return True if the point at the given coordinates belongs to this plane, otherwise false.
   */
  def containsPoint(x: Float, y: Float, z: Float): Boolean = this.normal.dot(x, y, z) ~= this.d

  /**
   * Checks if the given point belongs to this plane.
   *
   * @param point The point.
   * @return True if the given point belongs to this plane, otherwise false.
   */
  def containsPoint(point: Vec3f): Boolean = this.containsPoint(point.x, point.y, point.z)

  /**
   * Checks if this plane intersects the given line, defined by a point and a direction.
   *
   * @param point Any point the line is passing through.
   * @param direction The direction of the line.
   * @return True if the given line intersects this plane, otherwise false.
   */
  def intersectsLine(point: Vec3f, direction: Vec3f): Boolean = this.containsPoint(point) || !(this.normal.dot(direction) ~= 0.0f)

  /**
   * Returns the point of intersection between this plane and the given line, defined by a point and a direction.
   *
   * If the given line lies on this plane, the result will be the given point.
   *
   * If the given lines does not intersect this plane, the components of the resulting vector will be either `NaN` or `Infinity`.
   * The validity of the result can be checked with [[intersectsLine]].
   *
   * @param point Any point the line is passing through.
   * @param direction The direction of the line.
   * @return The point of intersection between this plane and the given line.
   */
  def lineIntersection(point: Vec3f, direction: Vec3f): Vec3f = {
    if(this.containsPoint(point)) {
      point
    } else {
      point + direction * -((this.normal.dot(point) - this.d) / this.normal.dot(direction))
    }
  }

  /**
   * Checks if this plane is parallel to the given one.
   *
   * Congruent planes are parallel.
   *
   * @param plane The second plane.
   * @return True if this plane is parallel to the given one, otherwise false.
   * @see [[isCongruentTo]]
   */
  def isParallelTo(plane: Plane): Boolean = this.normal.cross(plane.normal).lengthSquared ~= 0.0f

  /**
   * Checks if this plane is congruent to the given one.
   *
   * @param plane The second plane.
   * @return True if this plane is congruent to the given one, otherwise false.
   * @see [[isParallelTo]]
   */
  def isCongruentTo(plane: Plane): Boolean = this.isParallelTo(plane) && ((this.d / this.normal.length).abs ~= (plane.d / plane.normal.length).abs)

  /**
   * Checks if this plane intersects the given one.
   *
   * @param plane The second plane.
   * @return True if this plane intersects the given one, otherwise false.
   */
  def intersects(plane: Plane): Boolean = !this.isParallelTo(plane) || ((this.d / this.normal.length) ~= (plane.d / plane.normal.length))

  /**
   * Checks if this plane and the given ones intersect in a single point.
   *
   * Can be used to check the validity of the result of [[intersection]].
   *
   * @param plane1 The second plane.
   * @param plane2 The third plane.
   * @return True if the three planes intersect in a single point, otherwise false.
   */
  def intersectsInPoint(plane1: Plane, plane2: Plane): Boolean = !(this.normal.cross(plane1.normal).dot(plane2.normal) ~= 0.0f)

  /**
   * Returns the point of intersection between this plane and the given ones.
   *
   * If all the three planes are parallel, all the components of the resulting vector will be `NaN`.
   * If the intersection between the three planes is a line, the components of the resulting vector will be either `NaN` or `Infinity`.
   * The validity of the result can be checked using [[intersectsInPoint]].
   *
   * @param plane1 The second plane.
   * @param plane2 The third plane.
   * @return The point of intersection between the three planes.
   */
  def intersection(plane1: Plane, plane2: Plane): Vec3f = (plane1.normal.cross(plane2.normal) * this.d + plane2.normal.cross(this.normal) * plane1.d + this.normal.cross(plane1.normal) * plane2.d) / this.normal.cross(plane1.normal).dot(plane2.normal)

  def project(point: Vec3f): Vec3f = point - this.normal * this.distanceTo(point)

  def project(x: Float, y: Float, z: Float): Vec3f = this.project(Vec3f(x, y, z))

  /**
   * Checks if the point at the given coordinates is "above" this plane.
   * Which side of the plane is the "above" side is determined by the direction of the plane's normal.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @param z The z coordinate of the point.
   * @return True if the point at the given coordinates is "above" this plane, otherwise false.
   */
  def isPointOver(x: Float, y: Float, z: Float): Boolean = this.normal.dot(x, y, z) > this.d

  /**
   * Checks if the given point is "above" this plane.
   * Which side of the plane is the "above" side is determined by the direction of the plane's normal.
   *
   * @param point The point.
   * @return True if the given point is "above" this plane, otherwise false.
   */
  def isPointOver(point: Vec3f): Boolean = this.normal.dot(point) > this.d

  /**
   * Transforms this plane by the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * @param m A 3x3 transformation matrix.
   * @return The resulting plane.
   */
  def transform(m: Mat3f): Plane = Plane(m.inverse.transposed * this.normal, m * (this.normal * this.d))

  /**
   * Transforms this plane by the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * @param m A 3x4 transformation matrix.
   * @return The resulting plane.
   */
  def transform(m: Mat3x4f): Plane = Plane(Mat3f.fromColumns(m.col0, m.col1, m.col2).inverse.transposed * this.normal, m * (this.normal * this.d, 1.0f))
  // TODO: Add a better way to extract a 3x3 submatrix from a 3x4 matrix

  /**
   * Transforms this plane by the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * @param m A 4x4 transformation matrix.
   * @return The resulting plane.
   */
  def transform(m: Mat4f): Plane = Plane((m.inverse.transposed * (this.normal, 1.0f)).xyz, (m * (this.normal * this.d, 1.0f)).xyz)

  /**
   * Inversely transforms this plane by the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * `plane.inverseTransform(matrix)` is equivalent to `plane.transform(matrix.inverse)`.
   *
   * @param m A 3x3 transformation matrix.
   * @return The resulting plane.
   */
  def inverseTransform(m: Mat3f): Plane = Plane(m.transposed * this.normal, m.inverse * (this.normal * this.d))

  /**
   * Inversely transforms this plane by the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * `plane.inverseTransform(matrix)` is equivalent to `plane.transform(matrix.inverse)`.
   *
   * @param m A 4x4 transformation matrix.
   * @return The resulting plane.
   */
  def inverseTransform(m: Mat4f): Plane = Plane((m.transposed * (this.normal, 1.0f)).xyz, (m.inverse * (this.normal * this.d, 1.0f)).xyz)

  /**
   * Inversely transforms this plane by the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * `plane * matrix` is equivalent to `matrix.inverse * plane`.
   *
   * @param m A 3x3 transformation matrix.
   * @return The resulting plane.
   */
  def *(m: Mat3f): Plane = this.inverseTransform(m)

  /**
   * Inversely transforms this plane by the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * `plane * matrix` is equivalent to `matrix.inverse * plane`.
   *
   * @param m A 4x4 transformation matrix.
   * @return The resulting plane.
   */
  def *(m: Mat4f): Plane = this.inverseTransform(m)

  /**
   * Checks if this plane is approximately equal to the given one using an internal epsilon.
   *
   * This method checks if the normals and the distances `d` are approximately equal, therefore it may still return false for congruent planes with a different equation.
   * Use [[isCongruentTo]] to check if two planes are congruent.
   *
   * @param plane The second plane.
   * @return True if this plane is approximately equal to the given one, otherwise false.
   */
  def ~=(plane: Plane): Boolean = (this.normal ~= plane.normal) && (this.d ~= plane.d)

  /**
   * Checks if this plane is approximately equal to the given one using an internal epsilon.
   *
   * This method can be used in place of the `~=` operator for better interoperability with Java.
   *
   * This method checks if the normals and the distances `d` are approximately equal, therefore it may still return false for congruent planes with a different equation.
   * Use [[isCongruentTo]] to check if two planes are congruent.
   *
   * @param plane The second plane.
   * @return True if this plane is approximately equal to the given one, otherwise false.
   */
  def equalsApprox(plane: Plane): Boolean = this ~= plane

  override def toString: String = f"Plane(${this.a}, ${this.b}, ${this.c}, ${this.d})"
}

/**
 * Factory methods and implicits for planes.
 */
object Plane {

  /**
   * Constructs a plane passing through the origin with the given normal.
   *
   * This method allows to construct a plane without using the `new` keyword in Scala.
   *
   * @param normal The normal of the plane. Not required to be a unit vector. Should not be the zero vector.
   * @return The newly instantiated plane.
   */
  def apply(normal: Vec3f): Plane = new Plane(normal)

  /**
   * Constructs a plane with the equation `ax + by + cz = d`.
   *
   * This method allows to construct a plane without using the `new` keyword in Scala.
   *
   * @param a The x component of the plane's normal.
   * @param b The y component of the plane's normal.
   * @param c The z component of the plane's normal.
   * @param d The distance from the origin to the plane.
   * @return The newly instantiated plane.
   */
  def apply(a: Float, b: Float, c: Float, d: Float): Plane = new Plane(a, b, c, d)

  /**
   * Constructs a plane passing through the origin with the equation `ax + by + cz = 0`.
   *
   * This method allows to construct a plane without using the `new` keyword in Scala.
   *
   * @param x The x component of the plane's normal.
   * @param y The y component of the plane's normal.
   * @param z The z component of the plane's normal.
   * @return The newly instantiated plane.
   */
  def apply(x: Float, y: Float, z: Float): Plane = new Plane(x, y, z)

  /**
   * Constructs a plane passing through the given point with the given normal.
   *
   * This method allows to construct a plane without using the `new` keyword in Scala.
   *
   * @param normal The normal of the plane. Not required to be a unit vector. Should not be the zero vector.
   * @param point A point the plane is passing through.
   * @return The newly instantiated plane.
   */
  def apply(normal: Vec3f, point: Vec3f): Plane = new Plane(normal, point)

  /**
   * Constructs a plane passing through the three given points.
   *
   * This method allows to construct a plane without using the `new` keyword in Scala.
   *
   * @param p1 The first point.
   * @param p2 The second point.
   * @param p3 The third point.
   * @return The newly instantiated plane.
   */
  def apply(p1: Vec3f, p2: Vec3f, p3: Vec3f): Plane = new Plane(p1, p2, p3)

  /**
   * Implicit class that extends [[Mat3f]] to add the `*` operator for planes.
   *
   * @param self This matrix.
   */
  implicit class Transform3x3(val self: Mat3f) extends AnyVal {

    /**
     * Transforms the given plane by the this matrix under the assumption that it is a valid transformation matrix and returns the result.
     *
     * @param plane The plane to transform.
     * @return The resulting plane.
     */
    def *(plane: Plane): Plane = plane.transform(self)
  }

  /**
   * Implicit class that extends [[Mat3x4f]] to add the `*` operator for planes.
   *
   * @param self This matrix.
   */
  implicit class Transform3x4(val self: Mat3x4f) extends AnyVal {

    /**
     * Transforms the given plane by the this matrix under the assumption that it is a valid transformation matrix and returns the result.
     *
     * @param plane The plane to transform.
     * @return The resulting plane.
     */
    def *(plane: Plane): Plane = plane.transform(self)
  }

  /**
   * Implicit class that extends [[Mat4f]] to add the `*` operator for planes.
   *
   * @param self This matrix.
   */
  implicit class Transform4x4(val self: Mat4f) extends AnyVal {

    /**
     * Transforms the given plane by the this matrix under the assumption that it is a valid transformation matrix and returns the result.
     *
     * @param plane The plane to transform.
     * @return The resulting plane.
     */
    def *(plane: Plane): Plane = plane.transform(self)
  }
}
