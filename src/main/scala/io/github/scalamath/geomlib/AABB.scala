package io.github.scalamath.geomlib

import io.github.scalamath.FloatEqualsApprox
import io.github.scalamath.vecmatlib.{Mat3f, Mat3x4f, Mat4f, Vec3f}

/**
 * A 3D axis-aligned bounding box, defined by a [[position]] and a [[size]].
 *
 * Negative values for the size are allowed.
 * The size is considered to be the vector from the origin of the AABB to its [[end]] point.
 *
 * The 2D counterpart is [[Rect2]].
 *
 * @constructor Constructs an AABB at the given x, y, and z coordinates with the given width, height, and depth.
 * @param x The x coordinate of the position of the AABB.
 * @param y The y coordinate of the position of the AABB.
 * @param z The z coordinate of the position of the AABB.
 * @param width The size of the AABB on the x axis.
 * @param height The size of the AABB on the y axis.
 * @param depth The size of the AABB on the z axis.
 */
case class AABB(x: Float, y: Float, z: Float, width: Float, height: Float, depth: Float) {

  /**
   * Constructs an AABB at the given position with the given size.
   *
   * @param position The position of the AABB.
   * @param size The size of the AABB.
   */
  def this(position: Vec3f, size: Vec3f) = this(position.x, position.y, position.z, size.x, size.y, size.z)

  /**
   * Constructs an AABB at the given position with the given width, height, and depth.
   *
   * @param position The position of the AABB.
   * @param width The size of the AABB on the x axis.
   * @param height The size of the AABB on the y axis.
   * @param depth The size of the AABB on the z axis.
   */
  def this(position: Vec3f, width: Float, height: Float, depth: Float) = this(position.x, position.y, position.z, width, height, depth)

  /**
   * Constructs an AABB at the given x, y, and z coordinates with the given size.
   *
   * @param x The x coordinate of the position of the AABB.
   * @param y The y coordinate of the position of the AABB.
   * @param z The z coordinate of the position of the AABB.
   * @param size The size of the AABB.
   */
  def this(x: Float, y: Float, z: Float, size: Vec3f) = this(x, y, z, size.x, size.y, size.z)

  /**
   * Constructs an empty AABB, whose position and size are both `(0, 0, 0)`.
   */
  def this() = this(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f)

  /**
   * Returns the position of this AABB.
   *
   * @return The origin position of this AABB.
   */
  def position: Vec3f = Vec3f(this.x, this.y, this.z)

  /**
   * Returns the size of this AABB.
   * Can be negative.
   *
   * The size of the AABB is the vector from its origin [[position]] to its [[end]].
   *
   * @return The size of this AABB.
   */
  def size: Vec3f = Vec3f(this.width, this.height, this.depth)

  /**
   * Returns the end point of this AABB.
   *
   * Equivalent to [[position]]` + `[[size]].
   *
   * @return The end point of this AABB.
   */
  def end: Vec3f = Vec3f(this.x + this.width, this.y + this.height, this.z + this.depth)

  /**
   * Returns the center point of this AABB.
   *
   * Equivalent to [[position]]` + `[[size]]` / 2`.
   *
   * @return The center point of this AABB.
   */
  def center: Vec3f = Vec3f(this.x + this.width / 2.0f, this.y + this.height / 2.0f, this.z + this.depth / 2.0f)

  /**
   * Returns the volume of this AABB.
   *
   * Equivalent to `width * height * depth`.
   *
   * @return The volume of this AABB.
   */
  def volume: Float = this.width * this.height * this.depth

  /**
   * Returns the x coordinate of the leftmost point of this AABB.
   * This method is guaranteed to return the left extent of the AABB regardless of the sign of its [[size]].
   *
   * @return The x coordinate of the leftmost point of this AABB.
   * @see [[leftPlane]]
   */
  def left: Float = math.min(this.x, this.x + this.width)

  /**
   * Returns the plane passing through the left face of this AABB, with its normal pointing left.
   *
   * @return The plane passing through the left face of this AABB.
   * @see [[left]]
   */
  def leftPlane: Plane = Plane(-1.0f, 0.0f, 0.0f, this.left)

  /**
   * Returns the x coordinate of the rightmost point of this AABB.
   * This method is guaranteed to return the right extent of the AABB regardless of the sign of its [[size]].
   *
   * @return The x coordinate of the rightmost point of this AABB.
   * @see [[rightPlane]]
   */
  def right: Float = math.max(this.x, this.x + this.width)

  /**
   * Returns the plane passing through the right face of this AABB, with its normal pointing right.
   *
   * @return The plane passing through the right face of this AABB.
   * @see [[right]]
   */
  def rightPlane: Plane = Plane(1.0f, 0.0f, 0.0f, this.right)

  /**
   * Returns the y coordinate of the highest point of this AABB.
   * This method is guaranteed to return the top extent of the AABB regardless of the sign of its [[size]].
   *
   * @return The x coordinate of the highest point of this AABB.
   * @see [[topPlane]]
   */
  def top: Float = math.max(this.y, this.y + this.height)

  /**
   * Returns the plane passing through the top face of this AABB, with its normal pointing up.
   *
   * @return The plane passing through the top face of this AABB.
   * @see [[top]]
   */
  def topPlane: Plane = Plane(0.0f, 1.0f, 0.0f, this.top)

  /**
   * Returns the y coordinate of the lowest point of this AABB.
   * This method is guaranteed to return the bottom extent of the AABB regardless of the sign of its [[size]].
   *
   * @return The x coordinate of the lowest point of this AABB.
   * @see [[bottomPlane]]
   */
  def bottom: Float = math.min(this.y, this.y + this.height)

  /**
   * Returns the plane passing through the bottom face of this AABB, with its normal pointing down.
   *
   * @return The plane passing through the bottom face of this AABB.
   * @see [[bottom]]
   */
  def bottomPlane: Plane = Plane(0.0f, -1.0f, 0.0f, this.bottom)

  /**
   * Returns the z coordinate of the foremost point of this AABB.
   * This method is guaranteed to return the front extent of the AABB regardless of the sign of its [[size]].
   *
   * @return The x coordinate of the foremost point of this AABB.
   * @see [[frontPlane]]
   */
  def front: Float = math.min(this.z, this.z + this.depth)

  /**
   * Returns the plane passing through the front face of this AABB, with its normal pointing forward.
   *
   * @return The plane passing through the front face of this AABB.
   * @see [[front]]
   */
  def frontPlane: Plane = Plane(0.0f, 0.0f, 1.0f, this.front)

  /**
   * Returns the z coordinate of the hindmost point of this AABB.
   * This method is guaranteed to return the back extent of the AABB regardless of the sign of its [[size]].
   *
   * @return The x coordinate of the hindmost point of this AABB.
   * @see [[backPlane]]
   */
  def back: Float = math.max(this.z, this.z + this.depth)

  /**
   * Returns the plane passing through the back face of this AABB, with its normal pointing backwards.
   *
   * @return The plane passing through the back face of this AABB.
   * @see [[back]]
   */
  def backPlane: Plane = Plane(0.0f, 0.0f, -1.0f, this.back)

  /**
   * Checks if this AABB contains the point at the given coordinates.
   * If `includeFaces` is true, points laying on the faces of the AABB will be considered inside the AABB.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @param z The z coordinate of the point.
   * @param includeFaces If true, points laying on the faces of the AABB will be considered inside the AABB.
   * @return True if the point at the given coordinates is inside this AABB, otherwise false.
   */
  def containsPoint(x: Float, y: Float, z: Float, includeFaces: Boolean): Boolean = {
    if(includeFaces) {
      x >= this.left && x <= this.right && y >= this.bottom && y <= this.top && z >= this.front && z <= this.back
    } else {
      x > this.left && x < this.right && y > this.bottom && y < this.top && z > this.front && z < this.back
    }
  }

  /**
   * Checks if this AABB contains the given point.
   * If `includeFaces` is true, points laying on the faces of the AABB will be considered inside the AABB.
   *
   * @param point The point.
   * @param includeFaces If true, points laying on the faces of the AABB will be considered inside the AABB.
   * @return True if the given point is inside this AABB, otherwise false.
   */
  def containsPoint(point: Vec3f, includeFaces: Boolean): Boolean = this.containsPoint(point.x, point.y, point.z, includeFaces)

  /**
   * Checks if this AABB contains the point at the given coordinates.
   * Points laying on the faces of the AABB are not considered to be inside the AABB.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @param z The z coordinate of the point.
   * @return True if the point at the given coordinates is inside this AABB, otherwise false.
   */
  def containsPoint(x: Float, y: Float, z: Float): Boolean = this.containsPoint(x, y, z, includeFaces = false)

  /**
   * Checks if this AABB contains the given point.
   * Points laying on the faces of the AABB are not considered to be inside the AABB.
   *
   * @param point The point.
   * @return True if the given point is inside this AABB, otherwise false.
   */
  def containsPoint(point: Vec3f): Boolean = this.containsPoint(point, includeFaces = false)

  /**
   * Checks if this AABB overlaps with the given one.
   * If `includeFaces` is true, AABBs touching each other by a face are considered as overlapping.
   *
   * @param aabb The other AABB.
   * @param includeFaces If true, AABBs touching each other by a face are considered as overlapping.
   * @return True if this AABB overlaps with the given one, otherwise false.
   */
  def intersects(aabb: AABB, includeFaces: Boolean): Boolean = {
    if(includeFaces) {
      !(this.right < aabb.left || this.left > aabb.right || this.top < aabb.bottom || this.bottom > aabb.top || this.front > aabb.back || this.back < aabb.front)
    } else {
      !(this.right <= aabb.left || this.left >= aabb.right || this.top <= aabb.bottom || this.bottom >= aabb.top || this.front >= aabb.back || this.back <= aabb.front)
    }
  }

  /**
   * Checks if this AABB overlaps with the given one.
   * The faces of both AABBs are excluded.
   *
   * @param aabb The other AABB.
   * @return True if this AABB overlaps with the given one, otherwise false.
   */
  def intersects(aabb: AABB): Boolean = this.intersects(aabb, includeFaces = false)

  /**
   * Returns the intersection between this AABB and the given one or an empty AABB if the two do not intersect.
   * This method always returns an AABB with positive [[size]].
   *
   * @param aabb The other AABB.
   * @return The intersection between this AABB and the given one.
   */
  def intersection(aabb: AABB): AABB = {
    if(this.intersects(aabb, includeFaces = true)) {
      AABB.fromPoints(
        math.max(this.left, aabb.left), math.max(this.bottom, aabb.bottom), math.max(this.front, aabb.front),
        math.min(this.right, aabb.right), math.min(this.top, aabb.top), math.min(this.back, aabb.back)
      )
    } else {
      AABB()
    }
  }

  /**
   * Returns a [[List]] containing the 8 vertices of this AABB in no particular order.
   *
   * @return A list containing the 8 vertices of this AABB.
   */
  def vertices: List[Vec3f] = List(
    Vec3f(this.x, this.y, this.z),
    Vec3f(this.x, this.y, this.z + depth),
    Vec3f(this.x, this.y + height, this.z),
    Vec3f(this.x, this.y + height, this.z + depth),
    Vec3f(this.x + width, this.y, this.z),
    Vec3f(this.x + width, this.y, this.z + depth),
    Vec3f(this.x + width, this.y + height, this.z),
    Vec3f(this.x + width, this.y + height, this.z + depth)
  )

  /**
   * Checks if the given plane intersects this AABB.
   *
   * @param plane The plane.
   * @return True if the given plane intersects this AABB, otherwise false.
   */
  def intersects(plane: Plane): Boolean = {
    var over = false
    var under = false
    this.vertices.foreach(vertex => {
      if(plane.isPointOver(vertex)) {
        over = true
      } else {
        under = true
      }
    })
    under && over
  }

  /**
   * Checks if the given line, defined by a point and a direction, intersects this AABB.
   *
   * @param point A point on the line.
   * @param direction The direction of the line.
   * @return True if the given line intersects this AABB, otherwise false.
   * @see [[lineIntersection]]
   */
  def intersectsLine(point: Vec3f, direction: Vec3f): Boolean = {
    val t1 = (this.position - point) / direction
    val t2 = (this.end - point) / direction
    math.max(
      math.min(t1.x, t2.x),
      math.max(
        math.min(t1.y, t2.y),
        math.min(t1.z, t2.z)
      )
    ) <= math.min(
      math.max(t1.x, t2.x),
      math.min(
        math.max(t1.y, t2.y),
        math.max(t1.z, t2.z)
      )
    )
  }

  /**
   * Returns the point of intersection between the given line and this AABB.
   *
   * If the line intersects the AABB in two points, the resulting point will be the first one in the direction of the line.
   *
   * If the given line does not intersect this AABB, all the components resulting vector will be `NaN`.
   * The validity of the result can be checked with the [[intersectsLine]] method.
   *
   * @param point A point on the line.
   * @param direction The direction of the line.
   * @return The point of intersection between the given line and this AABB.
   */
  def lineIntersection(point: Vec3f, direction: Vec3f): Vec3f = {
    val t0 = (this.position - point) / direction
    val t1 = (this.end - point) / direction
    val d = math.max(math.min(t0.x, t1.x), math.max(math.min(t0.y, t1.y), math.min(t0.z, t1.z)))
    if(d <= math.min(math.max(t0.x, t1.x), math.min(math.max(t0.y, t1.y), math.max(t0.z, t1.z)))) {
      point + direction * d
    } else {
      Vec3f(Float.NaN, Float.NaN, Float.NaN)
    }
  }

  /**
   * Returns an AABB equivalent to this one with non-negative [[size]].
   *
   * @return An AABB equivalent to this one with non-negative size.
   */
  def abs: AABB = AABB(this.left, this.bottom, this.front, this.width.abs, this.height.abs, this.depth.abs)

  /**
   * Checks if this AABB completely encloses the given one.
   *
   * @param aabb The other AABB.
   * @return True if this AABB completely encloses the given one, otherwise false.
   * @see [[merge]]
   */
  def encloses(aabb: AABB): Boolean = this.left <= aabb.left && this.right >= aabb.right && this.top >= aabb.top && this.bottom <= aabb.bottom && this.front <= aabb.front && this.back >= aabb.back

  /**
   * Extends all the sides of this AABB by the given amount and returns the result.
   * A negative amount shrinks the AABB.
   *
   * If this size of this rectangle is negative on one of the axes, a positive amount will shrink it and a negative amount will grow it.
   *
   * @param amount Amount by which the AABB should grow.
   * @return A copy of this AABB with all sides extended by the given amount.
   */
  def grow(amount: Float): AABB = AABB(this.x - amount, this.y - amount, this.z - amount, this.width + amount * 2.0f, this.height + amount * 2.0f, this.depth + amount * 2.0f)

  /**
   * Returns a copy of this AABB expanded to align its edges with the given point.
   *
   * The resulting AABB is guaranteed to have a positive size.
   *
   * @param x The x coordinate of the point.
   * @param y The y coordinate of the point.
   * @param z The z coordinate of the point.
   * @return A copy of this AABB expanded to align its edges with the given point.
   */
  def expandTo(x: Float, y: Float, z: Float): AABB = AABB.fromPoints(
    math.min(this.left, x), math.min(this.bottom, y), math.min(this.front, z),
    math.max(this.right, x), math.max(this.top, y), math.max(this.back, z)
  )

  /**
   * Returns a copy of this AABB expanded to align its edges with the given point.
   *
   * The resulting AABB is guaranteed to have a positive size.
   *
   * @param point The point.
   * @return A copy of this AABB expanded to align its edges with the given point.
   */
  def expandTo(point: Vec3f): AABB = this.expandTo(point.x, point.y, point.z)

  /**
   * Returns an AABB that encloses both this one and the given one.
   *
   * @param aabb The other AABB.
   * @return An AABB that encloses both this one and the given one.
   * @see [[encloses]]
   */
  def merge(aabb: AABB): AABB = AABB.fromPoints(
    math.min(this.left, aabb.left), math.min(this.bottom, aabb.bottom), math.min(this.front, aabb.front),
    math.max(this.right, aabb.right), math.max(this.top, aabb.top), math.max(this.back, aabb.back)
  )

  /**
   * Returns a sphere that completely encloses this AABB.
   * The center of the sphere corresponds to the [[center]] of the AABB, its radius corresponds to the distance from the center to the corners of the AABB.
   *
   * @return A sphere that completely encloses this AABB.
   */
  def boundingSphere: Sphere = Sphere(this.center, this.x, this.y, this.z)

  /**
   * Transforms this AABB by multiplying it with the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * @param m A 3x3 transformation matrix.
   * @return The resulting AABB.
   */
  def transform(m: Mat3f): AABB = {
    val x = m.col0 * this.width
    val y = m.col1 * this.height
    val z = m.col2 * this.depth
    val pos = m * this.position
    AABB.fromPoints(pos, pos + x + y + z)
  }

  /**
   * Transforms this AABB by multiplying it with the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * @param m A 3x4 transformation matrix.
   * @return The resulting AABB.
   */
  def transform(m: Mat3x4f): AABB = {
    val x = m.col0 * this.width
    val y = m.col1 * this.height
    val z = m.col2 * this.depth
    val pos = m * (this.position, 1.0f)
    AABB.fromPoints(pos, pos + x + y + z)
  }

  /**
   * Transforms this AABB by multiplying it with the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * @param m A 4x4 transformation matrix.
   * @return The resulting AABB.
   */
  def transform(m: Mat4f): AABB = {
    val x = m.col0.xyz * this.width
    val y = m.col1.xyz * this.height
    val z = m.col2.xyz * this.depth
    val pos = (m * (this.position, 1.0f)).xyz
    AABB.fromPoints(pos, pos + x + y + z)
  }

  /**
   * Inversely transforms this AABB by multiplying it with the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * `aabb.inverseTransform(matrix)` is equivalent to `aabb.transform(matrix.inverse)`.
   *
   * @param m A 3x3 transformation matrix.
   * @return The resulting AABB.
   */
  def inverseTransform(m: Mat3f): AABB = this.transform(m.inverse)

  /**
   * Inversely transforms this AABB by multiplying it with the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * This method can be used in place of the `*` operator for better interoperability with Java.
   *
   * `aabb.inverseTransform(matrix)` is equivalent to `aabb.transform(matrix.inverse)`.
   *
   * @param m A 4x4 transformation matrix.
   * @return The resulting AABB.
   */
  def inverseTransform(m: Mat4f): AABB = this.transform(m.inverse)

  /**
   * Inversely transforms this AABB by multiplying it with the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * `aabb * matrix` is equivalent to `matrix.inverse * aabb`.
   *
   * @param m A 3x3 transformation matrix.
   * @return The resulting AABB.
   */
  def *(m: Mat3f): AABB = this.inverseTransform(m)

  /**
   * Inversely transforms this AABB by multiplying it with the given matrix under the assumption that it is a valid transformation matrix and returns the result.
   *
   * `aabb * matrix` is equivalent to `matrix.inverse * aabb`.
   *
   * @param m A 4x4 transformation matrix.
   * @return The resulting AABB.
   */
  def *(m: Mat4f): AABB = this.inverseTransform(m)

  def support(normal: Vec3f): Vec3f = {
    val halfExtents = this.size * 0.5f
    halfExtents * normal.sign + this.position + halfExtents
  }

  /**
   * Checks if this AABB is congruent to the given one.
   *
   * Unlike [[equalsApprox]], this method returns true for AABBs with different origins and sizes if they represent the same AABB.
   *
   * @param aabb The other AABB.
   * @return True if this AABB is congruent to the given one, otherwise false.
   */
  def isCongruentTo(aabb: AABB): Boolean = (this.left ~= aabb.left) && (this.right ~= aabb.right) && (this.top ~= aabb.top) && (this.bottom ~= aabb.bottom) && (this.front ~= aabb.front) && (this.back ~= aabb.back)

  /**
   * Checks if this AABB is approximately equal to the given one by checking if positions and sizes are approximately equal using an internal epsilon.
   *
   * Unlike [[isCongruentTo]], this operator returns false if the given AABB has a different origin or size even if it represents an AABB equal to this one.
   *
   * @param aabb The other AABB.
   * @return True if this AABB is approximately equal to the given one, otherwise false.
   */
  def ~=(aabb: AABB): Boolean = (this.x ~= aabb.x) && (this.y ~= aabb.y) && (this.z ~= aabb.z) && (this.width ~= aabb.width) && (this.height ~= aabb.height) && (this.depth ~= aabb.depth)

  /**
   * Checks if this AABB is approximately equal to the given one by checking if positions and sizes are approximately equal using an internal epsilon.
   *
   * This method can be used in place of the `~=` operator for better interoperability with Java.
   *
   * Unlike [[isCongruentTo]], this method returns false if the given AABB has a different origin or size even if it represents an AABB equal to this one.
   *
   * @param aabb The other AABB.
   * @return True if this AABB is approximately equal to the given one, otherwise false.
   */
  def equalsApprox(aabb: AABB): Boolean = this ~= aabb
}

/**
 * Factory methods and implicits for AABBs.
 */
object AABB {

  /**
   * Constructs an AABB at the given position with the given size.
   *
   * Allows to construct an AABB without using the `new` keyword in Scala.
   *
   * @param position The position of the AABB.
   * @param size The size of the AABB.
   * @return The newly instantiated AABB.
   */
  def apply(position: Vec3f, size: Vec3f): AABB = new AABB(position, size)

  /**
   * Constructs an AABB at the given position with the given width, height, and depth.
   *
   * Allows to construct an AABB without using the `new` keyword in Scala.
   *
   * @param position The position of the AABB.
   * @param width The size of the AABB on the x axis.
   * @param height The size of the AABB on the y axis.
   * @param depth The size of the AABB on the z axis.
   * @return The newly instantiated AABB.
   */
  def apply(position: Vec3f, width: Float, height: Float, depth: Float): AABB = new AABB(position, width, height, depth)

  /**
   * Constructs an AABB at the given x, y, and z coordinates with the given size.
   *
   * Allows to construct an AABB without using the `new` keyword in Scala.
   *
   * @param x The x coordinate of the position of the AABB.
   * @param y The y coordinate of the position of the AABB.
   * @param z The z coordinate of the position of the AABB.
   * @param size The size of the AABB.
   * @return The newly instantiated AABB.
   */
  def apply(x: Float, y: Float, z: Float, size: Vec3f): AABB = new AABB(x, y, z, size)

  /**
   * Constructs an empty AABB, whose position and size are both `(0, 0, 0)`.
   *
   * Allows to construct an AABB without using the `new` keyword in Scala.
   *
   * @return The newly instantiated AABB.
   */
  def apply(): AABB = new AABB()

  /**
   * Constructs an AABB using the two given points as the bounds of the AABB.
   * The given points will be two of the corners of the resulting AABB.
   *
   * The resulting AABB is guaranteed to have a positive size.
   *
   * @param x1 The x coordinate of the first point.
   * @param y1 The y coordinate of the first point.
   * @param z1 The z coordinate of the first point.
   * @param x2 The x coordinate of the second point.
   * @param y2 The y coordinate of the second point.
   * @param z2 The z coordinate of the second point.
   * @return The newly instantiated AABB.
   */
  def fromPoints(x1: Float, y1: Float, z1: Float, x2: Float, y2: Float, z2: Float): AABB = AABB(
    math.min(x1, x2), math.min(y1, y2), math.min(z1, z2),
    (x1 - x2).abs, (y1 - y2).abs, (z1 - z2).abs
  )

  /**
   * Constructs an AABB using the two given points as the bounds of the AABB.
   * The given points will be two of the corners of the resulting AABB.
   *
   * The resulting AABB is guaranteed to have a positive size.
   *
   * @param p1 The first point.
   * @param p2 The second point.
   * @return The newly instantiated AABB.
   */
  def fromPoints(p1: Vec3f, p2: Vec3f): AABB = this.fromPoints(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z)

  /**
   * Allows to use the `*` operator between a [[Mat3f]] and an [[AABB]].
   *
   * @param self The matrix.
   */
  implicit class Transform3x3(val self: Mat3f) extends AnyVal {

    /**
     * Transforms the given AABB by multiplying it with this matrix under the assumption that it is a valid transformation matrix and returns the result.
     *
     * @param aabb The AABB.
     * @return The resulting AABB.
     */
    def *(aabb: AABB): AABB = aabb.transform(self)
  }

  /**
   * Allows to use the `*` operator between a [[Mat3x4f]] and an [[AABB]].
   *
   * @param self The matrix.
   */
  implicit class Transform3x4(val self: Mat3x4f) extends AnyVal {

    /**
     * Transforms the given AABB by multiplying it with this matrix under the assumption that it is a valid transformation matrix and returns the result.
     *
     * @param aabb The AABB.
     * @return The resulting AABB.
     */
    def *(aabb: AABB): AABB = aabb.transform(self)
  }

  /**
   * Allows to use the `*` operator between a [[Mat4f]] and an [[AABB]].
   *
   * @param self The matrix.
   */
  implicit class Transform4x4(val self: Mat4f) extends AnyVal {

    /**
     * Transforms the given AABB by multiplying it with this matrix under the assumption that it is a valid transformation matrix and returns the result.
     *
     * @param aabb The AABB.
     * @return The resulting AABB.
     */
    def *(aabb: AABB): AABB = aabb.transform(self)
  }
}