package io.github.scalamath

import io.github.scalamath.vecmatlib.Vec2f

package object geomlib {

  def lineIntersection(m1: Float, q1: Float, m2: Float, q2: Float): Vec2f = Vec2f((q2 - q1) / (m1 - m2), m1 * (q2 - q1) / (m1 - m2) + q1)

  def lineIntersection(p1: Vec2f, slope1: Float, p2: Vec2f, slope2: Float): Vec2f = this.lineIntersection(slope1, p1.y - slope1 * p1.x, slope2, p2.y - slope2 * p2.x)

  def lineIntersection(p1: Vec2f, dir1: Vec2f, p2: Vec2f, dir2: Vec2f): Vec2f = this.lineIntersection(p1, dir1.aspect, p2, dir2.aspect)
}
