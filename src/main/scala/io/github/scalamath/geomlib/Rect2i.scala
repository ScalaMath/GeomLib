package io.github.scalamath.geomlib

import io.github.scalamath.vecmatlib.Vec2i

case class Rect2i(x: Int, y: Int, width: Int, height: Int) {

  def this(position: Vec2i, size: Vec2i) = this(position.x, position.y, size.x, size.y)

  def this(position: Vec2i, width: Int, height: Int) = this(position.x, position.y, width, height)

  def this(x: Int, y: Int, size: Vec2i) = this(x, y, size.x, size.y)

  def this(width: Int, height: Int) = this(0, 0, width, height)

  def this() = this(0, 0)

  def position: Vec2i = Vec2i(this.x, this.y)

  def size: Vec2i = Vec2i(this.width, this.height)

  def end: Vec2i = Vec2i(this.x + this.width, this.y + this.height)

  def center: Vec2i = Vec2i(this.x + this.width / 2, this.y + this.height / 2)

  def area: Int = this.width * this.height

  def left: Int = math.min(this.x, this.x + this.width)

  def right: Int = math.max(this.x, this.x + this.width)

  def top: Int = math.max(this.y, this.y + this.height)

  def bottom: Int = math.min(this.y, this.y + this.height)

  def bottomLeft: Vec2i = Vec2i(this.left, this.bottom)

  def topLeft: Vec2i = Vec2i(this.left, this.top)

  def bottomRight: Vec2i = Vec2i(this.right, this.bottom)

  def topRight: Vec2i = Vec2i(this.right, this.top)

  def containsPoint(x: Int, y: Int, includeBorders: Boolean): Boolean = {
    if(includeBorders) {
      x >= this.left && x <= this.right && y >= this.bottom && y <= this.top
    } else {
      x > this.left && x < this.right && y > this.bottom && y < this.top
    }
  }

  def containsPoint(point: Vec2i, includeBorders: Boolean): Boolean = this.containsPoint(point.x, point.y, includeBorders)

  def containsPoint(x: Int, y: Int): Boolean = this.containsPoint(x, y, includeBorders = false)

  def containsPoint(point: Vec2i): Boolean = this.containsPoint(point.x, point.y)

  def intersects(rect: Rect2i, includeBorders: Boolean): Boolean = {
    if(includeBorders) {
      !(this.right < rect.left || this.left > rect.right || this.top < rect.bottom || this.bottom > rect.top)
    } else {
      !(this.right <= rect.left || this.left >= rect.right || this.top <= rect.bottom || this.bottom >= rect.top)
    }
  }

  def intersects(rect: Rect2i): Boolean = this.intersects(rect, includeBorders = false)

  def intersection(rect: Rect2i): Rect2i = {
    if(this.intersects(rect, includeBorders = true)) {
      Rect2i.fromPoints(math.max(this.left, rect.left), math.max(this.bottom, rect.bottom), math.min(this.right, rect.right), math.min(this.top, rect.top))
    } else {
      Rect2i()
    }
  }

  def abs: Rect2i = Rect2i(this.left, this.bottom, this.width.abs, this.height.abs)

  def encloses(rect: Rect2i): Boolean = this.left <= rect.left && this.right >= rect.right && this.top >= rect.top && this.bottom <= rect.bottom

  def grow(left: Int, top: Int, right: Int, bottom: Int): Rect2i = Rect2i(this.x - left, this.y - top, this.width + left + right, this.height + top + bottom)

  def grow(amount: Int): Rect2i = Rect2i(this.x - amount, this.y - amount, this.width + amount * 2, this.height + amount * 2)

  def expandTo(x: Int, y: Int): Rect2i = Rect2i.fromPoints(math.min(this.left, x), math.min(this.bottom, y), math.max(this.right, x), math.max(this.top, y))

  def expandTo(point: Vec2i): Rect2i = this.expandTo(point.x, point.y)

  def merge(rect: Rect2i): Rect2i = Rect2i.fromPoints(math.min(this.left, rect.left), math.min(this.bottom, rect.bottom), math.max(this.right, rect.right), math.max(this.top, rect.top))

  def isCongruentTo(rect: Rect2i): Boolean = this.left == rect.left && this.right == rect.right && this.top == rect.top && this.bottom == rect.bottom

  def toFloat: Rect2 = Rect2(this.x.toFloat, this.y.toFloat, this.width.toFloat, this.height.toFloat)
}

object Rect2i {

  def apply(position: Vec2i, size: Vec2i): Rect2i = new Rect2i(position, size)

  def apply(position: Vec2i, width: Int, height: Int): Rect2i = new Rect2i(position, width, height)

  def apply(x: Int, y: Int, size: Vec2i): Rect2i = new Rect2i(x, y, size)

  def apply(width: Int, height: Int): Rect2i = new Rect2i(width, height)

  def apply(): Rect2i = new Rect2i()

  def fromPoints(x1: Int, y1: Int, x2: Int, y2: Int): Rect2i = Rect2i(math.min(x1, x2), math.min(y1, y2), (x1 - x2).abs, (y1 - y2).abs)

  def fromPoints(p1: Vec2i, p2: Vec2i): Rect2i = this.fromPoints(p1.x, p1.y, p2.x, p2.y)

  implicit val conversion: Rect2i => Rect2 = rect => rect.toFloat
}