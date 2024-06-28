
# GeomLib

A Scala library for geometry and primitive shapes.

## Project goals

GeomLib is an extension of [VecMatLib](https://github.com/ScalaMath/VecMatLib) for geometry.

All operations in GeomLib are designed to **not** modify the object on which the operation is invoked to respect the principles of purity and immutability of functional programming.

All classes are written in Scala, but are designed to be completely interoperable with Java.
All methods with symbolic names have an alias for better interoperability with java.

## Geometry and shapes

GeomLib offers a representation for primitive 2D and 3D shapes useful for collision detection algorithms and graphics rendering such as Rectangles, Circles, Spheres and Axis-Aligned Bounding Boxes.

Scala example:

```scala
val rect1 = Rect2(2.0f, 1.0f, 4.0f, 3.0f)
val rect2 = Rect2(3.0f, 2.0f, 5.0f, 4.0f)
if(rect1.intersects(rect2)) {
  // ...
}
```

Java example:

```java
Rect2 rect1 = new Rect2(2.0f, 1.0f, 4.0f, 3.0f);
Rect2 rect2 = new Rect2(3.0f, 2.0f, 5.0f, 4.0f);
if(rect1.intersects(rect2)) {
    // ...
}
```

GeomLib can also be used together with [VecMatLib](https://github.com/ScalaMath/VecMatLib) to represent geometric transformations.

```scala
val transform = Mat3x4f.translation(x, y, z) * Mat4f.rotation(x, y, z) * Mat4f.scaling(x, y, z)
val plane = Plane(0.0f, 1.0f, 0.0f, 2.0f)
val transformedPlane = transform * plane
```

```java
var transform = Mat3x4f.translation(x, y, z)
        .multiply(Mat4f.rotation(x, y, z))
        .multiply(Mat4f.scaling(x, y, z));
var plane = new Plane(0.0f, 1.0f, 0.0f, 2.0f);
var transformedPlane = plane.transform(transform);
```

## Multithreading

Due to GeomLib not using any internal or temporal objects during any computations, neither modifying objects on which operations are called, it can be used safely in a multithreaded application.

## Add GeomLib to your project

### sbt

```
libraryDependencies += "io.github.scalamath" % "geomlib" % "1.0"
```

### Maven

```
<dependency>
    <groupId>io.github.scalamath</groupId>
    <artifactId>geomlib</artifactId>
    <version>1.0</version>
</dependency>
```

### Gradle

```
implementation 'io.github.scalamath:geomlib:1.0'
```

## Questions and answers

**Q**: Why does GeomLib not use scala 3?

**A**: One of the design goals of GeomLib is to be usable both in Scala and Java. Support for Scala 3 in IDEs is still actively being developed, therefore a Scala 3 library may not be suitable to work with.

## Contributing

GeomLib was developed by a single person as an extension of [VecMatLib](https://github.com/ScalaMath/VecMatLib).

Your contributions are always welcome!
Please submit a pull request or open an issue if you want to contribute with bug fixes, code improvements, documentation, and better unit test coverage.

## Support

Support the project with a donation:

* [PayPal](https://paypal.me/hexagonnico)
* [Ko-fi](https://ko-fi.com/HexagonNico)