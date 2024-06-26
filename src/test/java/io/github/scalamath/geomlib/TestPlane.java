package io.github.scalamath.geomlib;

import io.github.scalamath.vecmatlib.Mat3f;
import io.github.scalamath.vecmatlib.Mat4f;
import io.github.scalamath.vecmatlib.Mat3x4f;
import org.junit.Assert;
import org.junit.Test;

public class TestPlane {
    
    @Test
    public void testFlipPlane() {
        var plane = new Plane(1.0f, 0.0f, 1.0f, -2.0f);
        var res = new Plane(-1.0f, 0.0f, -1.0f, 2.0f);
        Assert.assertEquals(res, plane.flip());
    }
    
    @Test
    public void testTransform3x3() {
        var transform = Mat3f.rotation(Math.PI / 2.0, 0.0, Math.PI / 4.0);
        var plane = new Plane(1.0f, 1.0f, 0.0f, 1.0f);
        var res = new Plane(0.0f, 0.0f, 1.0f, (float) Math.sqrt(2.0));
        Assert.assertTrue(plane.transform(transform).isCongruentTo(res));
    }
    
    @Test
    public void testTransform3x4() {
        var transform = Mat3x4f.translation(0.0f, 0.0f, 1.0f)
            .multiply(Mat4f.rotation(Math.PI / 2.0, 0.0, Math.PI / 4.0));
        var plane = new Plane(1.0f, 1.0f, 0.0f, 1.0f);
        var res = new Plane(0.0f, 0.0f, 1.0f, (float) (Math.sqrt(2.0)) + 1.0f);
        Assert.assertTrue(plane.transform(transform).isCongruentTo(res));
    }

    @Test
    public void testTransform4x4() {
        var transform = Mat4f.translation(0.0f, 0.0f, 1.0f)
            .multiply(Mat4f.rotation(Math.PI / 2.0, 0.0, Math.PI / 4.0));
        var plane = new Plane(1.0f, 1.0f, 0.0f, 1.0f);
        var res = new Plane(0.0f, 0.0f, 1.0f, (float) Math.sqrt(2.0) + 1.0f);
        Assert.assertTrue(plane.transform(transform).isCongruentTo(res));
    }

    @Test
    public void testInverseTransform3x3() {
        var transform = Mat3f.rotation(Math.PI / 2.0, 0.0, Math.PI / 4.0);
        var plane = new Plane(1.0f, 1.0f, 0.0f, 1.0f);
        var res = new Plane(0.5f, -0.5f, (float) -Math.sqrt(2.0) / 2.0f, (float) Math.sqrt(2.0));
        Assert.assertTrue(plane.inverseTransform(transform).isCongruentTo(res));
    }

    @Test
    public void testInverseTransform4x4() {
        var transform = Mat4f.translation(0.0f, 1.0f, 0.0f)
            .multiply(Mat4f.rotation(Math.PI / 2.0, 0.0, Math.PI / 4.0));
        var plane = new Plane(1.0f, 1.0f, 0.0f, 1.0f);
        var res = new Plane(0.5f, -0.5f, (float) -Math.sqrt(2.0) / 2.0f, (float) Math.sqrt(2.0) / 2.0f);
        Assert.assertTrue(plane.inverseTransform(transform).isCongruentTo(res));
    }

    @Test
    public void testEqualsApprox() {
        var plane1 = new Plane(1.0f, 2.0f, 3.0f, 4.0f);
        var plane2 = new Plane(0.9999999f, 1.9999999f, 3.0000001f, 4.0000001f);
        var plane3 = plane1.flip();
        Assert.assertNotEquals(plane1, plane2);
        Assert.assertTrue(plane1.equalsApprox(plane2));
        Assert.assertTrue(plane1.isCongruentTo(plane3));
        Assert.assertFalse(plane1.equalsApprox(plane3));
    }
}
