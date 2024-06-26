package io.github.scalamath.geomlib;

import io.github.scalamath.vecmatlib.Mat3f;
import io.github.scalamath.vecmatlib.Mat3x4f;
import io.github.scalamath.vecmatlib.Mat4f;
import org.junit.Assert;
import org.junit.Test;

public class TestAABB {

    @Test
    public void testTransform3x3() {
        var aabb = new AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f);
        var transform = Mat3f.scaling(2.0f, 3.0f, 2.0f);
        var res = new AABB(-2.0f, -3.0f, -2.0f, 4.0f, 6.0f, 4.0f);
        Assert.assertEquals(res, aabb.transform(transform));
    }

    @Test
    public void testTransform3x4() {
        var aabb = new AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f);
        var transform = Mat3x4f.translation(2.0f, 3.0f, 2.0f);
        var res = new AABB(1.0f, 2.0f, 1.0f, 2.0f, 2.0f, 2.0f);
        Assert.assertEquals(res, aabb.transform(transform));
    }

    @Test
    public void testTransform4x4() {
        var aabb = new AABB(-1.0f, -1.0f, -1.0f, 2.0f, 2.0f, 2.0f);
        var transform = Mat4f.translation(2.0f, 3.0f, 2.0f)
            .multiply(Mat4f.scaling(2.0f, 3.0f, 2.0f));
        var res = new AABB(0.0f, 0.0f, 0.0f, 4.0f, 6.0f, 4.0f);
        Assert.assertEquals(res, aabb.transform(transform));
    }

    // TODO: Inverse transform

    @Test
    public void testEqualsApprox() {
        var aabb1 = new AABB(-1.0f, -2.0f, -3.0f, 3.0f, 2.0f, 1.0f);
        var aabb2 = new AABB(-1.0000001f, -1.9999999f, -2.9999999f, 3.0000001f, 2.0000001f, 0.99999999f);
        Assert.assertNotEquals(aabb1, aabb2);
        Assert.assertTrue(aabb1.equalsApprox(aabb2));
    }
}
