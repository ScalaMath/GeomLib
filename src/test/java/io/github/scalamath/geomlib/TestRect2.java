package io.github.scalamath.geomlib;

import io.github.scalamath.vecmatlib.Mat2f;
import io.github.scalamath.vecmatlib.Mat2x3f;
import io.github.scalamath.vecmatlib.Mat3f;
import org.junit.Assert;
import org.junit.Test;

public class TestRect2 {

    @Test
    public void testTransform2x2() {
        var rotation = Mat2f.rotation(Math.PI / 2.0);
        var rect = new Rect2(4.0f, 3.0f);
        var res1 = new Rect2(-3.0f, 0.0f, 3.0f, 4.0f);
        Assert.assertEquals(res1, rect.transform(rotation));
        var scaling = Mat2f.scaling(2.0f, 3.0f);
        var res2 = new Rect2(8.0f, 9.0f);
        Assert.assertEquals(res2, rect.transform(scaling));
    }

    @Test
    public void testTransform2x3() {
        var translation = Mat2x3f.translation(2.0f, 3.0f);
        var rect = new Rect2(2.0f, 1.0f, 4.0f, 2.0f);
        var res1 = new Rect2(4.0f, 4.0f, 4.0f, 2.0f);
        Assert.assertEquals(res1, rect.transform(translation));
        var transform = Mat2f.rotation(Math.PI / 2.0).multiply(translation);
        var res2 = new Rect2(-6.0f, 4.0f, 2.0f, 4.0f);
        Assert.assertEquals(res2, rect.transform(transform));
    }

    @Test
    public void testTransform3x3() {
        var translation = Mat3f.translation(2.0f, 3.0f);
        var rect = new Rect2(2.0f, 1.0f, 4.0f, 2.0f);
        var res1 = new Rect2(4.0f, 4.0f, 4.0f, 2.0f);
        Assert.assertEquals(res1, rect.transform(translation));
        var transform = Mat3f.rotationZ(Math.PI / 2.0).multiply(translation);
        var res2 = new Rect2(-6.0f, 4.0f, 2.0f, 4.0f);
        Assert.assertEquals(res2, rect.transform(transform));
    }

    // TODO: Inverse transform

    @Test
    public void testEqualsApprox() {
        var rect1 = new Rect2(2.0f, 1.0f, 4.0f, 3.0f);
        var rect2 = new Rect2(1.9999999f, 1.0000001f, 4.0000001f, 2.9999999f);
        var rect3 = new Rect2(6.0f, 4.0f, -4.0f, -3.0f);
        Assert.assertNotEquals(rect1, rect2);
        Assert.assertTrue(rect1.equalsApprox(rect2));
        Assert.assertFalse(rect1.equalsApprox(rect3));
    }
}
