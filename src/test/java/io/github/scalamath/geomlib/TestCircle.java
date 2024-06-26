package io.github.scalamath.geomlib;

import org.junit.Assert;
import org.junit.Test;

public class TestCircle {

    @Test
    public void testEqualsApprox() {
        var c1 = new Circle(1.0f, 1.0f, 2.0f);
        var c2 = new Circle(1.0000001f, 0.9999999f, 2.0000001f);
        Assert.assertNotEquals(c1, c2);
        Assert.assertTrue(c1.equalsApprox(c2));
    }
}
