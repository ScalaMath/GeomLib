package io.github.scalamath.geomlib;

import org.junit.Assert;
import org.junit.Test;

public class TestSphere {

    @Test
    public void testEqualsApprox() {
        var s1 = new Sphere(1.0f, 1.0f, 2.0f, 3.0f);
        var s2 = new Sphere(1.0000001f, 0.9999999f, 2.0000001f, 2.9999999f);
        Assert.assertNotEquals(s1, s2);
        Assert.assertTrue(s1.equalsApprox(s2));
    }
}
