package de.z0rdak.regionshield.test.common.core.area;

import de.z0rdak.regionshield.common.core.area.SphereArea;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class SphereAreaTest {

    private static SphereArea area;

    @BeforeAll
    public static void setup(){
        BlockPos centerPoint = new BlockPos(0,0,0);
        area = new SphereArea(centerPoint, 5);
    }

    @Test
    public void testContains(){
        assertTrue(area.contains(BlockPos.ZERO), "Checked: " + BlockPos.ZERO);

        BlockPos pos = new BlockPos(4,1,1);
        assertTrue(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(-3, 3,2);
        assertTrue(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(1,4,2);
        assertTrue(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(1,1,4);
        assertTrue(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(5,5,5);
        assertFalse(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(-3, 3,3);
        assertFalse(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(-3, 2,4);
        assertFalse(area.contains(pos), "Checked " + pos);

    }

    @Test
    public void testSerialization(){
        CompoundNBT nbt = area.serializeNBT();
        SphereArea sphere = new SphereArea(nbt);

        assertEquals(sphere.getAreaType(), area.getAreaType());
        assertEquals(sphere.getCenterP(), area.getCenterP());
        assertEquals(sphere.getRadius(), area.getRadius());
    }

}
