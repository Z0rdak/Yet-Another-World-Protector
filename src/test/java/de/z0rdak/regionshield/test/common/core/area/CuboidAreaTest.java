package de.z0rdak.regionshield.test.common.core.area;

import de.z0rdak.regionshield.common.core.area.CuboidArea;
import de.z0rdak.regionshield.common.core.area.SphereArea;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CuboidAreaTest {

    private static CuboidArea area;

    @BeforeAll
    public static void setup(){
        BlockPos p1 = new BlockPos(0,0,0);
        BlockPos p2 = new BlockPos(5,5,5);
        area = new CuboidArea(new AxisAlignedBB(p1,p2));
    }

    @Test
    public void testContains(){
        assertTrue(area.contains(BlockPos.ZERO), "Checked: " + BlockPos.ZERO);

        BlockPos pos = new BlockPos(4,1,1);
        assertTrue(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(-3, 3,2);
        assertFalse(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(1,4,2);
        assertTrue(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(1,1,5);
        assertTrue(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(4,6,5);
        assertFalse(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(4,4,-1);
        assertFalse(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(4,5,4);
        assertTrue(area.contains(pos), "Checked " + pos);

        pos = new BlockPos(4,4,4);
        assertTrue(area.contains(pos), "Checked " + pos);

    }

    @Test
    public void testSerialization(){
        CompoundNBT nbt = area.serializeNBT();
        CuboidArea cuboid = new CuboidArea(nbt);

        assertEquals(cuboid.getAreaType(), area.getAreaType());
        assertEquals(cuboid.getArea(), area.getArea());
    }
}
