package de.z0rdak.regionshield.test.common.core.area;

import de.z0rdak.regionshield.common.core.area.CylinderArea;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assertions.assertEquals;

public class CylinderAreaTest {

    private static CylinderArea area;

    @BeforeAll
    public static void setup(){
        BlockPos center = new BlockPos(0,0,0);
        BlockPos radiusPoint = new BlockPos(0,7,4);
        area = new CylinderArea(center, radiusPoint);
    }

    // @Test
    public void testContains(){
        // TODO: assertTrue(area.contains(BlockPos.ZERO), "Checked: " + BlockPos.ZERO);

        BlockPos pos = new BlockPos(4,1,1);
        // assertTrue(area.contains(pos), "Checked " + pos);

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
        CylinderArea cylinder = new CylinderArea(nbt);

        assertEquals(cylinder.getAreaType(), area.getAreaType());
        assertEquals(cylinder.getCenter(), area.getCenter());
        // TODO: assertEquals(cylinder.getScopePoint(), area.getScopePoint());
        assertEquals(cylinder.getHeight(), area.getHeight());

    }
}
