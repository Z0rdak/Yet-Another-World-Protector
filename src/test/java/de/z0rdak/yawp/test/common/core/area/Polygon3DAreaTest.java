package de.z0rdak.yawp.test.common.core.area;

import de.z0rdak.yawp.core.area.Polygon3DArea;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import org.junit.jupiter.api.BeforeAll;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class Polygon3DAreaTest {

    private static Polygon3DArea area;

    @BeforeAll
    public static void setup(){
        List<BlockPos> posList = new ArrayList<>();
        posList.add(new BlockPos(2, -1, 3));
        posList.add(new BlockPos(2, 5, 3));

        posList.add(new BlockPos(-2, -1, 3));
        posList.add(new BlockPos(-2, 5, 3));

        posList.add(new BlockPos(-3, -1, -1));
        posList.add(new BlockPos(-3, 5, -1));

        posList.add(new BlockPos(0, -1, -4));
        posList.add(new BlockPos(0, 5, -4));

        posList.add(new BlockPos(5, -1, 0));
        posList.add(new BlockPos(5, 5, 0));
        area = new Polygon3DArea(posList);
    }

    // TODO: check contains
    // @Test
    public void testContains(){
       BlockPos pos = BlockPos.ZERO; // true
        assertTrue(area.contains(pos));
        pos = new BlockPos(1,1,1); // true
        assertTrue(area.contains(pos));
        pos = new BlockPos(2,4,3); // true
        assertTrue(area.contains(pos));
        pos = new BlockPos(5,5,1); // true
        assertTrue(area.contains(pos));
        pos = new BlockPos(-1, 4, -2); // true
        assertTrue(area.contains(pos));
        pos = new BlockPos(6, 1, 1); // false
        assertFalse(area.contains(pos));
        pos = new BlockPos(3, 4, 2); // false
        assertFalse(area.contains(pos));
        pos = new BlockPos(3, -1, 2); // false
        assertFalse(area.contains(pos));
    }



    public void testSerialization(){
        CompoundTag nbt = area.serializeNBT();
        Polygon3DArea polygon3DArea = new Polygon3DArea(nbt);

        assertEquals(polygon3DArea.getAreaType(), area.getAreaType());
        assertEquals(polygon3DArea.getPositions(), area.getPositions());
    }

}
