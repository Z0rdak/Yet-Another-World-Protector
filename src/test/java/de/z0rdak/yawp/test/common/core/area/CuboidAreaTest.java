package de.z0rdak.yawp.test.common.core.area;

import de.z0rdak.yawp.core.area.CuboidArea;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class CuboidAreaTest {

    public static Map<Integer, CuboidArea> cuboidsToTest;
    private static final BlockPos p1 = new BlockPos(0,10,0);
    private static Map<Integer, Set<BlockPos>> validPositionsForSize;
    private static Map<Integer, Set<BlockPos>> invalidPositionsForSize;
    private static Map<Integer, Integer> expectedValidForSize;
    private static Map<Integer, Integer> expectedInvalidForSize;
    public static int maxSize = 6;

    @BeforeAll
    public static void setup(){
        validPositionsForSize = new HashMap<>();
        invalidPositionsForSize = new HashMap<>();
        expectedValidForSize = new HashMap<>();
        expectedInvalidForSize = new HashMap<>();
        cuboidsToTest = new HashMap<>();

        for (int i = 1; i < maxSize; i++) {
            cuboidsToTest.put(i, new CuboidArea(p1, p1.offset(i-1,i-1,i-1)));
            validPositionsForSize.put(i, new HashSet<>());
            invalidPositionsForSize.put(i, new HashSet<>());
        }
        expectedValidForSize.put(1, 1);
        expectedValidForSize.put(2, 8);
        expectedValidForSize.put(3, 27);
        expectedValidForSize.put(4, 64);
        expectedValidForSize.put(5, 125);

        expectedInvalidForSize.put(1, 26);
        expectedInvalidForSize.put(2, 56);
        expectedInvalidForSize.put(3, 98);
        expectedInvalidForSize.put(4, 152);
        expectedInvalidForSize.put(5, 218);

        for (int size = 1; size < maxSize; size++) {
            for (int x = p1.getX() - 1; x < p1.getX() + size + 1; x++) {
                for (int z = p1.getZ() - 1; z < p1.getZ() + size + 1; z++) {
                    for (int y = p1.getY() - 1; y < p1.getY() + size + 1; y++) {
                        BlockPos pos = new BlockPos(x, y, z);
                        if (cuboidsToTest.get(size).contains(pos)) {
                            validPositionsForSize.get(size).add(pos);
                        } else {
                            invalidPositionsForSize.get(size).add(pos);
                        }
                    }
                }
            }
        }
    }

    @Test
    public void testValidPositions(){
        for (int i = 1; i < maxSize; i++) {
            assertEquals(validPositionsForSize.get(i).size(), expectedValidForSize.get(i), "Valid positions test for '" + cuboidsToTest.get(i) + "' (area " + i + ") failed!");
            assertEquals(invalidPositionsForSize.get(i).size(), expectedInvalidForSize.get(i), "Invalid positions test for '" + cuboidsToTest.get(i) + "' (area " + i + ") failed!");
        }
    }

    @Test
    public void testSerialization(){
        cuboidsToTest.forEach( (k,cuboidArea) -> {
            CompoundNBT area = cuboidArea.serializeNBT();
            CuboidArea clone = new CuboidArea(area);

            assertEquals(cuboidArea.getArea(), clone.getArea(), "Area '" + cuboidArea.getArea().toString() + "' not equal to '" + clone.getArea().toString() + "'");
            assertEquals(cuboidArea.getAreaP1(), clone.getAreaP1(), "Point 1 '" + cuboidArea.getAreaP1().toShortString() + "' not equal to '" + clone.getAreaP1().toShortString() + "'");
            assertEquals(cuboidArea.getAreaP2(), clone.getAreaP2(), "Point 2 '" + cuboidArea.getAreaP2().toShortString() + "' not equal to '" + clone.getAreaP2().toShortString() + "'");
            assertEquals(cuboidArea.getAreaType(), clone.getAreaType(),"AreaType '" + cuboidArea.getAreaType().toString() + "' not equal to '" + clone.getAreaType().toString() + "'");
            assertEquals(cuboidArea.toString(), clone.toString(),"CuboidArea '" + cuboidArea + "' not equal to '" + clone + "'");
        });
    }
}
