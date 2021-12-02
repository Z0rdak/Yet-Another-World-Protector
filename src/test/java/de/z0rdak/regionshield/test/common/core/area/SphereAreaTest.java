package de.z0rdak.regionshield.test.common.core.area;

import de.z0rdak.regionshield.common.core.area.SphereArea;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class SphereAreaTest {

    private static final List<SphereArea> spheresToTest = new ArrayList<>();
    private static final BlockPos center = new BlockPos(0,100,0);
    private static Map<Integer, Set<BlockPos>> validPositionsForRadius;
    private static Map<Integer, Set<BlockPos>> invalidPositionsForRadius;
    private static Map<Integer, Integer> expectedValidForRadius;
    private static Map<Integer, Integer> expectedInvalidForRadius;

    @BeforeAll
    public static void setup() {
        validPositionsForRadius = new HashMap<>();
        invalidPositionsForRadius = new HashMap<>();
        expectedValidForRadius = new HashMap<>();
        expectedInvalidForRadius = new HashMap<>();

        for (int i = 0; i < 4; i++) {
            spheresToTest.add(new SphereArea(center, i));
            validPositionsForRadius.put(i, new HashSet<>());
            invalidPositionsForRadius.put(i, new HashSet<>());
        }

        expectedValidForRadius.put(0, 1);
        expectedValidForRadius.put(1, 19);
        expectedValidForRadius.put(2, 81);
        expectedValidForRadius.put(3, 179);

        expectedInvalidForRadius.put(0, 7);
        expectedInvalidForRadius.put(1, 45);
        expectedInvalidForRadius.put(2, 135);
        expectedInvalidForRadius.put(3, 333);

        for (int r = 0; r < 4; r++) {
            validPositionsForRadius.get(r).add(center);
            for (int x = -r - 1; x < r + 1; x++) {
                for (int y = -r - 1; y < r + 1; y++) {
                    for (int z = -r - 1; z < r + 1; z++) {
                        BlockPos pos = new BlockPos(center.getX() + x, center.getY() + y, center.getZ() + z);
                        if (spheresToTest.get(r).contains(pos)) {
                            validPositionsForRadius.get(r).add(pos);
                        } else {
                            invalidPositionsForRadius.get(r).add(pos);
                        }
                    }
                }
            }
        }
    }

    @Test
    public void testValidPositions(){
        for (int i = 0; i < 4; i++) {
            assertEquals(invalidPositionsForRadius.get(i).size(), expectedInvalidForRadius.get(i));
            assertEquals(validPositionsForRadius.get(i).size(), expectedValidForRadius.get(i));
        }
    }

    @Test
    public void testSerialization(){
        spheresToTest.forEach( sphereArea -> {
            CompoundNBT area = sphereArea.serializeNBT();
            SphereArea clone = new SphereArea(area);

            assertEquals(sphereArea.getCenter(), clone.getCenter(), "Center '" + sphereArea.getCenter() + "' not equal to '" + clone.getCenter().toString() + "'");
            assertEquals(sphereArea.getRadius(), clone.getRadius(), "Radius '" + sphereArea.getRadius() + "' not equal to '" + clone.getRadius() + "'");
            assertEquals(sphereArea.getAreaType(), clone.getAreaType(),"AreaType '" + sphereArea.getAreaType().toString() + "' not equal to '" + clone.getAreaType().toString() + "'");
            assertEquals(sphereArea.toString(), clone.toString(),"CuboidArea '" + sphereArea + "' not equal to '" + clone + "'");
        });
    }
}
