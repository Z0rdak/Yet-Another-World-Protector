package de.z0rdak.yawp.test.common.core.area;

import de.z0rdak.yawp.core.area.SphereArea;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
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

    public static List<BlockPos> getBlocksJustOutOfSphere(BlockPos center, int radius) {
        List<BlockPos> blocksOnSphereSurface = new ArrayList<>();

        int centerX = center.getX();
        int centerY = center.getY();
        int centerZ = center.getZ();

        for (int x = centerX - radius; x <= centerX + radius; x++) {
            for (int y = centerY - radius; y <= centerY + radius; y++) {
                for (int z = centerZ - radius; z <= centerZ + radius; z++) {
                    double distanceSquared = Math.pow(x - centerX, 2) + Math.pow(y - centerY, 2) + Math.pow(z - centerZ, 2);
                    if (Math.abs(distanceSquared - radius * radius) < 1e-6) {
                        blocksOnSphereSurface.add(new BlockPos(x, y, z));
                    }
                }
            }
        }

        return blocksOnSphereSurface;
    }

    public static List<BlockPos> getBlocksInSphereHalf(BlockPos center, int radius) {
        List<BlockPos> blocksInSphereHalf = new ArrayList<>();

        int centerX = center.getX();
        int centerY = center.getY();
        int centerZ = center.getZ();

        for (int x = centerX - radius; x <= centerX + radius; x++) {
            for (int y = centerY - radius; y <= centerY + radius; y++) {
                for (int z = centerZ; z <= centerZ + radius; z++) {
                    double distanceSquared = Math.pow(x - centerX, 2) + Math.pow(y - centerY, 2) + Math.pow(z - centerZ, 2);
                    if (distanceSquared <= radius * radius) {
                        blocksInSphereHalf.add(new BlockPos(x, y, z));
                    }
                }
            }
        }

        return blocksInSphereHalf;
    }

    @Test
    public void testContainsBlocks() {
        int radius = 5;
        BlockPos radiusPos = new BlockPos(0, radius, 0);
        SphereArea sphere = new SphereArea(new BlockPos(0, 0, 0), radiusPos);
        // SphereArea sphere1 = new SphereArea(new BlockPos(0, 0, 0), radius);
        System.out.println("Testing sphere with radius " + radius);
        System.out.println("Center: " + sphere.getCenterPos().toShortString());
        System.out.println("Radius: " + sphere.getRadius());
        System.out.println("Test containment: ");
        List<BlockPos> blocksInSphereHalf = getBlocksInSphereHalf(sphere.getCenterPos(), radius);
        for (BlockPos pos : blocksInSphereHalf) {
            System.out.println("\tChecking " + pos.toShortString() + " in sphere");
            assertTrue(sphere.contains(pos), "Sphere does not contain " + pos);
        }

        System.out.println("Test no containment: ");
        getBlocksJustOutOfSphere(sphere.getCenterPos(), radius + 1).forEach(pos -> {
            System.out.println("\tChecking " + pos.toShortString() + " out of sphere hull");
            assertFalse(sphere.contains(pos), "Sphere does contain " + pos);
        });
    }

    @Test
    public void testCreation() {
        for (int i = 0; i < 10; i++) {
            int radius = i;
            System.out.println("Testing sphere with radius " + radius);
            BlockPos radiusPos = new BlockPos(0, radius, 0);
            SphereArea sa1 = new SphereArea(new BlockPos(0, 0, 0), radiusPos);
            SphereArea sa2 = new SphereArea(new BlockPos(0, 0, 0), radius);
            assertEquals(sa1.getCenter(), sa2.getCenter());
            assertEquals(sa1.getRadius(), sa2.getRadius());
            assertArrayEquals(sa1.getMarkedBlocks().toArray(), sa2.getMarkedBlocks().toArray());
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
            CompoundTag area = sphereArea.serializeNBT();
            SphereArea clone = new SphereArea(area);

            // assertEquals(sphereArea.getCenter(), clone.getCenter(), "Center '" + sphereArea.getCenter() + "' not equal to '" + clone.getCenter().toString() + "'");
            // assertEquals(sphereArea.getRadius(), clone.getRadius(), "Radius '" + sphereArea.getRadius() + "' not equal to '" + clone.getRadius() + "'");
            // assertEquals(sphereArea.getAreaType(), clone.getAreaType(),"AreaType '" + sphereArea.getAreaType().toString() + "' not equal to '" + clone.getAreaType().toString() + "'");
            // assertEquals(sphereArea.toString(), clone.toString(),"CuboidArea '" + sphereArea + "' not equal to '" + clone + "'");
        });
    }
}
