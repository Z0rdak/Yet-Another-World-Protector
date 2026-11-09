package de.z0rdak.yawp.gametest;

import de.z0rdak.yawp.core.flag.RegionFlag;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;

/**
 * Loader independent game test scenarios.
 * <p>
 * Every {@code public static void name(GameTestHelper)} method is a test:
 * <ul>
 *     <li>NeoForge: registered automatically (see {@code NeoForgeGameTests}).</li>
 *     <li>Fabric: needs a delegating {@code @GameTest} method in {@code FabricGameTests}.</li>
 * </ul>
 * Tests run in an empty 8x8x8 structure, positions are relative to it.
 * Run with {@code ./gradlew :fabric:runGameTest} and {@code ./gradlew :neoforge:runGameTestServer}.
 */
public final class BlockInteractionScenarios {

    private BlockInteractionScenarios() {

    }


    public static void blockInHand_placeBlocksDenied_doesNotPlaceBlock(GameTestHelper helper) {
        var target = new BlockPos(3, 2, 3);
        helper.setBlock(target, Blocks.STONE);
        try (TestArea _ = TestArea.around(helper).deny(RegionFlag.PLACE_BLOCKS).create()) {
            var player = GameTestUtils.makeMockServerPlayer(helper);
            FlagRecorder.FlagLog log = FlagRecorder.watch(player);

            GameTestUtils.click(helper, player, Items.STONE, false, target, Direction.UP);

            helper.assertTrue(log.wasDenied(RegionFlag.PLACE_BLOCKS), "place-blocks should be denied, was " + log.describe());
            helper.assertBlockNotPresent(Blocks.STONE, target.above());
        }
        helper.succeed();
    }

    public static void blockInHand_placeBlocksAllowed_placesBlock(GameTestHelper helper) {
        var target = new BlockPos(3, 2, 3);
        helper.setBlock(target, Blocks.STONE);
        try (TestArea _ = TestArea.around(helper).allow(RegionFlag.PLACE_BLOCKS).create()) {
            var player = GameTestUtils.makeMockServerPlayer(helper);
            FlagRecorder.FlagLog log = FlagRecorder.watch(player);

            GameTestUtils.click(helper, player, Items.STONE, false, target, Direction.UP);

            helper.assertTrue(log.wasAllowed(RegionFlag.PLACE_BLOCKS), "place-blocks should be allowed, was " + log.describe());
            helper.assertBlockPresent(Blocks.STONE, target.above());
        }
        helper.succeed();
    }
}
