package de.z0rdak.yawp.gametest;

import net.fabricmc.fabric.api.gametest.v1.GameTest;
import net.minecraft.gametest.framework.GameTestHelper;

/**
 * Fabric registration of the shared scenarios, see {@link BlockInteractionScenarios}.
 * Registered as {@code fabric-gametest} entrypoint in this source set's {@code fabric.mod.json}.
 */
public class FabricGameTests {

    @GameTest
    public void blockInHand_placeBlocksDenied_doesNotPlaceBlock(GameTestHelper helper) {
        BlockInteractionScenarios.blockInHand_placeBlocksDenied_doesNotPlaceBlock(helper);
    }

    @GameTest
    public void blockInHand_placeBlocksAllowed_placesBlock(GameTestHelper helper) {
        BlockInteractionScenarios.blockInHand_placeBlocksAllowed_placesBlock(helper);
    }
}
