package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.config.server.FeatureConfig;
import de.z0rdak.yawp.handler.PlayerPosTracker;
import de.z0rdak.yawp.platform.services.FeatureManager;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerTickEvents;
import net.fabricmc.fabric.api.networking.v1.ServerPlayConnectionEvents;

public class FabricFeatureManager implements FeatureManager {
    @Override
    public void enablePlayerTracker() {
        ServerTickEvents.START_WORLD_TICK.register(PlayerPosTracker::tickLevel);
        ServerPlayConnectionEvents.DISCONNECT.register(
                (e, s) -> PlayerPosTracker.onPlayerDisc(e.getPlayer()));
    }

    @Override
    public boolean shouldCreateNewLevelData() {
        return FeatureConfig.shouldCreateNewLevelData();
    }
}
