package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.config.server.FeatureConfig;
import de.z0rdak.yawp.handler.PlayerPosTracker;
import de.z0rdak.yawp.platform.services.FeatureManager;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.tick.LevelTickEvent;

public class NeoForgeFeatureManager implements FeatureManager {

    @Override
    public void enablePlayerTracker() {
        NeoForge.EVENT_BUS.addListener((LevelTickEvent.Pre e) -> {
            if (e.getLevel() instanceof ServerLevel level)
                PlayerPosTracker.tickLevel(level);
        });
        NeoForge.EVENT_BUS.addListener((PlayerEvent.PlayerLoggedOutEvent e) -> {
            if (!e.getEntity().level().isClientSide() && e.getEntity() instanceof ServerPlayer player) {
                PlayerPosTracker.onPlayerDisc(player);
            }
        });
    }

    @Override
    public boolean shouldCreateNewLevelData() {
        return FeatureConfig.shouldCreateNewLevelData();
    }
}
