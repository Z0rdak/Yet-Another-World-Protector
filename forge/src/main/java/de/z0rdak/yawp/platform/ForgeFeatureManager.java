package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.config.server.FeatureConfig;
import de.z0rdak.yawp.handler.PlayerPosTracker;
import de.z0rdak.yawp.platform.services.FeatureManager;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;

public class ForgeFeatureManager implements FeatureManager {

    @Override
    public void enablePlayerTracker() {
        MinecraftForge.EVENT_BUS.addListener((TickEvent.LevelTickEvent e) -> {
            if (e.phase == TickEvent.Phase.START && e.level instanceof ServerLevel level)
                PlayerPosTracker.tickLevel(level);
        });
        MinecraftForge.EVENT_BUS.addListener((PlayerEvent.PlayerLoggedOutEvent e) -> {
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
