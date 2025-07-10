package de.z0rdak.yawp;

import de.z0rdak.yawp.api.visualization.VisualizationManager;
import de.z0rdak.yawp.api.events.flag.FabricFlagEvents;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.data.PlayerManager;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.handler.flags.PlayerFlagHandler;
import de.z0rdak.yawp.platform.Services;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.entity.event.v1.ServerEntityWorldChangeEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerChunkEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerEntityEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerWorldEvents;
import net.minecraft.server.level.ServerLevel;

import static de.z0rdak.yawp.handler.YawpEventHandler.removeInvolvedEntities;

public class YetAnotherWorldProtector implements ModInitializer, YAWPModInitializer {

    @Override
    public void onInitialize() {
        YAWPCommon.init();

        registerConfig();
        setupRegionDataLifecycleHooks();
        registerCommands();

        // register flag handlers
        PlayerFlagHandler.register();
        FabricFlagEvents.ADD_FLAG.register(YetAnotherWorldProtector::onAddFlag);
    }

    private static void onAddFlag(FlagEvent.AddFlagEvent event) {
        if (event.getFlag().getName().contains("spawning") && Services.FLAG_CONFIG.removeEntitiesEnabled()) {
            removeInvolvedEntities(event.getSrc(), event.getRegion(), RegionFlag.fromId(event.getFlag().getName()));
        }
    }

    @Override
    public void registerCommands() {
        CommandRegistrationCallback.EVENT.register(CommandRegistry::registerCommands);
    }

    @Override
    public void setupRegionDataLifecycleHooks() {
        ServerLifecycleEvents.SERVER_STARTING.register(RegionDataManager::onServerStarting);
        ServerLifecycleEvents.SERVER_STARTING.register(PlayerManager::onServerStart);
        ServerLifecycleEvents.SERVER_STARTING.register(VisualizationManager::initServerInstance);
        ServerWorldEvents.LOAD.register((server, level) -> {
            if (level.dimension().equals(ServerLevel.OVERWORLD)) {
                RegionDataManager.loadLevelListData(server);
            }
        });
        ServerWorldEvents.LOAD.register(RegionDataManager::worldLoad);
        ServerEntityEvents.ENTITY_LOAD.register(RegionDataManager::initLevelDataOnLogin);
        ServerEntityWorldChangeEvents.AFTER_PLAYER_CHANGE_WORLD.register(RegionDataManager::initLevelDataOnChangeWorld);
        ServerLifecycleEvents.BEFORE_SAVE.register(RegionDataManager::save);
        ServerWorldEvents.UNLOAD.register(RegionDataManager::saveOnUnload);
        ServerLifecycleEvents.SERVER_STOPPING.register(RegionDataManager::saveOnStop);
    }

    @Override
    public void registerConfig() {
        Services.CONFIG_REGISTRY.register();
    }
}
