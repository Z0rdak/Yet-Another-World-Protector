package de.z0rdak.yawp;

import de.z0rdak.yawp.api.FlagRegister;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.api.visualization.VisualizationManager;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.ConfigRegistry;

import de.z0rdak.yawp.data.PlayerManager;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.handler.YawpEventHandler;
import de.z0rdak.yawp.handler.flags.PlayerFlagHandler;
import de.z0rdak.yawp.platform.Services;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.entity.event.v1.ServerEntityLevelChangeEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerEntityEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLevelEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;

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
        ServerLifecycleEvents.SERVER_STARTING.register(YawpEventHandler::storeRef);
    }

    private static void onAddFlag(FlagEvent.Add event) {
        if (event.getFlag().getName().contains("spawning") && Services.FLAG_CONFIG.removeEntitiesEnabled()) {
            removeInvolvedEntities(event.getRegion(), FlagRegister.byId(event.getFlag().getName()));
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
        ServerLevelEvents.LOAD.register((server, level) -> {
            if (level.dimension().equals(ServerLevel.OVERWORLD)) {
                RegionDataManager.loadLevelListData(server);
            }
        });
        ServerLevelEvents.LOAD.register(RegionDataManager::worldLoad);
        ServerEntityEvents.ENTITY_LOAD.register(RegionDataManager::initLevelDataOnLogin);
        ServerEntityLevelChangeEvents.AFTER_PLAYER_CHANGE_LEVEL.register(RegionDataManager::initLevelDataOnChangeWorld);
        ServerLifecycleEvents.BEFORE_SAVE.register(RegionDataManager::save);
        ServerLevelEvents.UNLOAD.register((server, level) -> RegionDataManager.saveOnUnload(level));
        ServerLifecycleEvents.SERVER_STOPPING.register(RegionDataManager::saveOnStop);
    }

    @Override
    public void registerConfig() {
        Services.CONFIG_REGISTRY.register();
    }
}
