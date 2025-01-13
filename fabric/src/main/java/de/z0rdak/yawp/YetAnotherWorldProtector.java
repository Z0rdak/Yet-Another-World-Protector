package de.z0rdak.yawp;

import de.z0rdak.yawp.api.events.flag.FabricFlagEvents;
import de.z0rdak.yawp.api.events.flag.FlagEvent;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.ConfigRegistry;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.handler.flags.PlayerFlagHandler;
import de.z0rdak.yawp.platform.Services;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.entity.event.v1.ServerEntityWorldChangeEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerEntityEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerWorldEvents;

import static de.z0rdak.yawp.handler.YawpEventHandler.removeInvolvedEntities;

public class YetAnotherWorldProtector implements ModInitializer, YAWPModInitializer {
    
    private static void onAddFlag(FlagEvent.AddFlagEvent event) {
        if (event.getFlag().getName().contains("spawning") && Services.FLAG_CONFIG.removeEntitiesEnabled()) {
            removeInvolvedEntities(event.getSrc(), event.getRegion(), RegionFlag.fromId(event.getFlag().getName()));
        }
    }

    @Override
    public void onInitialize() {
        YAWPCommon.init();

        registerConfig();
        initServerInstance();
        loadRegionData();
        addDimKeyOnPlayerLogin();
        addDimKeyOnDimensionChange();
        registerCommands();

        // register flag handlers
        PlayerFlagHandler.register();
        FabricFlagEvents.ADD_FLAG.register(YetAnotherWorldProtector::onAddFlag);
    }

    @Override
    public void registerCommands() {
        CommandRegistrationCallback.EVENT.register(CommandRegistry::registerCommands);
    }

    @Override
    public void initServerInstance() {
        ServerLifecycleEvents.SERVER_STARTING.register(RegionDataManager::initServerInstance);
    }

    @Override
    public void loadRegionData() {
        ServerWorldEvents.LOAD.register(RegionDataManager::loadRegionDataForWorld);
    }

    @Override
    public void addDimKeyOnPlayerLogin() {
        ServerEntityEvents.ENTITY_LOAD.register(RegionDataManager::addDimKeyOnPlayerLogin);
    }

    @Override
    public void addDimKeyOnDimensionChange() {
        ServerEntityWorldChangeEvents.AFTER_PLAYER_CHANGE_WORLD.register(RegionDataManager::addDimKeyOnDimensionChange);
    }

    @Override
    public void registerConfig() {
        Services.CONFIG_REGISTRY.register();
    }
}
