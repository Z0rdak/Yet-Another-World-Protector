package de.z0rdak.yawp;

import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.ConfigRegistry;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.handler.CommonEvents;
import de.z0rdak.yawp.handler.flags.PlayerFlagHandler;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.fabricmc.api.DedicatedServerModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.entity.event.v1.ServerEntityWorldChangeEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerEntityEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerWorldEvents;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class YetAnotherWorldProtector implements DedicatedServerModInitializer {

    public static final String MODID = "yawp";
    public static final Logger LOGGER = LogManager.getLogger("YAWP");

    @Override
    public void onInitializeServer() {
        // callback to register commands
        CommandRegistrationCallback.EVENT.register(CommandRegistry::registerCommands);

        /* Register event handler for managing persistent region data */
        ServerLifecycleEvents.SERVER_STARTING.register(RegionDataManager::initServerInstance);
        ServerLifecycleEvents.SERVER_STARTING.register(CommandPermissionConfig::initServerInstance);
        ServerWorldEvents.LOAD.register(RegionDataManager::loadRegionDataForWorld);
        ServerEntityEvents.ENTITY_LOAD.register(RegionDataManager::onPlayerLoadAddDimKey);
        ServerEntityWorldChangeEvents.AFTER_PLAYER_CHANGE_WORLD.register(RegionDataManager::onPlayerChangeWorldAddDimKey);

        ConfigRegistry.register();
        CommonEvents.register();
        PlayerFlagHandler.registerEventHandler();
    }
}
