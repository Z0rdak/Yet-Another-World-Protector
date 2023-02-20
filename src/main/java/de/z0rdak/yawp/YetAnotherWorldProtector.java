package de.z0rdak.yawp;

import com.mojang.brigadier.CommandDispatcher;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.handler.CommonEvents;
import de.z0rdak.yawp.handler.flags.PlayerFlagHandler;
import de.z0rdak.yawp.handler.flags.WorldFlagHandler;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.fabricmc.api.ModInitializer;
import net.fabricmc.fabric.api.command.v2.CommandRegistrationCallback;
import net.fabricmc.fabric.api.entity.event.v1.ServerEntityWorldChangeEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerEntityEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerLifecycleEvents;
import net.fabricmc.fabric.api.event.lifecycle.v1.ServerWorldEvents;
import net.minecraft.command.CommandRegistryAccess;
import net.minecraft.server.command.CommandManager;
import net.minecraft.server.command.ServerCommandSource;
import net.minecraftforge.api.ModLoadingContext;
import net.minecraftforge.fml.config.ModConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class YetAnotherWorldProtector implements ModInitializer {

    public static final String MODID = "yawp";
    public static final Logger LOGGER = LogManager.getLogger();

    private static void registerCommands(CommandDispatcher<ServerCommandSource> commandDispatcher, CommandRegistryAccess commandRegistryAccess, CommandManager.RegistrationEnvironment registrationEnvironment) {
        if (registrationEnvironment.dedicated) {
            CommandRegistry.init(commandDispatcher, "wp");
        } else {
            // Only on integrated server
        }
    }

    @Override
    public void onInitialize() {
        // callback to register commands
        CommandRegistrationCallback.EVENT.register(YetAnotherWorldProtector::registerCommands);

        /* Register event handler for managing persistent region data */
        ServerLifecycleEvents.SERVER_STARTING.register(RegionDataManager::initServerInstance);
        ServerLifecycleEvents.SERVER_STARTING.register(CommandPermissionConfig::initServerInstance);
        ServerWorldEvents.LOAD.register(RegionDataManager::loadRegionDataForWorld);
        ServerEntityEvents.ENTITY_LOAD.register(RegionDataManager::onPlayerLoadAddDimKey);
        ServerEntityWorldChangeEvents.AFTER_PLAYER_CHANGE_WORLD.register(RegionDataManager::onPlayerChangeWorldAddDimKey);

        ModLoadingContext.registerConfig(MODID, ModConfig.Type.SERVER, CommandPermissionConfig.CONFIG_SPEC, CommandPermissionConfig.CONFIG_NAME);
        ModLoadingContext.registerConfig(MODID, ModConfig.Type.SERVER, FlagConfig.CONFIG_SPEC, FlagConfig.CONFIG_NAME);
        ModLoadingContext.registerConfig(MODID, ModConfig.Type.SERVER, RegionConfig.CONFIG_SPEC, RegionConfig.CONFIG_NAME);

        // TODO: Class for event listener for different types of flags
        CommonEvents.register();

        PlayerFlagHandler.registerEventHandler();
        WorldFlagHandler.registerEventHandler();
    }
}
