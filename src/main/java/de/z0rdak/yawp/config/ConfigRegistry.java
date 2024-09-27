package de.z0rdak.yawp.config;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.api.events.region.RegionEvents;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import fuzs.forgeconfigapiport.api.config.v2.ForgeConfigRegistry;
import fuzs.forgeconfigapiport.api.config.v2.ModConfigEvents;
import net.fabricmc.loader.api.FabricLoader;
import net.minecraftforge.fml.config.ModConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static de.z0rdak.yawp.YetAnotherWorldProtector.MODID;

public final class ConfigRegistry {

    public static final Logger CONFIG_LOGGER = LogManager.getLogger(YetAnotherWorldProtector.MODID.toUpperCase() + "-Config");

    private ConfigRegistry() {
    }

    public static void register() {
        ModConfigEvents.loading(MODID).register(ConfigRegistry::onModLoading);
        ModConfigEvents.reloading(MODID).register(ConfigRegistry::onModReloading);

        // registering configuration
        ForgeConfigRegistry.INSTANCE.register(MODID, ModConfig.Type.SERVER, CommandPermissionConfig.CONFIG_SPEC, CommandPermissionConfig.CONFIG_NAME);
        ForgeConfigRegistry.INSTANCE.register(MODID, ModConfig.Type.SERVER, FlagConfig.CONFIG_SPEC, FlagConfig.CONFIG_NAME);
        ForgeConfigRegistry.INSTANCE.register(MODID, ModConfig.Type.SERVER, RegionConfig.CONFIG_SPEC, RegionConfig.CONFIG_NAME);
        ForgeConfigRegistry.INSTANCE.register(MODID, ModConfig.Type.SERVER, LoggingConfig.CONFIG_SPEC, LoggingConfig.CONFIG_NAME);
    }

    private static void onModReloading(ModConfig modConfig) {
        if (modConfig.getModId().equals(MODID)) {
            CONFIG_LOGGER.info("Reloaded: '{}'", modConfig.getFileName());
        }
    }

    private static void onModLoading(ModConfig modConfig) {
        if (modConfig.getModId().equals(MODID)) {
            switch (modConfig.getFileName()) {
                case CommandPermissionConfig.CONFIG_NAME: {
                    CommandPermissionConfig.BASE_CMD = CommandPermissionConfig.getBaseCmd();
                    if (FabricLoader.getInstance().isModLoaded("journeymap")) {
                        CommandPermissionConfig.BASE_CMD = CommandPermissionConfig.getBaseCmdAlt();
                        CONFIG_LOGGER.info("Detected JourneyMap to be loaded beside YAWP.");
                    }
                    CONFIG_LOGGER.info("Setting YAWP base command to '/{}'", CommandPermissionConfig.BASE_CMD);
                    int numOfUuidsWithPermission = CommandPermissionConfig.UUIDsWithPermission().size();
                    String uuidsWithPermission = (numOfUuidsWithPermission > 0
                            ? ": " + String.join(", ", CommandPermissionConfig.UUIDsWithPermission())
                            : "");
                    CONFIG_LOGGER.info("{} UUID(s) with permission read from config{}", numOfUuidsWithPermission, uuidsWithPermission);
                    CommandRegistry.register(CommandPermissionConfig.BASE_CMD);
                }
                break;
                case RegionConfig.CONFIG_NAME: {
                    int numLocalDefaultFlags = RegionConfig.getDefaultFlags().size();
                    String loadedLocalFlags = (numLocalDefaultFlags > 0
                            ? ": " + String.join(", ", RegionConfig.getDefaultFlags())
                            : "");
                    CONFIG_LOGGER.info("{} default flag(s) for Local Regions read from config{}", numLocalDefaultFlags, loadedLocalFlags);

                    int numDimDefaultFlags = RegionConfig.getDefaultDimFlags().size();
                    String loadedDimFlags = (numDimDefaultFlags > 0
                            ? ": " + String.join(", ", RegionConfig.getDefaultDimFlags())
                            : "");
                    CONFIG_LOGGER.info("{} default flag(s) for Dimensional Regions read from config{}", numDimDefaultFlags, loadedDimFlags);
                }
                break;
                case FlagConfig.CONFIG_NAME: {
                    int numBreakEntityEntries = FlagConfig.getCoveredBlockEntities().size();
                    String loadedBreakEntities = (numBreakEntityEntries > 0
                            ? ": " + String.join(", ", FlagConfig.getCoveredBlockEntities())
                            : "");
                    CONFIG_LOGGER.info("{} Block Entity entries read from config{}", numBreakEntityEntries, loadedBreakEntities);

                    int numBreakEntityTagEntries = FlagConfig.getCoveredBlockEntityTags().size();
                    String loadedBreakEntityTags = (numBreakEntityTagEntries > 0
                            ? ": " + String.join(", ", FlagConfig.getCoveredBlockEntityTags())
                            : "");
                    CONFIG_LOGGER.info("{} Block Entity tag entries read from config{}", numBreakEntityTagEntries, loadedBreakEntityTags);
                    break;
                }
                case LoggingConfig.CONFIG_NAME: {
                    CONFIG_LOGGER.info("Logging flag checks: {}", LoggingConfig.shouldLogFlagChecks());
                    CONFIG_LOGGER.info("Logging flag check results: {}", LoggingConfig.shouldLogFlagCheckResults());
                    CONFIG_LOGGER.info("Logging flag categories: [{}]", String.join(",", LoggingConfig.getFlagCategories()));
                    CONFIG_LOGGER.info("Logging flags: [{}]", String.join(",", LoggingConfig.getFlagsToLog()));
                    CONFIG_LOGGER.info("Logging empty flag results: {}", LoggingConfig.shouldLogEmptyResults());
                    // CONFIG_LOGGER.info("Logging detailed player flag checks: {}", LoggingConfig.shouldLogDetailedPlayerFlags());

                    if (LoggingConfig.shouldLogFlagChecks()) {
                        RegionEvents.CHECK_FLAG.register(LoggingConfig::logCheck);
                    }
                    if (LoggingConfig.shouldLogFlagCheckResults()) {
                        RegionEvents.FLAG_RESULT.register(LoggingConfig::logResult);
                    }
                }
                break;
            }
        }
    }
}