package de.z0rdak.yawp.config;

import de.z0rdak.yawp.api.events.region.FabricRegionEvents;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.config.server.PermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.platform.Services;
import fuzs.forgeconfigapiport.api.config.v2.ForgeConfigRegistry;
import fuzs.forgeconfigapiport.api.config.v2.ModConfigEvents;
import net.minecraftforge.fml.config.ModConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public final class ConfigRegistry {

    public static final Logger CONFIG_LOGGER = LogManager.getLogger(MOD_ID.toUpperCase() + "-Config");

    private ConfigRegistry() {
    }

    public static void register() {

        ModConfigEvents.loading(MOD_ID).register(ConfigRegistry::onModLoading);
        ModConfigEvents.reloading(MOD_ID).register(ConfigRegistry::onModReloading);

        // registering configuration
        ForgeConfigRegistry.INSTANCE.register(MOD_ID, ModConfig.Type.SERVER, PermissionConfig.CONFIG_SPEC, PermissionConfig.CONFIG_NAME);
        ForgeConfigRegistry.INSTANCE.register(MOD_ID, ModConfig.Type.SERVER, FlagConfig.CONFIG_SPEC, FlagConfig.CONFIG_NAME);
        ForgeConfigRegistry.INSTANCE.register(MOD_ID, ModConfig.Type.SERVER, RegionConfig.CONFIG_SPEC, RegionConfig.CONFIG_NAME);
        ForgeConfigRegistry.INSTANCE.register(MOD_ID, ModConfig.Type.SERVER, LoggingConfig.CONFIG_SPEC, LoggingConfig.CONFIG_NAME);
    }

    private static void onModReloading(ModConfig modConfig) {
        if (modConfig.getModId().equals(MOD_ID)) {
            CONFIG_LOGGER.info("Reloaded: '{}'", modConfig.getFileName());
        }
    }

    private static void onModLoading(ModConfig modConfig) {
        if (modConfig.getModId().equals(MOD_ID)) {
            switch (modConfig.getFileName()) {
                case PermissionConfig.CONFIG_NAME: {
                    PermissionConfig.BASE_CMD = PermissionConfig.getBaseCmd();

                    if (Services.PLATFORM.isModLoaded("journeymap")) {
                        PermissionConfig.BASE_CMD = PermissionConfig.getBaseCmdAlt();
                        CONFIG_LOGGER.info("Detected JourneyMap to be loaded beside YAWP.");
                    }
                    CONFIG_LOGGER.info("Setting YAWP base command to '/{}'", PermissionConfig.BASE_CMD);
                    int numOfUuidsWithPermission = PermissionConfig.UUIDsWithPermission().size();
                    String uuidsWithPermission = (numOfUuidsWithPermission > 0
                            ? ": " + String.join(", ", PermissionConfig.UUIDsWithPermission())
                            : "");
                    CONFIG_LOGGER.info("{} UUID(s) with permission read from config{}", numOfUuidsWithPermission, uuidsWithPermission);
                    CommandRegistry.register(PermissionConfig.BASE_CMD);
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
                    CONFIG_LOGGER.info("Logging flag results: [{}]", String.join(",", LoggingConfig.getResultValuesToLog()));
                    CONFIG_LOGGER.info("Logging flags: [{}]", String.join(",", LoggingConfig.getFlagsToLog()));
                    CONFIG_LOGGER.info("Logging empty flag results: {}", LoggingConfig.shouldLogEmptyResults());
                    // CONFIG_LOGGER.info("Logging detailed player flag checks: {}", LoggingConfig.shouldLogDetailedPlayerFlags());

                    if (LoggingConfig.shouldLogFlagChecks()) {
                        FabricRegionEvents.CHECK_FLAG.register(LoggingConfig::logCheck);
                    }
                    if (LoggingConfig.shouldLogFlagCheckResults()) {
                        FabricRegionEvents.FLAG_RESULT.register(LoggingConfig::logResult);
                    }
                }
                break;
            }
        }
    }
}