package de.z0rdak.yawp.config;

import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.config.server.PermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.platform.Services;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public final class ConfigRegistry {

    public static final Logger CONFIG_LOGGER = LogManager.getLogger(MOD_ID.toUpperCase() + "-Config");

    private ConfigRegistry() {
    }

    public static void onModLoaded(String configName, Runnable registerHandler) {
        switch (configName) {
            case PermissionConfig.CONFIG_NAME: {
                PermissionConfig.BASE_CMD = PermissionConfig.getBaseCmd();
                if (Services.PLATFORM.isJourneyMapLoaded()) {
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

                registerHandler.run();
            }
            break;
        }
    }

    public static void onModReloaded(String configName, Runnable reloadHandler) {
        CONFIG_LOGGER.info("Reloaded: '{}'", configName);
        reloadHandler.run();
    }

}