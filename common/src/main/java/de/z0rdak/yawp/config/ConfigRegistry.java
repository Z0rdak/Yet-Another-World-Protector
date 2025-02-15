package de.z0rdak.yawp.config;

import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.config.server.PermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static de.z0rdak.yawp.config.server.FlagConfig.FLAG_CONFIG_LOGGER;
import static de.z0rdak.yawp.config.server.LoggingConfig.LOGGING_CONFIG_LOGGER;
import static de.z0rdak.yawp.config.server.PermissionConfig.PERMISSION_CONFIG_LOGGER;
import static de.z0rdak.yawp.config.server.RegionConfig.REGION_CONFIG_LOGGER;
import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public final class ConfigRegistry {

    public static final Logger CONFIG_LOGGER = LogManager.getLogger(MOD_ID.toUpperCase() + "-Config");

    private ConfigRegistry() {
    }

    public static void onModLoaded(String configName, Runnable registerHandler) {
        switch (configName) {
            case PermissionConfig.CONFIG_NAME: {
                int numOfUuidsWithPermission = PermissionConfig.UUIDsWithPermission().size();
                String uuidsWithPermission = (numOfUuidsWithPermission > 0
                        ? ": " + String.join(", ", PermissionConfig.UUIDsWithPermission())
                        : "");
                PERMISSION_CONFIG_LOGGER.info("{} UUID(s) with permission read from config{}", numOfUuidsWithPermission, uuidsWithPermission);
                PERMISSION_CONFIG_LOGGER.info("Required OP level to use commands: {}", PermissionConfig.getRequiredOpLevel());
                PERMISSION_CONFIG_LOGGER.info("Command block execution: {}", PermissionConfig.isCommandBlockExecutionAllowed() ? "enabled" : "disabled");
                PERMISSION_CONFIG_LOGGER.info("Region info commands for all players: {}", PermissionConfig.isReadOnlyAllowed() ? "enabled" : "disabled");
                PERMISSION_CONFIG_LOGGER.info("Region-Hierarchy ownership: {}", PermissionConfig.isHierarchyOwnershipEnabled() ? "enabled" : "disabled");
                PERMISSION_CONFIG_LOGGER.info("OP bypassing flags: {}", PermissionConfig.byPassFlagAllowed() ? "enabled" : "disabled");
                PERMISSION_CONFIG_LOGGER.info("Commands for non OPs: {}", PermissionConfig.isCmdEnabledForNonOp() ? "enabled" : "disabled");
                PERMISSION_CONFIG_LOGGER.info("RegionMarker creation: {}", PermissionConfig.isMarkerCreationEnabled() ? "enabled" : "disabled");
                PERMISSION_CONFIG_LOGGER.info("Region teleportation: {}", PermissionConfig.allowRegionTp() ? "enabled" : "disabled");
            }
            break;
            case RegionConfig.CONFIG_NAME: {
                int numLocalDefaultFlags = RegionConfig.getDefaultFlags().size();
                String loadedLocalFlags = (numLocalDefaultFlags > 0
                        ? ": " + String.join(", ", RegionConfig.getDefaultFlags())
                        : "");
                REGION_CONFIG_LOGGER.info("{} default flag(s) for Local Regions read from config{}", numLocalDefaultFlags, loadedLocalFlags);

                int numDimDefaultFlags = RegionConfig.getDefaultDimFlags().size();
                String loadedDimFlags = (numDimDefaultFlags > 0
                        ? ": " + String.join(", ", RegionConfig.getDefaultDimFlags())
                        : "");
                REGION_CONFIG_LOGGER.info("{} default flag(s) for Dimensional Regions read from config{}", numDimDefaultFlags, loadedDimFlags);
                REGION_CONFIG_LOGGER.info("Enabling newly created Dimensional Regions: {}", RegionConfig.shouldActivateNewDimRegion() ? "enabled" : "disabled");
            }
            break;
            case FlagConfig.CONFIG_NAME: {
                int numBreakEntityEntries = FlagConfig.getCoveredBlockEntities().size();
                String loadedBreakEntities = (numBreakEntityEntries > 0
                        ? ": " + String.join(", ", FlagConfig.getCoveredBlockEntities())
                        : "");
                FLAG_CONFIG_LOGGER.info("{} Block Entity entries read from config{}", numBreakEntityEntries, loadedBreakEntities);

                int numBreakEntityTagEntries = FlagConfig.getCoveredBlockEntityTags().size();
                String loadedBreakEntityTags = (numBreakEntityTagEntries > 0
                        ? ": " + String.join(", ", FlagConfig.getCoveredBlockEntityTags())
                        : "");
                FLAG_CONFIG_LOGGER.info("{} Block Entity tag entries read from config{}", numBreakEntityTagEntries, loadedBreakEntityTags);
                FLAG_CONFIG_LOGGER.info("Remove entities when enabling spawning flags: {}", FlagConfig.removeEntitiesEnabled() ? "enabled" : "disabled");
                break;
            }
            case LoggingConfig.CONFIG_NAME: {
                LOGGING_CONFIG_LOGGER.info("Logging flag checks: {}", LoggingConfig.shouldLogFlagChecks());
                LOGGING_CONFIG_LOGGER.info("Logging flag check results: {}", LoggingConfig.shouldLogFlagCheckResults());
                LOGGING_CONFIG_LOGGER.info("Logging empty flag results: {}", LoggingConfig.shouldLogEmptyResults());
                LOGGING_CONFIG_LOGGER.info("Logging flag categories: [{}]", String.join(",", LoggingConfig.getFlagCategories()));
                LOGGING_CONFIG_LOGGER.info("Logging flag results: [{}]", String.join(",", LoggingConfig.getResultValuesToLog()));
                LOGGING_CONFIG_LOGGER.info("Logging flags: [{}]", String.join(",", LoggingConfig.getFlagsToLog()));
                // LOGGING_CONFIG_LOGGER.info("Logging detailed player flag checks: {}", LoggingConfig.shouldLogDetailedPlayerFlags());
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