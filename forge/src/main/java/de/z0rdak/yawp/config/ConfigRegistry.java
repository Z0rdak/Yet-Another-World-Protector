package de.z0rdak.yawp.config;

import de.z0rdak.yawp.config.server.*;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.handler.YawpEventHandler;
import de.z0rdak.yawp.platform.Services;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.event.config.ModConfigEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static de.z0rdak.yawp.config.server.FeatureConfig.FEATURE_CONFIG_LOGGER;
import static de.z0rdak.yawp.config.server.FlagConfig.FLAG_CONFIG_LOGGER;
import static de.z0rdak.yawp.config.server.LoggingConfig.LOGGING_CONFIG_LOGGER;
import static de.z0rdak.yawp.config.server.PermissionConfig.PERMISSION_CONFIG_LOGGER;
import static de.z0rdak.yawp.config.server.RegionConfig.REGION_CONFIG_LOGGER;

public final class ConfigRegistry {

    public static final Logger CONFIG_LOGGER = LogManager.getLogger(Constants.MOD_ID.toUpperCase() + "-Config-Event");

    private ConfigRegistry() {
    }

    public static void register() {
        FMLJavaModLoadingContext.get().getModEventBus().addListener(ConfigRegistry::onConfigLoading);
        FMLJavaModLoadingContext.get().getModEventBus().addListener(ConfigRegistry::onConfigReloading);

        ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, PermissionConfig.CONFIG_SPEC, PermissionConfig.CONFIG_NAME);
        ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, FlagConfig.CONFIG_SPEC, FlagConfig.CONFIG_NAME);
        ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, RegionConfig.CONFIG_SPEC, RegionConfig.CONFIG_NAME);
        ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, LoggingConfig.CONFIG_SPEC, LoggingConfig.CONFIG_NAME);
        ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, FeatureConfig.CONFIG_SPEC, FeatureConfig.CONFIG_NAME);
    }

    @SubscribeEvent
    public static void onConfigLoading(ModConfigEvent.Loading event) {
        if (event.getConfig().getModId().equals(Constants.MOD_ID)) {
            switch (event.getConfig().getFileName()) {
                case FeatureConfig.CONFIG_NAME: {
                    var enablePlayerTracker = FeatureConfig.enablePlayerTracker();
                    FEATURE_CONFIG_LOGGER.info("Player tracking feature: {}", enablePlayerTracker ? "enabled" : "disabled" );

                    if (enablePlayerTracker) {
                        Services.FEATURE_MANAGER.enablePlayerTracker();
                        YawpEventHandler.enableRegionSpatialCache();
                        YawpEventHandler.enablePlayerRegionMessages();
                    }
                }
                break;
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
                    LOGGING_CONFIG_LOGGER.info("Logging flag categories: [{}]", String.join(",", LoggingConfig.getFlagTags()));
                    LOGGING_CONFIG_LOGGER.info("Logging flag results: [{}]", String.join(",", LoggingConfig.getResultValuesToLog()));
                    LOGGING_CONFIG_LOGGER.info("Logging flags: [{}]", String.join(",", LoggingConfig.getFlagsToLog()));
                    // LOGGING_CONFIG_LOGGER.info("Logging detailed player flag checks: {}", LoggingConfig.shouldLogDetailedPlayerFlags());
                    LOGGING_CONFIG_LOGGER.info("Logging events: [{}]", LoggingConfig.shouldLogEvents());

                    if (LoggingConfig.shouldLogEvents()) {
                        YawpEventHandler.enableDetailedEventLogger();
                    }
                    if (LoggingConfig.shouldLogFlagChecks()) {
                        MinecraftForge.EVENT_BUS.addListener(LoggingConfig::logCheck);
                    }
                    if (LoggingConfig.shouldLogFlagCheckResults()) {
                        MinecraftForge.EVENT_BUS.addListener(LoggingConfig::logResult);
                    }
                }
                break;
            }
        }
    }

    @SubscribeEvent
    public static void onConfigReloading(ModConfigEvent.Reloading event) {
        if (event.getConfig().getModId().equals(Constants.MOD_ID)) {
            CONFIG_LOGGER.info("Reloaded: '{}'", event.getConfig().getFileName());
        }
    }

}
