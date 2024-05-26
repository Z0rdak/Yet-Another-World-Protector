package de.z0rdak.yawp.config;

import de.z0rdak.yawp.YetAnotherWorldProtector;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.ModList;
import net.neoforged.fml.ModLoadingContext;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.fml.event.config.ModConfigEvent;

import static de.z0rdak.yawp.YetAnotherWorldProtector.MODID;

public final class ConfigRegistry {

    private ConfigRegistry() {
    }

    public static void register(IEventBus modEventBus) {
        modEventBus.addListener(ConfigRegistry::onConfigLoading);
        modEventBus.addListener(ConfigRegistry::onConfigReloading);
        ModLoadingContext modLoadingContext = ModLoadingContext.get();
        modLoadingContext.getActiveContainer().registerConfig(ModConfig.Type.SERVER, CommandPermissionConfig.CONFIG_SPEC, CommandPermissionConfig.CONFIG_NAME);
        modLoadingContext.getActiveContainer().registerConfig(ModConfig.Type.SERVER, FlagConfig.CONFIG_SPEC, FlagConfig.CONFIG_NAME);
        modLoadingContext.getActiveContainer().registerConfig(ModConfig.Type.SERVER, RegionConfig.CONFIG_SPEC, RegionConfig.CONFIG_NAME);
    }

    @SubscribeEvent
    public static void onConfigLoading(ModConfigEvent.Loading event) {
        if (event.getConfig().getModId().equals(MODID)) {
            switch (event.getConfig().getFileName()) {
                case CommandPermissionConfig.CONFIG_NAME: {
                    CommandPermissionConfig.BASE_CMD = CommandPermissionConfig.getBaseCmd();
                    if (ModList.get().isLoaded("journeymap")) {
                        CommandPermissionConfig.BASE_CMD = CommandPermissionConfig.getBaseCmdAlt();
                        YetAnotherWorldProtector.LOGGER.info("Detected JourneyMap to be loaded beside YAWP.");
                    }
                    YetAnotherWorldProtector.LOGGER.info("Setting YAWP base command to '/" + CommandPermissionConfig.BASE_CMD + "'");
                    int numOfUuidsWithPermission = CommandPermissionConfig.UUIDsWithPermission().size();
                    String uuidsWithPermission = (numOfUuidsWithPermission > 0
                            ? ": " + String.join(", ", CommandPermissionConfig.UUIDsWithPermission())
                            : "");
                    YetAnotherWorldProtector.LOGGER.info(numOfUuidsWithPermission + " UUID(s) with permission read from config" + uuidsWithPermission);
                    CommandRegistry.register(CommandPermissionConfig.BASE_CMD);
                }
                break;
                case RegionConfig.CONFIG_NAME: {
                    int numLocalDefaultFlags = RegionConfig.getDefaultFlags().size();
                    String loadedLocalFlags = (numLocalDefaultFlags > 0
                            ? ": " + String.join(", ", RegionConfig.getDefaultFlags())
                            : "");
                    YetAnotherWorldProtector.LOGGER.info(numLocalDefaultFlags + " default flag(s) for Local Regions read from config" + loadedLocalFlags);

                    int numDimDefaultFlags = RegionConfig.getDefaultDimFlags().size();
                    String loadedDimFlags = (numDimDefaultFlags > 0
                            ? ": " + String.join(", ", RegionConfig.getDefaultDimFlags())
                            : "");
                    YetAnotherWorldProtector.LOGGER.info(numDimDefaultFlags + " default flag(s) for Dimensional Regions read from config" + loadedDimFlags);
                }
                break;
                case FlagConfig.CONFIG_NAME: {
                    int numBreakEntityEntries = FlagConfig.getCoveredBlockEntities().size();
                    String loadedBreakEntities = (numBreakEntityEntries > 0
                            ? ": " + String.join(", ", FlagConfig.getCoveredBlockEntities())
                            : "");
                    YetAnotherWorldProtector.LOGGER.info(numBreakEntityEntries + " Block Entity entries read from config" + loadedBreakEntities);

                    int numBreakEntityTagEntries = FlagConfig.getCoveredBlockEntityTags().size();
                    String loadedBreakEntityTags = (numBreakEntityTagEntries > 0
                            ? ": " + String.join(", ", FlagConfig.getCoveredBlockEntityTags())
                            : "");
                    YetAnotherWorldProtector.LOGGER.info(numBreakEntityTagEntries + " Block Entity tag entries read from config" + loadedBreakEntityTags);
                }
                break;
            }
        }
    }

    @SubscribeEvent
    public static void onConfigReloading(ModConfigEvent.Reloading event) {
        if (event.getConfig().getModId().equals(MODID)) {
            YetAnotherWorldProtector.LOGGER.info("Reloaded: '" + event.getConfig().getFileName() + "'");
        }
    }

}
