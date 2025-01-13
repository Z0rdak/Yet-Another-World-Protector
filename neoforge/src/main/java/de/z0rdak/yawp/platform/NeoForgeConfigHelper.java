package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.api.events.region.FlagCheckEvent;
import de.z0rdak.yawp.api.events.region.NeoForgeFlagCheckEvent;
import de.z0rdak.yawp.api.events.region.NeoForgeFlagCheckResult;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.ConfigRegistry;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.config.server.PermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.platform.config.NeoForgeLoggingConfigHelper;
import de.z0rdak.yawp.platform.services.IConfigHelper;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.ModLoadingContext;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.fml.event.config.ModConfigEvent;
import net.neoforged.neoforge.common.ModConfigSpec;
import net.neoforged.neoforge.common.NeoForge;

public class NeoForgeConfigHelper implements IConfigHelper {

    private ModContainer modContainer;

    public void setModContainer(ModContainer modContainer) {
        this.modContainer = modContainer;
    }
    
    @Override
    public void registerServerConfig(ModConfigSpec spec, String configName) {
        registerModConfig(spec, ModConfig.Type.SERVER, configName);
    }

    @Override
    public void registerClientConfig(ModConfigSpec spec, String configName) {
        registerModConfig(spec, ModConfig.Type.CLIENT, configName);
    }

    @Override
    public void registerCommonConfig(ModConfigSpec spec, String configName) {
        registerModConfig(spec, ModConfig.Type.COMMON, configName);
    }
    
    private void registerModConfig(ModConfigSpec spec, ModConfig.Type type, String configName) {
        modContainer.registerConfig(type, spec, configName);
    }

    private IEventBus modEventBus;
    
    public void setEventBus(IEventBus modEventBus) {
        this.modEventBus = modEventBus;
    }
    
    public void register() {
        modEventBus.addListener(NeoForgeConfigHelper::onConfigLoading);
        modEventBus.addListener(NeoForgeConfigHelper::onConfigReloading);
        ModLoadingContext modLoadingContext = ModLoadingContext.get();
        ModContainer activeContainer = modLoadingContext.getActiveContainer();

        ((NeoForgeConfigHelper)Services.CONFIG_REGISTRY).setModContainer(activeContainer);
        Services.CONFIG_REGISTRY.registerServerConfig(PermissionConfig.CONFIG_SPEC, PermissionConfig.CONFIG_NAME);
        Services.CONFIG_REGISTRY.registerServerConfig(FlagConfig.CONFIG_SPEC, FlagConfig.CONFIG_NAME);
        Services.CONFIG_REGISTRY.registerServerConfig(RegionConfig.CONFIG_SPEC, RegionConfig.CONFIG_NAME);
        Services.CONFIG_REGISTRY.registerServerConfig(LoggingConfig.CONFIG_SPEC, LoggingConfig.CONFIG_NAME);
    }

    @SubscribeEvent
    public static void onConfigLoading(ModConfigEvent.Loading event) {
        if (event.getConfig().getModId().equals(Constants.MOD_ID)) {
            Runnable registerHandler = () -> {
                if (LoggingConfig.shouldLogFlagChecks()) {
                    NeoForge.EVENT_BUS.addListener(
                            (NeoForgeFlagCheckEvent e) -> LoggingConfig.logCheck(NeoForgeFlagCheckEvent.asNonEvent(e)));
                }
                if (LoggingConfig.shouldLogFlagCheckResults()) {
                    NeoForge.EVENT_BUS.addListener(
                            (NeoForgeFlagCheckResult e) -> LoggingConfig.logResult(NeoForgeFlagCheckResult.asNonEvent(e)));
                }
            };            
            ConfigRegistry.onModLoaded(event.getConfig().getFileName(), registerHandler);
        }
    }

    @SubscribeEvent
    public static void onConfigReloading(ModConfigEvent.Reloading event) {
        if (event.getConfig().getModId().equals(Constants.MOD_ID)) {
            ConfigRegistry.onModReloaded(event.getConfig().getFileName(), () -> {});
        }
    }
}
