package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.api.events.region.FabricRegionEvents;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.ConfigRegistry;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.config.server.PermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.platform.services.IConfigHelper;
import fuzs.forgeconfigapiport.fabric.api.neoforge.v4.NeoForgeConfigRegistry;
import fuzs.forgeconfigapiport.fabric.api.neoforge.v4.NeoForgeModConfigEvents;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.neoforge.common.ModConfigSpec;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public class FabricConfigHelper implements IConfigHelper {
    
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
        NeoForgeConfigRegistry.INSTANCE.register(MOD_ID, type, spec, configName);
    }

    public void register() {
        NeoForgeModConfigEvents.loading(MOD_ID).register(FabricConfigHelper::onModLoading);
        NeoForgeModConfigEvents.reloading(MOD_ID).register(FabricConfigHelper::onModReloading);
        // registering configuration
        Services.CONFIG_REGISTRY.registerServerConfig(PermissionConfig.CONFIG_SPEC, PermissionConfig.CONFIG_NAME);
        Services.CONFIG_REGISTRY.registerServerConfig(FlagConfig.CONFIG_SPEC, FlagConfig.CONFIG_NAME);
        Services.CONFIG_REGISTRY.registerServerConfig(RegionConfig.CONFIG_SPEC, RegionConfig.CONFIG_NAME);
        Services.CONFIG_REGISTRY.registerServerConfig(LoggingConfig.CONFIG_SPEC, LoggingConfig.CONFIG_NAME);
    }

    private static void onModReloading(ModConfig modConfig) {
        if (modConfig.getModId().equals(MOD_ID)) {
            ConfigRegistry.onModReloaded(modConfig.getFileName(), () -> {});
        }
    }    
     
    private static void onModLoading(ModConfig modConfig) {
        if (modConfig.getModId().equals(MOD_ID)) {
            Runnable registerHandler = () -> {
                if (LoggingConfig.shouldLogFlagChecks()) {
                    FabricRegionEvents.CHECK_FLAG.register(LoggingConfig::logCheck);
                }
                if (LoggingConfig.shouldLogFlagCheckResults()) {
                    FabricRegionEvents.FLAG_RESULT.register(LoggingConfig::logResult);
                }
            };
            ConfigRegistry.onModLoaded(modConfig.getFileName(), registerHandler);
        }
    }
}
