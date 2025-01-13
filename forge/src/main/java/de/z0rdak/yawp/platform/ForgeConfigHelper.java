package de.z0rdak.yawp.platform;

import de.z0rdak.yawp.api.events.region.ForgeFlagCheckEvent;
import de.z0rdak.yawp.api.events.region.ForgeFlagCheckResult;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.ConfigRegistry;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.LoggingConfig;
import de.z0rdak.yawp.config.server.PermissionConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.platform.services.IConfigHelper;
import fuzs.forgeconfigapiport.forge.api.neoforge.v4.NeoForgeConfigRegistry;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.event.config.ModConfigEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.neoforged.neoforge.common.ModConfigSpec;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

public class ForgeConfigHelper implements IConfigHelper {
    
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
        FMLJavaModLoadingContext.get().getModEventBus().addListener(ForgeConfigHelper::onConfigLoading);
        FMLJavaModLoadingContext.get().getModEventBus().addListener(ForgeConfigHelper::onConfigReloading);

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
                    MinecraftForge.EVENT_BUS.addListener(
                            (ForgeFlagCheckEvent e) -> LoggingConfig.logCheck(ForgeFlagCheckEvent.asNonEvent(e)));
                }
                if (LoggingConfig.shouldLogFlagCheckResults()) {
                    MinecraftForge.EVENT_BUS.addListener(
                            (ForgeFlagCheckResult e) -> LoggingConfig.logResult(ForgeFlagCheckResult.asNonEvent(e)));
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


