package de.z0rdak.yawp;

import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.server.CommandPermissionConfig;
import de.z0rdak.yawp.config.server.FlagConfig;
import de.z0rdak.yawp.config.server.RegionConfig;
import de.z0rdak.yawp.managers.data.player.PlayerTrackingManager;
import de.z0rdak.yawp.managers.data.region.RegionDataManager;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.IExtensionPoint;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.event.config.ModConfigEvent;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.network.NetworkConstants;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(YetAnotherWorldProtector.MODID)
public class YetAnotherWorldProtector
{
    public static final String MODID = "yawp";
    public static final String MODID_LONG = "Yet Another World Protector";
    public static final Logger LOGGER = LogManager.getLogger();


    public YetAnotherWorldProtector() {
        DistExecutor.unsafeRunWhenOn(Dist.DEDICATED_SERVER, () -> () -> {
            FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);
            FMLJavaModLoadingContext.get().getModEventBus().addListener(this::onConfigLoading);
            FMLJavaModLoadingContext.get().getModEventBus().addListener(this::onConfigReloading);

            ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, CommandPermissionConfig.CONFIG_SPEC, CommandPermissionConfig.CONFIG_NAME);
            ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, FlagConfig.CONFIG_SPEC, MODID + "-flags.toml");
            ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, RegionConfig.CONFIG_SPEC, MODID + "-region-defaults.toml");

            MinecraftForge.EVENT_BUS.register(this);
        });
        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
            LOGGER.info("You are loading " + MODID_LONG + " on a client. " + MODID_LONG + " is a server only mod!");
        });

        //Make sure the mod being absent on the other network side does not cause the client to display the server as incompatible
        ModLoadingContext.get().registerExtensionPoint(IExtensionPoint.DisplayTest.class, () -> new IExtensionPoint.DisplayTest(() -> NetworkConstants.IGNORESERVERONLY, (s, b) -> true));
    }

    private void setup(final FMLCommonSetupEvent event) {
        if (ModList.get().isLoaded("JourneyMap")) {
            YetAnotherWorldProtector.LOGGER.warn("Detected JourneyMap mod. Setting base command to '" + CommandPermissionConfig.WP_CMDS[2] + "'");
            CommandPermissionConfig.BASE_CMD = CommandPermissionConfig.WP_CMDS[2];
        }
    }

    @SubscribeEvent
    public void onServerStarting(ServerStartingEvent event) {
        RegionDataManager.loadRegionData(event);
        PlayerTrackingManager.loadPlayerData(event);
    }

    @SubscribeEvent
    public void onServerStartingRegisterCommands(RegisterCommandsEvent event) {
        CommandRegistry.init(event.getDispatcher());
    }

    @SubscribeEvent
    public void onConfigLoading(ModConfigEvent.Loading event) {
        if (event.getConfig().getFileName().equals(CommandPermissionConfig.CONFIG_NAME)){
            CommandPermissionConfig.setBaseCmd();
        }
    }

    @SubscribeEvent
    public void onConfigReloading(ModConfigEvent.Reloading event) {
        // reset player uuids?
    }
}
