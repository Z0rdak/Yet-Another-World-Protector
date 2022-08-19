package de.z0rdak.regionshield;

import de.z0rdak.regionshield.commands.*;
import de.z0rdak.regionshield.config.*;
import de.z0rdak.regionshield.managers.data.player.PlayerTrackingManager;
import de.z0rdak.regionshield.managers.data.region.RegionDataManager;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.event.server.FMLServerStartingEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(RegionShield.MODID)
public class RegionShield
{
    /**
     * Yet Another Region Protector - YAPR
     * Yet Another Protection Mod - YAPM
     * Yet Another World Protector - YAWP
     */
    public static final String MODID = "regionshield";
    public static final String MODID_SHORT = "rs";

    public static final String MODID_LONG = "RegionShield";
    public static final Logger LOGGER = LogManager.getLogger();


    public RegionShield() {
        DistExecutor.unsafeRunWhenOn(Dist.DEDICATED_SERVER, () -> () -> {
            FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);

            ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, ServerConfigBuilder.CONFIG_SPEC, MODID + "-common.toml" );
            ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, FlagConfig.CONFIG_SPEC, MODID + "-flags.toml" );
            ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, ServerRegionConfigBuilder.CONFIG_SPEC, MODID + "-region-defaults.toml" );

            MinecraftForge.EVENT_BUS.register(this);

        });
        DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
            LOGGER.info("You are loading " + MODID_LONG + " on a client. " + MODID_LONG + " is a server only mod!");
        });

    }

    private void setup(final FMLCommonSetupEvent event) {

    }

    @SubscribeEvent
    public void onServerStarting(FMLServerStartingEvent event) {
        RegionDataManager.loadRegionData(event);
        PlayerTrackingManager.loadPlayerData(event);
    }

    @SubscribeEvent
    public void onServerStarting(RegisterCommandsEvent event) {
        CommandRegionShield.init(event.getDispatcher());
    }

}
