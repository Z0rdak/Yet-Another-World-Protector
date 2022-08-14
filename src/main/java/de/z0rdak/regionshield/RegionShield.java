package de.z0rdak.regionshield;

import de.z0rdak.regionshield.commands.*;
import de.z0rdak.regionshield.config.*;
import de.z0rdak.regionshield.managers.data.player.PlayerTrackingManager;
import de.z0rdak.regionshield.managers.data.region.RegionDataManager;
import net.minecraft.item.Item;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.eventbus.api.SubscribeEvent;
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
    public static final Logger LOGGER = LogManager.getLogger();


    public RegionShield() {
        // Register the setup method for modloading
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::setup);
        // Register the enqueueIMC method for modloading
        ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, ServerConfigBuilder.CONFIG_SPEC, MODID + "-common.toml" );
        ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, FlagConfig.CONFIG_SPEC, MODID + "-flags.toml" );
        ModLoadingContext.get().registerConfig(ModConfig.Type.SERVER, ServerRegionConfigBuilder.CONFIG_SPEC, MODID + "-region-defaults.toml" );

        // Register ourselves for server and other game events we are interested in
        MinecraftForge.EVENT_BUS.register(this);
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

    @Mod.EventBusSubscriber(bus=Mod.EventBusSubscriber.Bus.MOD)
    public static class RegistryEvents {
        @SubscribeEvent
        public static void onItemRegistry(final RegistryEvent.Register<Item> itemRegistryEvent){
            // register a new item here
            // TODO: patchouli
        }
    }
}
