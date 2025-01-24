package de.z0rdak.yawp;

import de.z0rdak.yawp.api.events.flag.NeoForgeFlagEvent;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.ConfigRegistry;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.NeoForgeConfigHelper;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import net.neoforged.bus.api.EventPriority;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.IExtensionPoint;
import net.neoforged.fml.ModLoadingContext;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.event.RegisterCommandsEvent;
import net.neoforged.neoforge.event.entity.EntityTravelToDimensionEvent;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.server.ServerStartingEvent;

import static de.z0rdak.yawp.handler.YawpEventHandler.removeInvolvedEntities;

@Mod(Constants.MOD_ID)
public class YetAnotherWorldProtector implements YAWPModInitializer {
    
    private static IEventBus yawpEventBus;
    public YetAnotherWorldProtector(IEventBus modEventBus) {
        yawpEventBus = modEventBus;
        yawpEventBus.addListener((FMLCommonSetupEvent event) -> registerConfig());
        YAWPCommon.init();

        initServerInstance();
        loadRegionData();
        addDimKeyOnPlayerLogin();
        addDimKeyOnDimensionChange();
        registerCommands();

        NeoForge.EVENT_BUS.register(YetAnotherWorldProtector.class);
    }

    @Override
    public void registerConfig() {
        ((NeoForgeConfigHelper)Services.CONFIG_REGISTRY).setEventBus(yawpEventBus);
        Services.CONFIG_REGISTRY.register();
    }

    @SubscribeEvent
    public static void onAddFlag(NeoForgeFlagEvent.AddFlagEvent event) {
        if (event.getFlag().getName().contains("spawning") && Services.FLAG_CONFIG.removeEntitiesEnabled()) {
            removeInvolvedEntities(event.getSrc(), event.getRegion(), RegionFlag.fromId(event.getFlag().getName()));
        }
    }
    
    @Override
    public void registerCommands() {
        NeoForge.EVENT_BUS.addListener(this::registerCommandsForge);
    }

    private void registerCommandsForge(RegisterCommandsEvent event) {
        CommandRegistry.registerCommands(event.getDispatcher(), event.getBuildContext(), event.getCommandSelection());
    }

    @Override
    public void initServerInstance() {
        NeoForge.EVENT_BUS.addListener(this::initServerInstanceForge);
    }

    public void initServerInstanceForge(ServerStartingEvent event) {
        RegionDataManager.initServerInstance(event.getServer());
    }

    @Override
    public void loadRegionData() {
        NeoForge.EVENT_BUS.addListener(this::loadRegionDataForge);
    }

    private void loadRegionDataForge(ServerStartingEvent event) {
        MinecraftServer server = event.getServer();
        server.getAllLevels().forEach(level -> {
            if (level.dimension().equals(ServerLevel.OVERWORLD)) {
                RegionDataManager.loadRegionDataForWorld(server, level);
            }
        });
    }

    @Override
    public void addDimKeyOnPlayerLogin() {
        NeoForge.EVENT_BUS.addListener(this::addDimKeyOnPlayerLoginForge);
    }

    private void addDimKeyOnPlayerLoginForge(PlayerEvent.PlayerLoggedInEvent event) {
        RegionDataManager.addDimKeyOnPlayerLogin(event.getEntity(), event.getEntity().level());
    }

    @Override
    public void addDimKeyOnDimensionChange() {
        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, this::addDimKeyOnDimensionChangeForge);
    }

    private void addDimKeyOnDimensionChangeForge(EntityTravelToDimensionEvent event) {
        if (event.getEntity() instanceof Player && event.getEntity().getServer() != null) {
            ServerLevel level = event.getEntity().getServer().getLevel(event.getDimension());
            RegionDataManager.addDimKeyOnDimensionChange((Player) event.getEntity(), event.getEntity().level(), level);
        }
    }
}
