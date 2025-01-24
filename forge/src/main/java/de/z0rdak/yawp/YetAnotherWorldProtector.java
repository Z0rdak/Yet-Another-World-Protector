package de.z0rdak.yawp;

import de.z0rdak.yawp.api.events.flag.ForgeFlagEvent;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.ConfigRegistry;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.ForgeConfigHelper;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.event.entity.EntityTravelToDimensionEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.IExtensionPoint;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;

import static de.z0rdak.yawp.handler.YawpEventHandler.removeInvolvedEntities;
import static net.minecraftforge.fml.IExtensionPoint.DisplayTest.IGNORESERVERONLY;

@Mod(Constants.MOD_ID)
public class YetAnotherWorldProtector implements YAWPModInitializer {

    public YetAnotherWorldProtector() {
        YAWPCommon.init();

        registerConfig();
        initServerInstance();
        loadRegionData();
        addDimKeyOnPlayerLogin();
        addDimKeyOnDimensionChange();
        registerCommands();

        ModLoadingContext modLoadingContext = new ModLoadingContext();
        //Make sure the mod being absent on the other network side does not cause the client to display the server as incompatible
        modLoadingContext.registerExtensionPoint(IExtensionPoint.DisplayTest.class, () -> new IExtensionPoint.DisplayTest(() -> IGNORESERVERONLY, (s, b) -> true));
        MinecraftForge.EVENT_BUS.register(YetAnotherWorldProtector.class);
    }

    @SubscribeEvent
    public static void onAddFlag(ForgeFlagEvent.AddFlagEvent event) {
        if (event.getFlag().getName().contains("spawning") && Services.FLAG_CONFIG.removeEntitiesEnabled()) {
            removeInvolvedEntities(event.getSrc(), event.getRegion(), RegionFlag.fromId(event.getFlag().getName()));
        }
    }
    
    @Override
    public void registerCommands() {
        MinecraftForge.EVENT_BUS.addListener(this::registerCommandsForge);
    }

    private void registerCommandsForge(RegisterCommandsEvent event) {
        CommandRegistry.registerCommands(event.getDispatcher(), event.getBuildContext(), event.getCommandSelection());
    }

    @Override
    public void initServerInstance() {
        MinecraftForge.EVENT_BUS.addListener(this::initServerInstanceForge);
    }

    public void initServerInstanceForge(ServerStartingEvent event) {
        RegionDataManager.initServerInstance(event.getServer());
    }

    @Override
    public void loadRegionData() {
        MinecraftForge.EVENT_BUS.addListener(this::loadRegionDataForge);
    }

    private void loadRegionDataForge(ServerStartingEvent event) {
        MinecraftServer server = event.getServer();
        server.getAllLevels().forEach(level -> {
            if (level.dimension().location().equals(ServerLevel.OVERWORLD.location())) {
                RegionDataManager.loadRegionDataForWorld(server, level);
            }
        });
    }

    @Override
    public void addDimKeyOnPlayerLogin() {
        MinecraftForge.EVENT_BUS.addListener(this::addDimKeyOnPlayerLoginForge);
    }

    private void addDimKeyOnPlayerLoginForge(PlayerEvent.PlayerLoggedInEvent event) {
        RegionDataManager.addDimKeyOnPlayerLogin(event.getEntity(), event.getEntity().level());
    }

    @Override
    public void addDimKeyOnDimensionChange() {
        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, this::addDimKeyOnDimensionChangeForge);
    }

    private void addDimKeyOnDimensionChangeForge(EntityTravelToDimensionEvent event) {
        if (event.getEntity() instanceof Player && event.getEntity().getServer() != null) {
            ServerLevel level = event.getEntity().getServer().getLevel(event.getDimension());
            RegionDataManager.addDimKeyOnDimensionChange((Player) event.getEntity(), event.getEntity().level(), level);
        }
    }

    @Override
    public void registerConfig() {
        IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();
        modEventBus.addListener((FMLCommonSetupEvent event) ->  Services.CONFIG_REGISTRY.register());
    }
}
