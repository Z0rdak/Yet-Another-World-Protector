package de.z0rdak.yawp;

import de.z0rdak.yawp.api.events.flag.ForgeFlagEvent;
import de.z0rdak.yawp.api.visualization.VisualizationManager;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.config.ConfigRegistry;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.data.PlayerManager;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.event.entity.EntityTravelToDimensionEvent;
import net.minecraftforge.event.entity.player.PlayerEvent;
import net.minecraftforge.event.level.LevelEvent;
import net.minecraftforge.event.server.ServerAboutToStartEvent;
import net.minecraftforge.event.server.ServerStartingEvent;
import net.minecraftforge.event.server.ServerStoppingEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.SubscribeEvent;
import net.minecraftforge.fml.IExtensionPoint;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.network.NetworkConstants;

import static de.z0rdak.yawp.handler.YawpEventHandler.removeInvolvedEntities;

@Mod(Constants.MOD_ID)
public class YetAnotherWorldProtector implements YAWPModInitializer {

    public YetAnotherWorldProtector() {
        YAWPCommon.init();

        registerConfig();
        setupRegionDataLifecycleHooks();
        registerCommands();

        //Make sure the mod being absent on the other network side does not cause the client to display the server as incompatible
        ModLoadingContext.get().registerExtensionPoint(IExtensionPoint.DisplayTest.class, () -> new IExtensionPoint.DisplayTest(() -> NetworkConstants.IGNORESERVERONLY, (s, b) -> true));
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
    public void setupRegionDataLifecycleHooks() {
        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true,
                (ServerAboutToStartEvent startEvent) -> RegionDataManager.onServerStarting(startEvent.getServer()));
        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true,
                (ServerAboutToStartEvent startEvent) -> PlayerManager.onServerStart(startEvent.getServer()));
        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true,
                (ServerAboutToStartEvent startEvent) -> VisualizationManager.initServerInstance(startEvent.getServer()));
        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true,
                (LevelEvent.Load event) -> {
                    if (event.getLevel() instanceof ServerLevel serverLevel) {
                        if (serverLevel.dimension().equals(ServerLevel.OVERWORLD)) {
                            RegionDataManager.loadLevelListData(serverLevel.getServer());
                        }
                        RegionDataManager.worldLoad(serverLevel.getServer(), serverLevel);
                    }
                });
        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true,
                (PlayerEvent.PlayerLoggedInEvent event) -> {
                    if (event.getEntity().getCommandSenderWorld() instanceof ServerLevel serverLevel) {
                        RegionDataManager.initLevelDataOnLogin(event.getEntity(), serverLevel);
                    }
                });
        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true,
                (EntityTravelToDimensionEvent event) -> {
                    if (event.getEntity() instanceof Player player && event.getEntity().getServer() != null) {
                        Level targetLevel = event.getEntity().getServer().getLevel(event.getDimension());
                        RegionDataManager.initLevelDataOnChangeWorld(player, player.level(), targetLevel);
                    }
                });
        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true,
                (ServerStoppingEvent stoppingEvent) -> RegionDataManager.saveOnStop(stoppingEvent.getServer()));

        MinecraftForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true,
                (LevelEvent.Unload unloadEvent) -> {
                    if (unloadEvent.getLevel() instanceof ServerLevel serverLevel) {
                        RegionDataManager.saveOnUnload(serverLevel.getServer(), serverLevel);
                    }
                });
    }

    @Override
    public void registerConfig() {
        FMLJavaModLoadingContext.get().getModEventBus().addListener(this::registerConfigForge);
    }

    public void registerConfigForge(FMLCommonSetupEvent event) {
        ConfigRegistry.register();
    }
}
