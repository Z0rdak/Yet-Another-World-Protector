package de.z0rdak.yawp;

import de.z0rdak.yawp.api.events.flag.NeoForgeFlagEvent;
import de.z0rdak.yawp.api.visualization.VisualizationManager;
import de.z0rdak.yawp.commands.CommandRegistry;
import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.flag.RegionFlag;
import de.z0rdak.yawp.data.PlayerManager;
import de.z0rdak.yawp.data.region.RegionDataManager;
import de.z0rdak.yawp.platform.NeoForgeConfigHelper;
import de.z0rdak.yawp.platform.Services;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.neoforged.bus.api.EventPriority;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.event.RegisterCommandsEvent;
import net.neoforged.neoforge.event.entity.EntityTravelToDimensionEvent;
import net.neoforged.neoforge.event.entity.player.PlayerEvent;
import net.neoforged.neoforge.event.level.LevelEvent;
import net.neoforged.neoforge.event.server.ServerAboutToStartEvent;
import net.neoforged.neoforge.event.server.ServerStartingEvent;
import net.neoforged.neoforge.event.server.ServerStoppingEvent;

import static de.z0rdak.yawp.handler.YawpEventHandler.removeInvolvedEntities;

@Mod(Constants.MOD_ID)
public class YetAnotherWorldProtector implements YAWPModInitializer {

    private static IEventBus yawpEventBus;

    public YetAnotherWorldProtector(IEventBus modEventBus) {
        yawpEventBus = modEventBus;
        yawpEventBus.addListener((FMLCommonSetupEvent event) -> registerConfig());
        YAWPCommon.init();

        setupRegionDataLifecycleHooks();
        registerCommands();

        NeoForge.EVENT_BUS.register(YetAnotherWorldProtector.class);
    }

    @SubscribeEvent
    public static void onAddFlag(NeoForgeFlagEvent.AddFlagEvent event) {
        if (event.getFlag().getName().contains("spawning") && Services.FLAG_CONFIG.removeEntitiesEnabled()) {
            removeInvolvedEntities(event.getSrc(), event.getRegion(), RegionFlag.fromId(event.getFlag().getName()));
        }
    }

    @Override
    public void registerCommands() {
        NeoForge.EVENT_BUS.addListener((RegisterCommandsEvent event) -> CommandRegistry.registerCommands(event.getDispatcher(), event.getBuildContext(), event.getCommandSelection()));
    }

    @Override
    public void setupRegionDataLifecycleHooks() {
        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, (ServerAboutToStartEvent startEvent) -> RegionDataManager.onServerStarting(startEvent.getServer()));
        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, (ServerAboutToStartEvent startEvent) -> PlayerManager.onServerStart(startEvent.getServer()));
        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, (ServerAboutToStartEvent startEvent) -> VisualizationManager.initServerInstance(startEvent.getServer()));

        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, (LevelEvent.Load event) -> {
            if (event.getLevel() instanceof ServerLevel serverLevel)
                RegionDataManager.worldLoad(serverLevel.getServer(), serverLevel);
        });
        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, (PlayerEvent.PlayerLoggedInEvent event) -> {
            if (event.getEntity().getCommandSenderWorld() instanceof ServerLevel serverLevel) {
                RegionDataManager.initLevelDataOnLogin(event.getEntity(), serverLevel);
            }
        });
        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, (LevelEvent.Save saveEvent) -> {
            if (saveEvent.getLevel() instanceof ServerLevel serverLevel) {
                RegionDataManager.save(serverLevel.getServer(), false, false);
            }
        });
        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, (EntityTravelToDimensionEvent event) -> {
            if (event.getEntity() instanceof Player player && event.getEntity().getServer() != null) {
                Level targetLevel = event.getEntity().getServer().getLevel(event.getDimension());
                RegionDataManager.initLevelDataOnChangeWorld(player, player.level(), targetLevel);
            }
        });
        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, (ServerStoppingEvent stoppingEvent) -> RegionDataManager.saveOnStop(stoppingEvent.getServer()));
        NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, true, (LevelEvent.Unload unloadEvent) -> {
            if (unloadEvent.getLevel() instanceof ServerLevel serverLevel)
                RegionDataManager.saveOnUnload(unloadEvent.getLevel().getServer(), serverLevel);
        });

    }

    @Override
    public void registerConfig() {
        ((NeoForgeConfigHelper) Services.CONFIG_REGISTRY).setEventBus(yawpEventBus);
        Services.CONFIG_REGISTRY.register();
    }
}
