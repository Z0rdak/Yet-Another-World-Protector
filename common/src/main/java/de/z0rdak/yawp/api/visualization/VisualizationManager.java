package de.z0rdak.yawp.api.visualization;

import de.z0rdak.yawp.api.core.IDimensionRegionApi;
import de.z0rdak.yawp.api.core.RegionManager;
import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import de.z0rdak.yawp.core.area.DisplayType;
import de.z0rdak.yawp.core.area.TeleportAnchor;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import de.z0rdak.yawp.core.region.IProtectedRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.Identifier;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Display;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.entity.EntityTypeTest;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.util.*;

import static de.z0rdak.yawp.constants.Constants.MOD_ID;

/*
- TODO: Event for filtering tracking for players? -> separately in next patch (most likely with luckperms)
    -> when tracking other player is added?... kill entities and add tracking for other player or how doesi t work?
-TODO: Show teleport anchors
-TODO: Remove entities on server-stop
-Note: Remove on start not possible due to events fired by entities not yet loaded?
-TODO: permission for commands in CommandInterceptor
 */
public class VisualizationManager {

    // TODO: MOAR logging
    public static final Logger VISUALIZATION_LOGGER = LogManager.getLogger(MOD_ID.toUpperCase() + "-Visualization");

    public static void initServerInstance(MinecraftServer server) {
        serverInstance = server;
    }

    public final static Identifier REGION_BLOCK_DISPLAY_TAG = Identifier.parse("yawp:region_block_display");
    public final static Identifier REGION_TEXT_DISPLAY_TAG = Identifier.parse("yawp:region_text_display");

    public static int nukeDisplayEntities(ServerLevel level) {
        var entities = level.getEntities(EntityTypeTest.forClass(Display.class), (entity) -> {
            boolean containsTextTag = entity.entityTags().contains(REGION_TEXT_DISPLAY_TAG.toString());
            boolean containsBlockTag = entity.entityTags().contains(REGION_BLOCK_DISPLAY_TAG.toString());
            return containsTextTag || containsBlockTag;
        });
        var entityList = new ArrayList<>(entities);
        var entityAmount = entityList.size();
        entityList.forEach(e -> e.remove(Entity.RemovalReason.DISCARDED));
        if (entityAmount > 0) {
            VISUALIZATION_LOGGER.info("Nuked all ({}) untracked region display entities in level {}.", entityAmount, level.dimension().identifier());
        }
        return entityAmount;
    }

    private static MinecraftServer serverInstance;
    // level key -> VisualizationManager
    private static final Map<Identifier, VisualizationManager> dimVisualizationManagers = new HashMap<>();
    // region name -> RegionVisualizationManager
    private final Map<String, RegionVisualizationManager> regionDisplayManagers;

    private VisualizationManager() {
        this.regionDisplayManagers = new HashMap<>();
    }

    public static void hideRegionsAround(Player player, int radius) {
        Level level = player.level();
        BlockPos playerPos = player.blockPosition();
        Optional<IDimensionRegionApi> maybeApi = RegionManager.get().getDimRegionApi(level.dimension());
        if (maybeApi.isPresent()) {
            var dimApi = maybeApi.get();
            List<IMarkableRegion> regionsAround = dimApi.getRegionsAround(playerPos, radius);
            regionsAround.forEach(region -> {
                hide(region, DisplayType.FRAME);
                hide(region, DisplayType.HULL);
                hide(region, DisplayType.MINIMAL);
                hide(region, DisplayType.MARKED);
            });
        }
    }

    public static void showRegionsAround(Player player, int radius, DisplayType displayType) {
        Level level = player.level();
        BlockPos playerPos = player.blockPosition();
        Optional<IDimensionRegionApi> maybeApi = RegionManager.get().getDimRegionApi(level.dimension());
        if (maybeApi.isPresent()) {
            var dimApi = maybeApi.get();
            List<IMarkableRegion> regionsAround = dimApi.getRegionsAround(playerPos, radius);
            regionsAround.forEach(region -> {
                show(region, displayType);
            });
        }
    }

    public static void hideAllRegions(Level level, boolean untracked) {
        Optional<IDimensionRegionApi> maybeApi = RegionManager.get().getDimRegionApi(level.dimension());
        if (maybeApi.isPresent()) {
            var dimApi = maybeApi.get();
            Collection<IMarkableRegion> regions = dimApi.getAllLocalRegions();
            regions.forEach(region -> {
                hide(region, DisplayType.FRAME);
                hide(region, DisplayType.HULL);
                hide(region, DisplayType.MINIMAL);
                hide(region, DisplayType.MARKED);
            });

        }
        if (untracked) {
            nukeDisplayEntities((ServerLevel) level);
        }
    }

    public static void showTpAnchor(IMarkableRegion region, TeleportAnchor tpAnchor) {
        Identifier levelRl = region.getDim().identifier();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);

        ServerLevel level = serverInstance.getLevel(region.getDim());
        rvm.showTpAnchor(tpAnchor, level);
    }

    public static void hideTpAnchor(IMarkableRegion region, TeleportAnchor tpAnchor) {
        Identifier levelRl = region.getDim().identifier();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);

        ServerLevel level = serverInstance.getLevel(region.getDim());
        rvm.hideTpAnchor(tpAnchor, level);
    }

    public static void updateTpAnchor(IMarkableRegion region, TeleportAnchor tpAnchor) {
        Identifier levelRl = region.getDim().identifier();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);

        rvm.updateTpAnchor(tpAnchor);
    }

    // Note: Not useful, since tp anchor don't have a text component yet to distinguish them
    public static void showTeleportAnchors(IMarkableRegion region) {
        region.getTpAnchors().getAnchors().forEach(
                anchor -> showTpAnchor(region, anchor)
        );
    }

    public static void hideTeleportAnchors(IMarkableRegion region) {
        region.getTpAnchors().getAnchors().forEach(
                anchor -> hideTpAnchor(region, anchor)
        );
    }

    public static void show(IMarkableRegion region, DisplayType displayType, BlockDisplayProperties displayProperties) {
        Identifier levelRl = region.getDim().identifier();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);

        ServerLevel level = serverInstance.getLevel(region.getDim());
        rvm.show(displayType, displayProperties, level);
    }

    private static VisualizationManager getOrCreateVisualizationManager(Identifier levelRl) {
        if (!dimVisualizationManagers.containsKey(levelRl)) {
            VisualizationManager dimVm = new VisualizationManager();
            dimVisualizationManagers.put(levelRl, dimVm);
        }
        return dimVisualizationManagers.get(levelRl);
    }

    private static RegionVisualizationManager getOrCreateRegionVisualizationManager(VisualizationManager vm, IMarkableRegion region) {
        if (!vm.regionDisplayManagers.containsKey(region.getName())) {
            vm.regionDisplayManagers.put(region.getName(), new RegionVisualizationManager(region));
        }
        return vm.regionDisplayManagers.get(region.getName());
    }

    public static void show(IMarkableRegion region, DisplayType displayType) {
        show(region, displayType, region.getArea().getDisplay());
    }

    public static void hide(IMarkableRegion region, DisplayType displayType) {
        Identifier levelRl = region.getDim().identifier();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);
        rvm.hide(displayType);
    }

    public static void hide(IMarkableRegion region) {
        ResourceLocation levelRl = region.getDim().location();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);
        rvm.hide(DisplayType.HULL);
        rvm.hide(DisplayType.FRAME);
        rvm.hide(DisplayType.MARKED);
        rvm.hide(DisplayType.MINIMAL);
    }

    public static void updateRegionDisplay(IMarkableRegion region) {
        Identifier levelRl = region.getDim().identifier();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);
        ServerLevel level = serverInstance.getLevel(region.getDim());
        rvm.updateDisplay(region.getArea(), level);
    }


    public static void refreshDisplay(IMarkableRegion region, DisplayType displayType) {
        Identifier levelRl = region.getDim().identifier();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);

        rvm.updateDisplay(region.getArea().getDisplay(), displayType);
    }

    public static void refreshDisplay(IMarkableRegion region) {
        Identifier levelRl = region.getDim().identifier();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);

        rvm.updateDisplay(region.getArea().getDisplay(), true);
    }

    public static void showHierarchy(IMarkableRegion region, DisplayType displayType, boolean recursive) {
        Optional<IDimensionRegionApi> maybeApi = RegionManager.get().getDimRegionApi(region.getDim());
        if (maybeApi.isEmpty()) return;
        Collection<IProtectedRegion> children = region.getChildren().values();
        for (IProtectedRegion child : children) {
            if (child instanceof IMarkableRegion childRegion) {
                show(childRegion, displayType);
                if (recursive) {
                    showHierarchy(childRegion, displayType, recursive);
                }
            }
        }
    }

    public static void showIntersecting(IMarkableRegion region, DisplayType displayType) {
        Optional<IDimensionRegionApi> maybeApi = RegionManager.get().getDimRegionApi(region.getDim());
        if (maybeApi.isPresent()) {
            var dimApi = maybeApi.get();
            List<IMarkableRegion> intersectingRegions = dimApi.getIntersectingRegions(region);
            intersectingRegions.forEach(intersectingRegion -> {
                show(intersectingRegion, displayType);
            });
        }
    }

    public static void hideHierarchy(IMarkableRegion region, DisplayType displayType, boolean recursive) {
        Optional<IDimensionRegionApi> maybeApi = RegionManager.get().getDimRegionApi(region.getDim());
        if (maybeApi.isEmpty()) return;
        Collection<IProtectedRegion> children = region.getChildren().values();
        for (IProtectedRegion child : children) {
            if (child instanceof IMarkableRegion childRegion) {
                hide(childRegion, displayType);
                if (recursive) {
                    hideHierarchy(childRegion, displayType, recursive);
                }
            }
        }
    }

    public static void hideIntersecting(IMarkableRegion region, DisplayType displayType) {
        Optional<IDimensionRegionApi> maybeApi = RegionManager.get().getDimRegionApi(region.getDim());
        if (maybeApi.isPresent()) {
            var dimApi = maybeApi.get();
            List<IMarkableRegion> intersectingRegions = dimApi.getIntersectingRegions(region);
            intersectingRegions.forEach(intersectingRegion -> {
                hide(intersectingRegion, displayType);
            });
        }
    }
}
