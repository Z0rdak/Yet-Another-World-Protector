package de.z0rdak.yawp.api.visualization;

import de.z0rdak.yawp.constants.Constants;
import de.z0rdak.yawp.core.area.BlockDisplayProperties;
import de.z0rdak.yawp.core.area.DisplayType;
import de.z0rdak.yawp.core.area.TeleportAnchor;
import de.z0rdak.yawp.core.area.TextDisplayProperties;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;

import java.util.*;

import static de.z0rdak.yawp.api.visualization.VisualizationUtil.buildTeleportAnchorTextDisplayTag;
import static de.z0rdak.yawp.api.visualization.VisualizationUtil.createDisplayEntity;

public class VisualizationManager {

    public static void initServerInstance(MinecraftServer server) {
        serverInstance = server;
    }

    private static MinecraftServer serverInstance;
    // level key -> VisualizationManager
    private static final Map<ResourceLocation, VisualizationManager> dimVisualizationManagers = new HashMap<>();
    // region name -> RegionVisualizationManager

    private final Map<String, RegionVisualizationManager> regionDisplayManagers;

    private VisualizationManager() {
        this.regionDisplayManagers = new HashMap<>();
    }

    public static void show(IMarkableRegion region, TeleportAnchor tpAnchor, TextDisplayProperties textDisplayProperties) {}

    public static void show(IMarkableRegion region, TeleportAnchor tpAnchor) {
        // TODO: Just go with a default TextDisplayProperties for now
        var defaultProperties = new TextDisplayProperties(tpAnchor.getName());
        CompoundTag compoundTag = buildTeleportAnchorTextDisplayTag(region.getName(), defaultProperties);
        Optional<Entity> displayEntity = createDisplayEntity(serverInstance.getLevel(region.getDim()), tpAnchor.getPos(), compoundTag);
        if (displayEntity.isPresent()) {

        }
        show(region, tpAnchor, defaultProperties);
    }

    public static void hide(IMarkableRegion region, TeleportAnchor tpAnchor) {
        // TODO:
    }

    public static void show(IMarkableRegion region, DisplayType displayType, BlockDisplayProperties displayProperties) {
        ResourceLocation levelRl = region.getDim().location();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);

        ServerLevel level = serverInstance.getLevel(region.getDim());
        rvm.show(displayType, displayProperties, level);
    }

    private static VisualizationManager getOrCreateVisualizationManager(ResourceLocation levelRl) {
        if (!dimVisualizationManagers.containsKey(levelRl)) {
            VisualizationManager dimVm = new VisualizationManager();
            dimVisualizationManagers.put(levelRl, dimVm);
            Constants.LOGGER.info("Init VM for {}", levelRl.toString());
        }
        return dimVisualizationManagers.get(levelRl);
    }

    private static RegionVisualizationManager getOrCreateRegionVisualizationManager(VisualizationManager vm, IMarkableRegion region) {
        if (!vm.regionDisplayManagers.containsKey(region.getName())) {
            vm.regionDisplayManagers.put(region.getName(), new RegionVisualizationManager(region));
            Constants.LOGGER.info("Init RVM for {}", region.getName());
        }
        return vm.regionDisplayManagers.get(region.getName());
    }

    public static void show(IMarkableRegion region, DisplayType displayType) {
        show(region, displayType, region.getArea().getDisplay());
    }

    public static void hide(IMarkableRegion region, DisplayType displayType) {
        ResourceLocation levelRl = region.getDim().location();
        VisualizationManager vm = getOrCreateVisualizationManager(levelRl);
        RegionVisualizationManager rvm = getOrCreateRegionVisualizationManager(vm, region);

        rvm.hide(displayType);
    }


    public static void update(IMarkableRegion region) {

    }

    private static Set<BlockPos> getDisplayPositions(IMarkableRegion region, DisplayType displayType) {
        return switch (displayType) {
            case FRAME -> region.getArea().getFrame();
            case HULL -> region.getArea().getHull();
            case MARKED -> new HashSet<>(region.getArea().markedBlocks());
            case MINIMAL -> new HashSet<>(region.getArea().getMinimalOutline());
        };
    }

    public static void showHierarchy(IMarkableRegion region, boolean recursive) {

    }

    public static void showIntersecting(IMarkableRegion region) {

    }

    public static void hideHierarchy(IMarkableRegion region, boolean recursive) {

    }

    public static void hideIntersecting(IMarkableRegion region) {

    }

    public static void showTeleportAnchors(IMarkableRegion region) {
        region.getTpAnchors().getAnchors().forEach(
                anchor -> show(region, anchor)
        );
    }

    public static void hideTeleportAnchors(IMarkableRegion region) {
        region.getTpAnchors().getAnchors().forEach(
                anchor -> hide(region, anchor)
        );
    }


    /*
    TODO:
    - Track entities for region
    - Event for filtering tracking for players?
    -> when tracking other player is added?... kill entities and add tracking for other player or how doesi t work?

    - Show area for region
    - Show area for region and children
    - Show areas at blockpos
    - Show area and intersecting
- Show teleport anchors
- Remove non-persistent entities after unload/restart...



     */



}
