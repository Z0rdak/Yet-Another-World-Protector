package de.z0rdak.yawp.api.visualization;

import de.z0rdak.yawp.core.area.*;
import de.z0rdak.yawp.core.region.IMarkableRegion;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static de.z0rdak.yawp.api.visualization.VisualizationUtil.*;

public class RegionVisualizationManager {

    private final IMarkableRegion region;
    private final RegionVisualization hull;
    private final RegionVisualization frame;
    private final RegionVisualization minimalOutline;
    private final RegionVisualization marked;
    // TODO:
    private final Map<String, TpAnchorVisualization> tpAnchorVisualizations;

    public RegionVisualizationManager(IMarkableRegion region) {
        this.region = region;
        var blockDisplayProperties = region.getArea().getDisplay();
        this.hull = new RegionVisualization(blockDisplayProperties);
        this.frame = new RegionVisualization(blockDisplayProperties);
        this.minimalOutline = new RegionVisualization(blockDisplayProperties);
        this.marked = new RegionVisualization(blockDisplayProperties);
        this.tpAnchorVisualizations = new HashMap<>();
        region.getTpAnchors().getAnchors().forEach(anchor -> {
            TextDisplayProperties textDisplayProperties = new TextDisplayProperties(anchor.getName());
            TpAnchorVisualization tpAnchorVisualization = new TpAnchorVisualization(anchor, blockDisplayProperties, textDisplayProperties);
            this.tpAnchorVisualizations.put(anchor.getName(), tpAnchorVisualization);
        });
    }

    public void showTpAnchor(TeleportAnchor tpAnchor, ServerLevel level, BlockDisplayProperties displayProperties, TextDisplayProperties textDisplayProperties) {
        if (!tpAnchorVisualizations.containsKey(tpAnchor.getName())) {
            var tpAnchorVisualization = new TpAnchorVisualization(tpAnchor, displayProperties, textDisplayProperties);
            tpAnchorVisualizations.put(tpAnchor.getName(), tpAnchorVisualization);
        }
        TpAnchorVisualization tpVis = tpAnchorVisualizations.get(tpAnchor.getName());
        var maybeEntity = createBlockDisplayEntity(level, region.getName(), tpAnchor.getPos(), displayProperties);
        if (maybeEntity.isPresent()) {
            var entity = maybeEntity.get();
            entity.addTag(VisualizationManager.REGION_BLOCK_DISPLAY_TAG.toString());
            tpVis.trackTpAnchorBlockDisplay(entity);
            level.addFreshEntity(entity);
        }
    }

    public void showTpAnchor(TeleportAnchor tpAnchor, ServerLevel level) {
        var blockPorps = new BlockDisplayProperties(ResourceLocation.parse("minecraft:cyan_stained_glass_pane"), true, 15);
        var textProps = new TextDisplayProperties(tpAnchor.getName());
            showTpAnchor(tpAnchor, level, blockPorps, textProps);
    }

    public void hideTpAnchor(TeleportAnchor tpAnchor, ServerLevel level) {
        if (tpAnchorVisualizations.containsKey(tpAnchor.getName())) {
            TpAnchorVisualization tpVis = tpAnchorVisualizations.get(tpAnchor.getName());
            tpVis.discardTpAnchorDisplay();
        }
    }

    public void updateTpAnchor(TeleportAnchor anchor) {
        if (tpAnchorVisualizations.containsKey(anchor.getName())) {
            TpAnchorVisualization tpVis = tpAnchorVisualizations.get(anchor.getName());
            tpVis.updateBlockPosition(anchor);
            // tpVis.updateText(anchor); // TODO:
        }
    }

    public Set<BlockPos> blocksForDisplayType(DisplayType type) {
        return switch (type) {
            case FRAME -> region.getArea().getFrame();
            case HULL -> region.getArea().getHull();
            case MINIMAL -> region.getArea().getMinimalOutline();
            case MARKED -> region.getArea().markedBlocks();
        };
    }

    public void show(DisplayType displayType, BlockDisplayProperties displayProperties, ServerLevel level) {
        Set<BlockPos> blocks = blocksForDisplayType(displayType);
        blocks.stream()
                .filter(pos -> ! switch (displayType) {
                    case FRAME -> this.frame.doesTrackEntityAt(pos);
                    case HULL -> this.hull.doesTrackEntityAt(pos);
                    case MINIMAL -> this.minimalOutline.doesTrackEntityAt(pos);
                    case MARKED -> this.marked.doesTrackEntityAt(pos);
                })
                .forEach(pos -> {
                    var maybeEntity = createBlockDisplayEntity(level, region.getName(), pos, displayProperties);
                    if (maybeEntity.isPresent()) {
                        var entity = maybeEntity.get();
                        entity.addTag(VisualizationManager.REGION_BLOCK_DISPLAY_TAG.toString());
                        switch (displayType) {
                            case FRAME -> this.frame.trackBlockDisplay(pos, entity);
                            case HULL -> this.hull.trackBlockDisplay(pos, entity);
                            case MINIMAL -> this.minimalOutline.trackBlockDisplay(pos, entity);
                            case MARKED -> this.marked.trackBlockDisplay(pos, entity);
                        }
                        level.addFreshEntity(entity);
                    }
                });
    }

    public void show(DisplayType displayType, ServerLevel level) {
        show(displayType, region.getArea().getDisplay(), level);
    }

    public void hide(DisplayType displayType) {
        switch (displayType) {
            case FRAME -> this.frame.discardEntities();
            case HULL -> this.hull.discardEntities();
            case MINIMAL -> this.minimalOutline.discardEntities();
            case MARKED -> this.marked.discardEntities();
        }
    }

    public void updateDisplay(IMarkableArea area, ServerLevel level) {
        this.region.setArea(area); // just to be sure
        if (this.frame.hasEntitiesTracked()) {
            this.frame.discardEntities();
            this.show(DisplayType.FRAME, this.frame.getProperties(), level);
        }
        if (this.hull.hasEntitiesTracked()) {
            this.hull.discardEntities();
            this.show(DisplayType.HULL, this.hull.getProperties(), level);
        }
        if (this.minimalOutline.hasEntitiesTracked()) {
            this.minimalOutline.discardEntities();
            this.show(DisplayType.MINIMAL, this.minimalOutline.getProperties(), level);
        }
        if (this.marked.hasEntitiesTracked()) {
            this.marked.discardEntities();
            this.show(DisplayType.MARKED, this.marked.getProperties(), level);
        }
    }

    public void updateDisplay(BlockDisplayProperties displayProperties, DisplayType displayType) {
        this.frame.updateDisplay(displayProperties, false);
        this.hull.updateDisplay(displayProperties, false);
        this.minimalOutline.updateDisplay(displayProperties, false);
        this.marked.updateDisplay(displayProperties, false);
        switch (displayType) {
            case FRAME -> this.frame.updateDisplay(displayProperties, true);
            case HULL -> this.hull.updateDisplay(displayProperties, true);
            case MINIMAL -> this.minimalOutline.updateDisplay(displayProperties, true);
            case MARKED -> this.marked.updateDisplay(displayProperties, true);
        }
    }

    public void updateDisplay(BlockDisplayProperties displayProperties, boolean refresh) {
        this.frame.updateDisplay(displayProperties, refresh);
        this.hull.updateDisplay(displayProperties, refresh);
        this.minimalOutline.updateDisplay(displayProperties, refresh);
        this.marked.updateDisplay(displayProperties, refresh);
    }
}
